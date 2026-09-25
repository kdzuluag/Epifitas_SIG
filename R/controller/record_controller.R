# Controlador de selección de registro y de verificación de ubicación (validación, límite de
# tasa, auditoría).

#' Controlador del registro seleccionado (mapa o tabla)
#'
#' Solo acepta claves de registros que existen: así una entrada manipulada no llega al
#' resto de la aplicación.
#'
#' @param repo Repositorio ([load_repository()]).
#' @return Lista con `key` (reactivo), `select(k)` (función) y `record` (reactivo con la fila).
selection_controller <- function(repo) {
  key <- reactiveVal(NULL)
  is_valid_key <- function(k) {
    !is.null(k) && length(k) == 1 && !is.na(k) && k %in% repo$occ$record_key
  }
  list(
    key = key,
    select = function(k) {
      if (is_valid_key(k)) key(as.character(k))
    },
    record = reactive({
      k <- key()
      if (is.null(k)) NULL else repo$occ[match(k, repo$occ$record_key), , drop = FALSE]
    })
  )
}

#' Controlador de verificación de ubicación
#'
#' El taxónomo ya no deja comentarios libres: confirma si el punto está bien ubicado y, si no,
#' corrige el departamento/municipio o la región neotrópica. Ver [build_verdict()] (Modelo) para
#' la validación y `R/view/mod_detail.R` para el formulario.
#'
#' @param cfg Configuración.
#' @param repo Repositorio ([load_repository()]; aporta `admin1` y `regions`).
#' @param auth Resultado de [auth_controller()].
#' @param selected_key Reactivo con la clave del registro seleccionado.
#' @param notify Función para avisar al usuario (inyectable para pruebas).
#' @return Lista con `can_verify`, `departments_for(country_code)`, `regions`, `count(key)`,
#'   `counts` y `submit(payload)`.
verification_controller <- function(cfg, repo, auth, selected_key,
                                    notify = shiny::showNotification) {
  version <- reactiveVal(0L)
  limiter <- make_rate_limiter(cfg$verification_rate_max, cfg$verification_rate_window)
  can_verify <- reactive(auth$role %in% c("reviewer", "admin"))

  submit <- function(payload) {
    key <- selected_key()
    if (is.null(key) || !can_verify()) {
      return(FALSE)
    }
    built <- build_verdict(payload, cfg)
    if (!built$ok) {
      notify(built$error, type = "warning")
      return(FALSE)
    }
    if (!limiter()) {
      notify("Demasiadas verificaciones en poco tiempo. Intente de nuevo en un minuto.",
        type = "warning"
      )
      audit_log(cfg, "verificacion_limitada", auth$user, list(registro = key))
      return(FALSE)
    }
    vs_add(cfg$verification_db, key, auth$user, built$verdict)
    audit_log(cfg, "verificacion", auth$user, list(
      registro = key, bien_ubicado = built$verdict$well_placed,
      tipo = built$verdict$issue_type %||% NA_character_
    ))
    version(version() + 1L)
    notify("Verificación registrada. Gracias.", type = "message", duration = 3)
    TRUE
  }

  list(
    can_verify = can_verify,
    departments_for = function(country_code) departments_for_country(repo$admin1, country_code),
    regions = repo$regions,
    count = function(key) {
      version()
      vs_count_one(cfg$verification_db, key)
    },
    counts = reactive({
      version()
      vs_counts(cfg$verification_db)
    }),
    submit = submit
  )
}
