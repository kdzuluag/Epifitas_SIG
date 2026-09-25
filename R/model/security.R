# Ciberseguridad: autenticación por capas, saneamiento, límite de tasa y bitácora de auditoría.
# Modelo de amenazas resumido en docs/arquitectura.qmd. Defensa en profundidad:
#   1) la app rechaza sesiones sin identidad (aunque el hosting esté mal configurado como público)
#   2) los datos desplegados no contienen coordenadas exactas
#   3) toda entrada se sanea/valida; la salida se escapa; SQL siempre parametrizado
#   4) límite de tasa en escritura y bitácora de auditoría sin datos sensibles

# Caracteres de control ASCII (salvo salto de línea y tabulación) que se eliminan de la entrada.
CONTROL_CHARS <- "[\\x00-\\x08\\x0B\\x0C\\x0E-\\x1F\\x7F]"

#' Sanea un texto escrito por el usuario
#'
#' Elimina caracteres de control, recorta espacios y limita la longitud. El texto NO se
#' escapa aquí: se escapa al mostrarlo.
#'
#' @param x Texto de entrada.
#' @param max_chars Longitud máxima.
#' @return Texto saneado, o `NULL` si queda vacío.
sanitize_text <- function(x, max_chars = 1000) {
  x <- as.character(x %||% "")[1]
  x <- enc2utf8(x)
  x <- gsub(CONTROL_CHARS, "", x, perl = TRUE)
  x <- substr(trimws(x), 1, max_chars)
  if (nzchar(x)) x else NULL
}

#' Crea un limitador de tasa de ventana deslizante
#'
#' @param max_events Eventos permitidos por ventana.
#' @param window_sec Duración de la ventana en segundos.
#' @param clock Función que devuelve la hora en segundos (inyectable para pruebas).
#' @return Función sin argumentos: `TRUE` si el evento se permite (y se registra), `FALSE` si
#'   se excede el límite.
make_rate_limiter <- function(max_events, window_sec, clock = function() as.numeric(Sys.time())) {
  state <- new.env(parent = emptyenv())
  state$stamps <- numeric(0)
  function() {
    now <- clock()
    state$stamps <- state$stamps[now - state$stamps < window_sec]
    if (length(state$stamps) >= max_events) {
      return(FALSE)
    }
    state$stamps <- c(state$stamps, now)
    TRUE
  }
}

#' Registra un evento en la bitácora de auditoría (una línea JSON por evento)
#'
#' Un fallo al escribir NO se silencia: se avisa por la consola/log del servidor.
#'
#' @param cfg Configuración (usa `cfg$audit_log`).
#' @param event Nombre del evento.
#' @param user Identidad del usuario.
#' @param detail Lista con detalle sin datos sensibles.
#' @return El registro escrito, de forma invisible.
audit_log <- function(cfg, event, user = NA_character_, detail = list()) {
  rec <- list(
    ts = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), event = event,
    user = as.character(user), detail = detail
  )
  tryCatch(
    {
      dir.create(dirname(cfg$audit_log), recursive = TRUE, showWarnings = FALSE)
      line <- jsonlite::toJSON(rec, auto_unbox = TRUE, null = "null")
      # la advertencia previa de file() se descarta: el error real se avisa en el manejador
      suppressWarnings(cat(line, "\n", file = cfg$audit_log, append = TRUE, sep = ""))
    },
    error = function(e) {
      message("[EpIG] No se pudo escribir la bitácora de auditoría: ", conditionMessage(e))
    }
  )
  invisible(rec)
}

# Resultados posibles de la resolución de identidad.
deny_access <- function(reason) {
  list(ok = FALSE, user = NA_character_, role = "none", reason = reason)
}
# Resultado de acceso concedido: identidad y rol.
allow_access <- function(user, role) {
  list(ok = TRUE, user = user, role = role, reason = NA_character_)
}

# Identidad en Posit Connect: `session$user` y grupos; sin identidad se deniega.
resolve_connect_user <- function(session, cfg) {
  user <- session$user
  if (is.null(user) || !nzchar(user)) {
    return(deny_access("sin_identidad"))
  }
  groups <- as.character(session$groups %||% character(0))
  if (length(cfg$allowed_groups) && !any(groups %in% cfg$allowed_groups)) {
    return(deny_access("grupo_no_permitido"))
  }
  is_admin <- any(groups %in% cfg$admin_groups) || user %in% cfg$admin_users
  allow_access(substr(user, 1, 100), if (is_admin) "admin" else "reviewer")
}

# Identidad tras un proxy: cabecera de usuario + token compartido con el proxy.
resolve_proxy_user <- function(session, cfg) {
  user <- session$request[[cfg$proxy_user_header]]
  token <- session$request[[cfg$proxy_token_header]]
  if (is.null(user) || !nzchar(user)) {
    return(deny_access("sin_identidad"))
  }
  same_token <- !is.null(token) &&
    identical(digest::digest(as.character(token)), digest::digest(cfg$proxy_token))
  if (!same_token) {
    return(deny_access("token_proxy_invalido"))
  }
  allow_access(substr(user, 1, 100), if (user %in% cfg$admin_users) "admin" else "reviewer")
}

#' Determina la identidad y el rol de una sesión según el modo de autenticación
#'
#' @param session Sesión de Shiny (o una lista equivalente en pruebas).
#' @param cfg Configuración ([epig_config()]).
#' @return Lista con `ok`, `user`, `role` (`reviewer`, `admin` o `none`) y `reason`.
resolve_user <- function(session, cfg) {
  switch(cfg$auth_mode,
    none = allow_access("dev", "reviewer"),
    connect = resolve_connect_user(session, cfg),
    proxy = resolve_proxy_user(session, cfg),
    deny_access("modo_desconocido")
  )
}
