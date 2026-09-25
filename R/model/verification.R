# Validación y normalización del formulario de verificación de ubicación (pura: sin shiny).
# Vive en el Modelo para poder probarla sin servidor ni base de datos.

#' Valida y normaliza el formulario de verificación de ubicación
#'
#' @param payload Lista cruda tal como llega de la Vista: `well_placed` (lógico),
#'   `reviewer_name`, `reviewer_profession` y, cuando `well_placed` es falso, `issue_type`
#'   (`"ubicacion"` o `"region"`) con sus campos (`department`/`municipality`/`detail`, o
#'   `neotropical_region`).
#' @param cfg Configuración (longitudes máximas).
#' @return Lista con `ok`; si es válida, `verdict` (listo para [vs_add()]); si no, `error`
#'   (mensaje para mostrar al usuario).
build_verdict <- function(payload, cfg) {
  name <- sanitize_text(payload$reviewer_name, cfg$identity_max_chars)
  profession <- sanitize_text(payload$reviewer_profession, cfg$identity_max_chars)
  if (is.null(name) || is.null(profession)) {
    return(list(ok = FALSE, error = "Escriba su nombre y su profesión."))
  }
  verdict <- list(
    well_placed = isTRUE(payload$well_placed), reviewer_name = name,
    reviewer_profession = profession
  )
  if (verdict$well_placed) {
    return(list(ok = TRUE, verdict = verdict))
  }
  if (!isTRUE(payload$issue_type %in% ISSUE_TYPES)) {
    return(list(ok = FALSE, error = "Indique qué está incorrecto."))
  }
  verdict$issue_type <- payload$issue_type
  if (identical(payload$issue_type, "ubicacion")) {
    build_location_verdict(verdict, payload, cfg)
  } else {
    build_region_verdict(verdict, payload, cfg)
  }
}

# Completa el veredicto cuando lo incorrecto es la ubicación geográfica.
build_location_verdict <- function(verdict, payload, cfg) {
  department <- sanitize_text(payload$department, cfg$identity_max_chars)
  municipality <- sanitize_text(payload$municipality, cfg$identity_max_chars)
  if (is.null(department) || is.null(municipality)) {
    return(list(ok = FALSE, error = "Seleccione el departamento y escriba la municipalidad."))
  }
  verdict$department <- department
  verdict$municipality <- municipality
  verdict$detail <- sanitize_text(payload$detail, cfg$detail_max_chars) # detalle: opcional
  list(ok = TRUE, verdict = verdict)
}

# Completa el veredicto cuando lo incorrecto es la región neotrópica.
build_region_verdict <- function(verdict, payload, cfg) {
  region <- sanitize_text(payload$neotropical_region, cfg$identity_max_chars)
  if (is.null(region)) {
    return(list(ok = FALSE, error = "Seleccione la región del Neotrópico."))
  }
  verdict$neotropical_region <- region
  list(ok = TRUE, verdict = verdict)
}
