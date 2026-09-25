# Modelo: consulta de la referencia administrativa (departamento/estado/provincia) usada en el
# formulario de verificación taxonómica. Es un dato público (nombres de divisiones político-
# administrativas, no de ocurrencias) generado una vez por scripts/01b_build_admin1.R.

#' Departamentos/estados/provincias disponibles para un país
#'
#' @param admin1 `data.frame` con columnas `countryCode` y `department` (o `NULL` si no se pudo
#'   cargar la referencia).
#' @param country_code Código ISO-2 del país del registro (puede ser `NA`).
#' @return Vector de nombres ordenado; vacío si no hay país o no hay datos para ese país.
departments_for_country <- function(admin1, country_code) {
  if (is.null(admin1) || is.null(country_code) || is.na(country_code) || !nzchar(country_code)) {
    return(character(0))
  }
  sort(unique(admin1$department[admin1$countryCode == country_code]))
}
