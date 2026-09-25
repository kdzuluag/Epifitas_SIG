#' Operador "o si vacío": devuelve `b` cuando `a` es NULL, de longitud 0 o NA escalar
#'
#' @param a Valor preferido.
#' @param b Valor de respaldo.
#' @return `a` o `b`.
`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a
}

#' Formatea enteros con separador de miles en estilo es-CO (1.234)
#'
#' @param x Vector numérico.
#' @return Vector de texto.
fmt_int <- function(x) {
  format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE)
}

#' Formatea proporciones como porcentaje (12,3 %)
#'
#' @param x Proporciones entre 0 y 1.
#' @param digits Decimales.
#' @return Vector de texto.
fmt_pct <- function(x, digits = 1) {
  pct <- format(round(100 * x, digits), decimal.mark = ",", nsmall = digits, trim = TRUE)
  paste0(pct, " %")
}

#' Normaliza texto para búsqueda: minúsculas y sin tildes
#'
#' @param x Vector de texto.
#' @return Vector de texto en minúsculas ASCII.
norm_text <- function(x) {
  stringi::stri_trans_general(tolower(as.character(x)), "Latin-ASCII")
}

#' Año calendario actual
#'
#' @return Entero.
current_year <- function() as.integer(format(Sys.Date(), "%Y"))
