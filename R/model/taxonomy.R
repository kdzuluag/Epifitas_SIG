# Filtrado taxonómico/geográfico/ecológico. Toda entrada del usuario pasa por sanitize_filters().

#' Convierte el texto libre de búsqueda en términos normalizados
#'
#' @param x Texto escrito por el usuario.
#' @param cfg Configuración ([epig_config()]): límites de longitud y de términos.
#' @return Vector de términos en minúsculas sin tildes (máx. `cfg$max_search_tokens`).
sanitize_search <- function(x, cfg) {
  x <- as.character(x %||% "")[1]
  x <- substr(x, 1, cfg$max_search_chars)
  toks <- strsplit(norm_text(gsub("[[:cntrl:]]", " ", x)), "\\s+")[[1]]
  toks <- toks[nzchar(toks)]
  head(toks, cfg$max_search_tokens)
}

#' Filtros por defecto (sin ninguna restricción)
#'
#' @param occ Registros procesados.
#' @return Lista de filtros.
default_filters <- function(occ) {
  yrs <- suppressWarnings(range(occ$year, na.rm = TRUE))
  list(
    level = "family", values = character(0), text = "", elev_bands = character(0),
    countries = character(0), years = if (all(is.finite(yrs))) yrs else NULL,
    only_georef = FALSE
  )
}

#' Sanea y valida los filtros recibidos desde la interfaz
#'
#' Descarta valores fuera de catálogo, limita tamaños y trata el rango completo de
#' años como "sin filtro".
#'
#' @param f Lista de filtros sin confiar.
#' @param occ Registros procesados.
#' @param cfg Configuración.
#' @return Lista de filtros seguros.
sanitize_filters <- function(f, occ, cfg) {
  level <- if (isTRUE(f$level %in% TAX_LEVELS)) f$level else "family"
  vals <- as.character(f$values %||% character(0))
  vals <- head(vals[!is.na(vals)], cfg$max_filter_values)
  yrs <- suppressWarnings(as.integer(f$years))
  yrs <- if (length(yrs) == 2 && !anyNA(yrs)) sort(yrs) else NULL
  full <- suppressWarnings(range(occ$year, na.rm = TRUE))
  if (!is.null(yrs) && all(yrs == full)) yrs <- NULL
  list(
    level = level,
    values = vals,
    tokens = sanitize_search(f$text, cfg),
    elev_bands = intersect(as.character(f$elev_bands), ELEV_LABELS),
    countries = intersect(toupper(as.character(f$countries)), unique(occ$countryCode)),
    years = yrs,
    only_georef = isTRUE(f$only_georef)
  )
}

#' Aplica filtros ya saneados a los registros
#'
#' @param df Registros procesados.
#' @param f Resultado de [sanitize_filters()].
#' @return Subconjunto de `df`.
filter_occurrences <- function(df, f) {
  keep <- rep(TRUE, nrow(df))
  if (length(f$values)) keep <- keep & (df[[f$level]] %in% f$values)
  for (tk in f$tokens) keep <- keep & grepl(tk, df$search_txt, fixed = TRUE)
  if (length(f$elev_bands)) keep <- keep & (df$elev_band %in% f$elev_bands)
  if (length(f$countries)) keep <- keep & (df$countryCode %in% f$countries)
  if (!is.null(f$years)) {
    rng <- suppressWarnings(range(df$year, na.rm = TRUE))
    if (!all(f$years == rng)) {
      keep <- keep & !is.na(df$year) & df$year >= f$years[1] & df$year <= f$years[2]
    }
  }
  if (isTRUE(f$only_georef)) keep <- keep & df$has_coords
  df[keep, , drop = FALSE]
}

#' Opciones del selector taxonómico con su número de registros
#'
#' @param occ Registros procesados.
#' @param level Columna taxonómica (`order`, `family`, `genus`, `speciesName`).
#' @return Vector con nombres "Taxón (n)" y valores "Taxón", ordenado por frecuencia.
taxon_choices <- function(occ, level) {
  tab <- sort(table(occ[[level]]), decreasing = TRUE)
  stats::setNames(names(tab), sprintf("%s (%s)", names(tab), fmt_int(as.integer(tab))))
}

#' Etiquetas legibles de los filtros activos
#'
#' @param f Resultado de [sanitize_filters()].
#' @return Vector de texto (vacío si no hay filtros).
filter_summary <- function(f) {
  parts <- character(0)
  lvl <- names(TAX_LEVELS)[match(f$level, TAX_LEVELS)]
  n_values <- length(f$values)
  if (n_values) {
    suffix <- if (n_values == 1) "o" else "os"
    parts <- c(parts, sprintf("%s: %d seleccionad%s", lvl, n_values, suffix))
  }
  if (length(f$tokens)) parts <- c(parts, sprintf("Texto: «%s»", paste(f$tokens, collapse = " ")))
  if (length(f$elev_bands)) {
    parts <- c(parts, sprintf("%d piso(s) altitudinal(es)", length(f$elev_bands)))
  }
  if (length(f$countries)) parts <- c(parts, sprintf("%d país(es)", length(f$countries)))
  if (!is.null(f$years)) parts <- c(parts, sprintf("Años %d-%d", f$years[1], f$years[2]))
  if (isTRUE(f$only_georef)) parts <- c(parts, "Solo georreferenciados")
  parts
}
