# Estadística aplicada y ciencia de datos sobre registros de ocurrencia.
# Advertencia de dominio: un registro de colecta NO es un individuo; los estimadores basados en
# registros (Chao1, curva de acumulación) son indicativos del esfuerzo de muestreo, no censos.

#' Indicadores generales de un conjunto de registros
#'
#' @param df Registros procesados.
#' @return Lista con `n_records`, `n_species`, `n_localities`, `n_no_coords`, `pct_georef`.
summarise_overview <- function(df) {
  list(
    n_records = nrow(df),
    n_species = dplyr::n_distinct(df$speciesName, na.rm = TRUE),
    n_localities = dplyr::n_distinct(df$locality, na.rm = TRUE),
    n_no_coords = sum(!df$has_coords),
    pct_georef = if (nrow(df)) mean(df$has_coords) else NA_real_
  )
}

#' Cuenta registros o especies por taxón de un nivel
#'
#' @param df Registros procesados.
#' @param level Columna taxonómica.
#' @param metric `"records"` o `"species"`. En el nivel de especie, "species" no aporta
#'   información y se cambia a registros (ver atributo `metric`).
#' @param top_n Número máximo de taxones a devolver.
#' @return `data.frame` con `taxon` y `n`; atributo `metric` con la métrica realmente usada.
count_by_level <- function(df, level, metric = c("records", "species"), top_n = 15) {
  metric <- match.arg(metric)
  effective <- if (metric == "species" && level == "speciesName") "records" else metric
  d <- df[!is.na(df[[level]]), , drop = FALSE]
  if (!nrow(d)) {
    empty <- data.frame(taxon = character(0), n = integer(0))
    return(structure(empty, metric = effective))
  }
  g <- dplyr::group_by(d, taxon = .data[[level]])
  res <- if (effective == "species") {
    dplyr::summarise(g, n = dplyr::n_distinct(speciesName, na.rm = TRUE), .groups = "drop")
  } else {
    dplyr::summarise(g, n = dplyr::n(), .groups = "drop")
  }
  res <- utils::head(dplyr::arrange(res, dplyr::desc(n), taxon), top_n)
  structure(as.data.frame(res), metric = effective)
}

#' Estimador de riqueza Chao1 con corrección de sesgo (Chao 1987)
#'
#' @param species Vector con el nombre de especie de cada registro.
#' @return Lista con `observed`, `estimated`, `f1`, `f2`, `completeness` y `n_records`.
chao1 <- function(species) {
  tab <- table(species[!is.na(species)])
  if (!length(tab)) {
    return(list(
      observed = 0L, estimated = NA_real_, f1 = 0L, f2 = 0L,
      completeness = NA_real_, n_records = 0L
    ))
  }
  s_obs <- length(tab)
  f1 <- sum(tab == 1)
  f2 <- sum(tab == 2)
  est <- s_obs + f1 * (f1 - 1) / (2 * (f2 + 1))
  list(
    observed = s_obs, estimated = est, f1 = f1, f2 = f2,
    completeness = s_obs / est, n_records = sum(tab)
  )
}

#' Curva de acumulación de especies por permutación
#'
#' @param species Vector con la especie de cada registro.
#' @param n_perm Número de permutaciones del orden de los registros.
#' @param n_points Máximo de puntos de la curva devueltos.
#' @param seed Semilla (la curva es reproducible).
#' @return `data.frame` con `records`, `mean`, `lo` (percentil 5) y `hi` (percentil 95);
#'   `NULL` si hay menos de dos registros.
species_accumulation <- function(species, n_perm = 30, n_points = 150, seed = 1L) {
  species <- species[!is.na(species)]
  n <- length(species)
  if (n < 2) {
    return(NULL)
  }
  pts <- unique(round(seq(1, n, length.out = min(n_points, n))))
  one_perm <- function(i) cumsum(!duplicated(sample(species)))[pts]
  m <- withr::with_seed(seed, vapply(seq_len(n_perm), one_perm, numeric(length(pts))))
  q <- apply(m, 1, stats::quantile, probs = c(0.05, 0.95))
  data.frame(records = pts, mean = rowMeans(m), lo = q[1, ], hi = q[2, ])
}

#' Registros por piso altitudinal
#'
#' @param df Registros procesados.
#' @return `data.frame` con `band` (factor con todos los pisos) y `n`.
by_elevation <- function(df) {
  d <- df[!is.na(df$elev_band), , drop = FALSE]
  res <- data.frame(band = factor(ELEV_LABELS, levels = ELEV_LABELS), n = 0L)
  if (nrow(d)) res$n <- as.integer(table(factor(d$elev_band, levels = ELEV_LABELS)))
  res
}

#' Registros por década de colecta
#'
#' @param df Registros procesados.
#' @return `data.frame` con `decade` y `n`.
by_decade <- function(df) {
  d <- df[!is.na(df$year), , drop = FALSE]
  if (!nrow(d)) {
    return(data.frame(decade = integer(0), n = integer(0)))
  }
  dec <- (d$year %/% 10L) * 10L
  res <- as.data.frame(table(decade = dec), stringsAsFactors = FALSE)
  res$decade <- as.integer(res$decade)
  names(res)[2] <- "n"
  res[order(res$decade), ]
}

#' Registros y especies por país
#'
#' @param df Registros procesados.
#' @param top_n Número máximo de países.
#' @return `data.frame` ordenado por riqueza.
by_country <- function(df, top_n = 15) {
  d <- df[!is.na(df$countryCode), , drop = FALSE]
  if (!nrow(d)) {
    return(data.frame(country = character(0), n_records = integer(0), n_species = integer(0)))
  }
  res <- dplyr::summarise(
    dplyr::group_by(d, countryCode),
    n_records = dplyr::n(),
    n_species = dplyr::n_distinct(speciesName, na.rm = TRUE),
    .groups = "drop"
  )
  res$country <- country_name(res$countryCode)
  res <- dplyr::arrange(res, dplyr::desc(n_species), dplyr::desc(n_records))
  utils::head(as.data.frame(res), top_n)
}

#' Resumen de alertas de calidad y completitud de campos
#'
#' @param df Registros procesados.
#' @return Lista con `flags` (conteo por alerta), `completeness` y `n_with_flags`.
quality_summary <- function(df) {
  n <- nrow(df)
  codes <- names(QUALITY_FLAGS)
  cnt <- vapply(codes, function(cd) sum(grepl(cd, df$quality_flags, fixed = TRUE)), integer(1))
  flags <- data.frame(
    code = codes, label = unname(QUALITY_FLAGS), n = as.integer(cnt), stringsAsFactors = FALSE
  )
  flags$pct <- if (n) flags$n / n else NA_real_
  has_taxon <- !grepl("taxon_incompleto", df$quality_flags, fixed = TRUE)
  completeness <- data.frame(
    campo = c("Coordenadas utilizables", "Elevación", "Fecha (año)", "Familia/género/especie"),
    pct = if (n) {
      c(mean(df$has_coords), mean(!is.na(df$elev_m)), mean(!is.na(df$year)), mean(has_taxon))
    } else {
      rep(NA_real_, 4)
    }
  )
  list(
    flags = flags[order(-flags$n), ],
    completeness = completeness,
    n_with_flags = sum(nzchar(df$quality_flags))
  )
}
