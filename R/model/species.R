# Modelo: listado y ficha por especie (lista de especies + visor).
# Funciones puras; solo usan datos ya generalizados (sin coordenadas exactas).

#' Listado de especies con su familia y número de registros
#'
#' @param df Registros (filtrados).
#' @return `data.frame` ordenado alfabéticamente: `speciesName`, `family`, `genus`, `n_records`.
species_list <- function(df) {
  d <- df[!is.na(df$speciesName) & nzchar(df$speciesName), , drop = FALSE]
  if (!nrow(d)) {
    return(data.frame(
      speciesName = character(0), family = character(0), genus = character(0),
      n_records = integer(0)
    ))
  }
  res <- dplyr::summarise(
    dplyr::group_by(d, speciesName),
    family = modal_value(family), genus = modal_value(genus),
    n_records = dplyr::n(), .groups = "drop"
  )
  res <- res[order(res$speciesName), c("speciesName", "family", "genus", "n_records")]
  as.data.frame(res)
}

# Valor más frecuente (empates: el primero alfabéticamente); NA si no hay datos.
modal_value <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) {
    return(NA_character_)
  }
  tab <- table(x)
  names(tab)[which.max(tab)]
}

#' Ficha resumen de una especie
#'
#' @param df Registros (filtrados).
#' @param name Nombre de la especie (`speciesName`).
#' @return Lista con taxonomía, conteos, países, rango de elevación y de años, instituciones;
#'   `NULL` si la especie no está en `df`.
species_profile <- function(df, name) {
  d <- df[!is.na(df$speciesName) & df$speciesName == name, , drop = FALSE]
  if (!nrow(d)) {
    return(NULL)
  }
  rng <- function(x) if (all(is.na(x))) NULL else range(x, na.rm = TRUE)
  list(
    name = name, order = modal_value(d$order), family = modal_value(d$family),
    genus = modal_value(d$genus), n_records = nrow(d),
    n_cells = length(unique(stats::na.omit(d$cell_id))),
    countries = sort(unique(country_name(stats::na.omit(d$countryCode)))),
    elev_range = rng(d$elev_m), year_range = rng(d$year),
    institutions = sort(unique(stats::na.omit(d$institutionCode))),
    n_flags = sum(nzchar(d$quality_flags))
  )
}
