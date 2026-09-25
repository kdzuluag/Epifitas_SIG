# QA de datos: compuertas ("quality gates") sobre los datos procesados que consume la app.
# severidad "critico" bloquea el despliegue; "aviso" exige revisión humana pero no bloquea.

# Umbrales de las compuertas (un solo lugar para ajustarlos).
QA_LIMITS <- list(
  flagged_share = 0.15, # máx. proporción de registros con alguna alerta
  incomplete_taxon = 0.02, # máx. proporción con taxonomía incompleta
  iso_country = 0.99, # mín. proporción de códigos de país con formato ISO-2
  georef_min = 0.05, # mín. proporción georreferenciada para que el mapa aporte
  elev_range = c(-50, 7000),
  max_rds_mb = 5
)

# Columnas que la app necesita para funcionar.
QA_REQUIRED_COLS <- c(
  TABLE_COLS, "record_key", "has_coords", "lat_gen", "lon_gen", "cell_deg", "cell_ix", "cell_iy",
  "sensitivityLevel", "elev_m", "elev_band", "year", "search_txt"
)

# Una fila de resultado de compuerta.
gate <- function(id, severity, description, ok, detail = "") {
  data.frame(
    id = id, severidad = severity, descripcion = description,
    ok = isTRUE(ok), detalle = as.character(detail), stringsAsFactors = FALSE
  )
}

# Proporción como texto con un decimal (12.3 %).
pct_text <- function(x) sprintf("%.1f %%", 100 * x)

# Compuertas estructurales: los datos existen y traen lo necesario.
qa_structural_gates <- function(occ) {
  forbidden <- tolower(c(COORD_COLS, "latitude", "longitude", "lat", "lon", "lng"))
  missing_cols <- setdiff(QA_REQUIRED_COLS, names(occ))
  rbind(
    gate("Q01", "critico", "Hay registros", nrow(occ) > 0, nrow(occ)),
    gate(
      "Q02", "critico", "Sin columnas de coordenadas exactas",
      !length(intersect(tolower(names(occ)), forbidden))
    ),
    gate(
      "Q03", "critico", "Columnas obligatorias presentes",
      !length(missing_cols), paste(missing_cols, collapse = ", ")
    )
  )
}

# Compuertas críticas de privacidad y coherencia de la posición generalizada.
qa_privacy_gates <- function(occ, base_deg) {
  has <- occ$has_coords %in% TRUE
  cell <- occ$cell_deg[has]
  lat <- occ$lat_gen[has]
  lon <- occ$lon_gen[has]
  slack <- 4 * base_deg
  in_bbox <- lon >= NEO_BBOX[["xmin"]] - slack & lon <= NEO_BBOX[["xmax"]] + slack &
    lat >= NEO_BBOX[["ymin"]] - slack & lat <= NEO_BBOX[["ymax"]] + slack
  in_cell <- floor(lat / cell) == occ$cell_iy[has] & floor(lon / cell) == occ$cell_ix[has]
  coarse_1 <- occ$cell_deg[has & occ$sensitivityLevel == 1L]
  coarse_2 <- occ$cell_deg[has & occ$sensitivityLevel == 2L]
  years_ok <- is.na(occ$year) | (occ$year >= YEAR_MIN & occ$year <= current_year())
  search_ok <- !anyNA(occ$search_txt) && all(occ$search_txt == tolower(occ$search_txt)) &&
    !any(grepl("[^\\x01-\\x7F]", occ$search_txt, perl = TRUE))
  rbind(
    gate(
      "Q04", "critico", "record_key único y sin NA",
      !anyNA(occ$record_key) && !anyDuplicated(occ$record_key)
    ),
    gate(
      "Q05", "critico", "has_coords coincide con posición generalizada disponible",
      all(has == (!is.na(occ$lat_gen) & !is.na(occ$lon_gen)))
    ),
    gate("Q06", "critico", "Cada punto cae dentro de su celda", all(in_cell)),
    gate(
      "Q07", "critico", "Tamaño de celda permitido (1×, 2× o 4× la base)",
      all(cell %in% (base_deg * c(1, 2, 4)))
    ),
    gate(
      "Q08", "critico", "Especies sensibles usan celdas más gruesas (nivel 1 ≥ 2×, nivel 2 ≥ 4×)",
      all(coarse_1 >= 2 * base_deg) && all(coarse_2 >= 4 * base_deg)
    ),
    gate(
      "Q09", "critico", "Posiciones dentro del cajón del Neotrópico (con holgura de celda)",
      all(in_bbox)
    ),
    gate("Q10", "critico", "Años en rango 1700–año actual (o NA)", all(years_ok)),
    gate("Q11", "critico", "Índice de búsqueda sin NA, en minúsculas y ASCII", search_ok)
  )
}

# Avisos: exigen revisión humana pero no bloquean el despliegue.
qa_warning_gates <- function(occ, rds_path = NULL) {
  flagged <- mean(nzchar(occ$quality_flags))
  incomplete <- mean(grepl("taxon_incompleto", occ$quality_flags, fixed = TRUE))
  georef <- mean(occ$has_coords %in% TRUE)
  elev_ok <- is.na(occ$elev_m) |
    (occ$elev_m >= QA_LIMITS$elev_range[1] & occ$elev_m <= QA_LIMITS$elev_range[2])
  iso <- mean(is.na(occ$countryCode) | grepl("^[A-Z]{2}$", occ$countryCode))
  out <- rbind(
    gate(
      "Q12", "aviso", "Registros con alguna alerta de calidad ≤ 15 %",
      flagged <= QA_LIMITS$flagged_share, pct_text(flagged)
    ),
    gate(
      "Q13", "aviso", "Taxonomía incompleta ≤ 2 %",
      incomplete <= QA_LIMITS$incomplete_taxon, pct_text(incomplete)
    ),
    gate("Q14", "aviso", "Elevación en rango plausible (−50 a 7000 m) o NA", all(elev_ok)),
    gate(
      "Q15", "aviso", "Códigos de país con formato ISO-2 en ≥ 99 %",
      iso >= QA_LIMITS$iso_country
    ),
    gate(
      "Q16", "aviso", "Georreferenciación ≥ 5 % (si es menor, el mapa aporta poco)",
      georef >= QA_LIMITS$georef_min, pct_text(georef)
    )
  )
  if (!is.null(rds_path) && file.exists(rds_path)) {
    size_mb <- file.size(rds_path) / 1024^2
    out <- rbind(out, gate(
      "Q17", "aviso", sprintf("Archivo procesado ≤ %d MB", QA_LIMITS$max_rds_mb),
      size_mb <= QA_LIMITS$max_rds_mb, sprintf("%.2f MB", size_mb)
    ))
  }
  out
}

#' Ejecuta las compuertas de calidad sobre los datos procesados
#'
#' @param occ Registros procesados (salida de `clean_occurrences()`).
#' @param base_deg Tamaño base de celda en grados.
#' @param rds_path Ruta del `.rds` procesado (para la compuerta de tamaño); opcional.
#' @return `data.frame` con `id`, `severidad` (`critico`/`aviso`), `descripcion`, `ok`, `detalle`.
qa_data_gates <- function(occ, base_deg = attr(occ, "grid_deg") %||% 0.5, rds_path = NULL) {
  structural <- qa_structural_gates(occ)
  if (!structural$ok[structural$id == "Q03"]) {
    return(structural)
  }
  rbind(structural, qa_privacy_gates(occ, base_deg), qa_warning_gates(occ, rds_path))
}

#' Indica si se superaron todas las compuertas críticas
#'
#' @param res Resultado de [qa_data_gates()].
#' @return `TRUE` o `FALSE`.
qa_gate_passed <- function(res) all(res$ok[res$severidad == "critico"])
