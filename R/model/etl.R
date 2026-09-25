# Ingeniería de datos: validación, limpieza, control de calidad y generalización (Darwin Core).
# Función pura: recibe el data.frame crudo y devuelve el data.frame que consumirá la app.

#' Limpia una cadena: colapsa espacios y convierte vacíos en NA
#'
#' @param x Vector.
#' @return Vector de texto.
clean_str <- function(x) {
  x <- as.character(x)
  x <- trimws(gsub("\\s+", " ", x))
  x[!nzchar(x)] <- NA_character_
  x
}

#' Primera letra en mayúscula y el resto en minúscula
#'
#' @param x Vector de texto.
#' @return Vector de texto.
cap_first <- function(x) {
  ifelse(is.na(x), NA_character_, paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2))))
}

#' Interpreta elevaciones escritas en formatos heterogéneos
#'
#' Entiende "1200", "1200 m", "1,200 m", "c. 800", "2500 m s.n.m." y rangos
#' "1200-1500" (usa el punto medio).
#'
#' @param x Vector de texto.
#' @return Vector numérico en metros (NA si no se puede interpretar).
parse_elevation <- function(x) {
  vapply(as.character(x), function(s) {
    if (is.na(s)) {
      return(NA_real_)
    }
    s <- gsub("(?<=\\d),(?=\\d{3}\\b)", "", s, perl = TRUE)
    nums <- suppressWarnings(as.numeric(regmatches(s, gregexpr("\\d+(?:\\.\\d+)?", s))[[1]]))
    if (!length(nums)) {
      return(NA_real_)
    }
    is_range <- grepl("\\d\\s*[-–—]\\s*\\d", s) && length(nums) >= 2
    if (is_range) mean(nums[1:2]) else nums[1]
  }, numeric(1), USE.NAMES = FALSE)
}

#' Extrae el año de fechas completas o parciales
#'
#' @param x Vector de texto ("2019-05-14", "1999", "14/05/1985", ...).
#' @param max_year Año máximo plausible.
#' @return Vector entero (NA si no hay un año entre `YEAR_MIN` y `max_year`).
parse_year <- function(x, max_year = current_year()) {
  x <- as.character(x)
  year_pattern <- "(1[6-9]\\d{2}|20\\d{2})"
  y <- suppressWarnings(as.integer(sub(paste0(".*?", year_pattern, ".*"), "\\1", x, perl = TRUE)))
  y[!grepl(year_pattern, x)] <- NA_integer_
  y[!is.na(y) & (y < YEAR_MIN | y > max_year)] <- NA_integer_
  y
}

#' Convierte a número aceptando coma decimal
#'
#' @param x Vector.
#' @return Vector numérico (NA donde no se pueda convertir).
to_num <- function(x) suppressWarnings(as.numeric(gsub(",", ".", as.character(x), fixed = TRUE)))

#' Verifica que estén todas las columnas Darwin Core obligatorias
#'
#' @param raw `data.frame` de entrada.
#' @return `TRUE` de forma invisible; detiene la ejecución si faltan columnas.
validate_schema <- function(raw) {
  missing <- setdiff(DWC_COLS, names(raw))
  if (length(missing)) {
    stop("Faltan columnas obligatorias: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

# Limpia los campos de texto y normaliza taxonomía y código de país.
normalize_text_fields <- function(d) {
  for (col in setdiff(DWC_COLS, COORD_COLS)) d[[col]] <- clean_str(d[[col]])
  d$order <- cap_first(d$order)
  d$family <- cap_first(d$family)
  d$genus <- cap_first(d$genus)
  d$countryCode <- toupper(d$countryCode)
  d
}

# Clave única y estable por registro; marca los recordID repetidos.
make_record_keys <- function(record_id) {
  duplicated_id <- !is.na(record_id) &
    (duplicated(record_id) | duplicated(record_id, fromLast = TRUE))
  key <- record_id
  key[is.na(key)] <- paste0("fila", which(is.na(key)))
  list(key = make.unique(key, sep = "#"), duplicated = duplicated_id)
}

# Valida coordenadas: devuelve las utilizables y los motivos de descarte.
validate_coordinates <- function(lat_raw, lon_raw) {
  lat <- to_num(lat_raw)
  lon <- to_num(lon_raw)
  any_given <- !is.na(lat) | !is.na(lon)
  out_of_range <- abs(lat) > LAT_LIMIT | abs(lon) > LON_LIMIT
  null_island <- lat == 0 & lon == 0
  invalid <- any_given & (is.na(lat) | is.na(lon) | out_of_range | null_island)
  in_box <- lon >= NEO_BBOX[["xmin"]] & lon <= NEO_BBOX[["xmax"]] &
    lat >= NEO_BBOX[["ymin"]] & lat <= NEO_BBOX[["ymax"]]
  outside <- !invalid & any_given & !in_box
  usable <- any_given & !invalid & !outside
  lat[!usable] <- NA_real_
  lon[!usable] <- NA_real_
  list(lat = lat, lon = lon, usable = usable, invalid = invalid, outside = outside)
}

# Añade elevación interpretada, piso altitudinal y año.
add_ecology_and_time <- function(d, max_year) {
  d$elev_m <- parse_elevation(d$verbatimElevation)
  d$elev_band <- cut(d$elev_m, ELEV_BREAKS, labels = ELEV_LABELS, right = FALSE)
  d$year <- parse_year(d$eventDate, max_year)
  d
}

# Familia más frecuente de cada género (para detectar familias atípicas).
modal_family_by_genus <- function(family, genus) {
  tapply(family, genus, function(x) {
    x <- x[!is.na(x)]
    if (length(x)) names(which.max(table(x))) else NA_character_
  })
}

#' Alertas de calidad por registro
#'
#' @param d Registros ya normalizados (con `elev_m` y `year`).
#' @param coords Resultado de `validate_coordinates()`.
#' @param duplicated_id Vector lógico de `recordID` repetidos.
#' @return Vector de texto con los códigos de alerta separados por ";" ("" si no hay).
compute_quality_flags <- function(d, coords, duplicated_id) {
  first_word <- sub(" .*$", "", d$speciesName)
  modal <- modal_family_by_genus(d$family, d$genus)
  atypical_family <- !is.na(d$family) & !is.na(d$genus) & d$family != unname(modal[d$genus])
  flags <- list(
    taxon_incompleto = is.na(d$family) | is.na(d$genus) | is.na(d$speciesName),
    genero_no_coincide = !is.na(first_word) & !is.na(d$genus) & first_word != d$genus,
    genero_multiples_familias = atypical_family,
    pais_invalido = !is.na(d$countryCode) & !grepl("^[A-Z]{2}$", d$countryCode),
    elevacion_atipica = !is.na(d$elev_m) &
      (d$elev_m < ELEV_ATYPICAL_MIN | d$elev_m > ELEV_ATYPICAL_MAX),
    fecha_invalida = !is.na(d$eventDate) & is.na(d$year),
    coordenadas_invalidas = coords$invalid,
    coordenadas_fuera_neotropico = coords$outside,
    recordID_duplicado = duplicated_id
  )
  mat <- do.call(cbind, lapply(flags, function(f) {
    f[is.na(f)] <- FALSE
    f
  }))
  apply(mat, 1, function(r) paste(colnames(mat)[r], collapse = ";"))
}

# Índice de búsqueda precomputado (optimización): minúsculas y sin tildes.
build_search_index <- function(d) {
  parts <- lapply(TAX_COLS_SEARCH, function(cn) d[[cn]])
  norm_text(do.call(paste, c(parts, sep = " ")))
}

# Añade la posición generalizada y descarta las coordenadas exactas.
add_generalized_position <- function(d, coords, grid_deg, seed) {
  sens <- if ("sensitivityLevel" %in% names(d)) d$sensitivityLevel else rep(0L, nrow(d))
  d$sensitivityLevel <- suppressWarnings(as.integer(sens))
  d$sensitivityLevel[is.na(d$sensitivityLevel)] <- 0L
  cell <- sensitivity_to_cell(d$sensitivityLevel, grid_deg)
  d <- cbind(d, generalize_coords(coords$lat, coords$lon, d$record_key, cell, seed))
  d$has_coords <- coords$usable
  d[, COORD_COLS] <- NULL
  d
}

#' Limpia, valida y generaliza los registros crudos
#'
#' @param raw `data.frame` con las 20 columnas Darwin Core (opcional: `sensitivityLevel`).
#' @param grid_deg Tamaño base de celda en grados.
#' @param seed Semilla de texto para la posición pseudoaleatoria dentro de la celda.
#' @param max_year Año máximo plausible de colecta.
#' @return `data.frame` sin coordenadas exactas, con posición generalizada, elevación, año,
#'   alertas de calidad e índice de búsqueda. Atributos: `grid_deg`, `built_at`, `demo`.
clean_occurrences <- function(raw, grid_deg = 0.5, seed = "epig", max_year = current_year()) {
  validate_schema(raw)
  d <- normalize_text_fields(as.data.frame(raw, stringsAsFactors = FALSE))
  keys <- make_record_keys(d$recordID)
  d$record_key <- keys$key
  coords <- validate_coordinates(d$decimalLatitude, d$decimalLongitude)

  d <- add_generalized_position(d, coords, grid_deg, seed)
  d <- add_ecology_and_time(d, max_year)
  d$quality_flags <- compute_quality_flags(d, coords, keys$duplicated)
  d$search_txt <- build_search_index(d)

  assert_no_raw_coordinates(d)
  attr(d, "grid_deg") <- grid_deg
  attr(d, "built_at") <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  ids <- d$datasetID[!is.na(d$datasetID)]
  attr(d, "demo") <- length(ids) > 0 && all(grepl("^DEMO", ids))
  d
}
