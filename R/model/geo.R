# SIG: generalización de coordenadas (privacidad por diseño) y agregación por celdas.
#
# Principio: la ubicación mostrada es una posición pseudoaleatoria DENTRO de la celda de la
# cuadrícula, calculada solo con la identidad del registro y NO con la posición real dentro de la
# celda. Lo único que se revela es la celda (por defecto 0,5° ≈ 55 km). Es una generalización a
# cuadrícula en la línea de las buenas prácticas de GBIF para especies sensibles.
# Esta función se ejecuta en el ETL: la app desplegada nunca recibe coordenadas exactas.

# Sensibilidad 0/1/2 -> celdas cada vez más gruesas
GRID_MULTIPLIER <- c(`0` = 1, `1` = 2, `2` = 4)
JITTER_MARGIN <- 0.02 # evita puntos pegados al borde de la celda

#' Número pseudoaleatorio determinista en (0, 1) a partir de una clave
#'
#' @param key Vector de identificadores.
#' @param seed Semilla de texto (solo da variedad entre construcciones; no aporta seguridad).
#' @param stream Entero que separa flujos independientes (0, 1, ...).
#' @return Vector numérico en (0, 1).
hash_unit <- function(key, seed, stream = 1L) {
  s <- paste(key, seed, stream, sep = "|")
  vapply(s, function(x) {
    h <- digest::digest(x, algo = "sha256", serialize = FALSE)
    strtoi(substr(h, 1, 7), 16L) / 16^7
  }, numeric(1), USE.NAMES = FALSE)
}

#' Generaliza coordenadas a una posición dentro de su celda de cuadrícula
#'
#' La posición devuelta no depende de la posición real dentro de la celda: solo se
#' conserva el índice de celda.
#'
#' @param lat,lon Coordenadas decimales (NA permitido).
#' @param key Identificador único de cada registro.
#' @param cell_deg Tamaño de celda en grados (escalar o vector).
#' @param seed Semilla de texto.
#' @return `data.frame` con `lon_gen`, `lat_gen`, `cell_deg`, `cell_ix`, `cell_iy`, `cell_id`.
generalize_coords <- function(lat, lon, key, cell_deg, seed = "epig") {
  stopifnot(length(lat) == length(lon), length(lat) == length(key))
  cell_deg <- rep_len(cell_deg, length(lat))
  ix <- floor(lon / cell_deg)
  iy <- floor(lat / cell_deg)
  span <- 1 - 2 * JITTER_MARGIN
  u1 <- JITTER_MARGIN + span * hash_unit(key, seed, 1L)
  u2 <- JITTER_MARGIN + span * hash_unit(key, seed, 2L)
  data.frame(
    lon_gen = (ix + u1) * cell_deg,
    lat_gen = (iy + u2) * cell_deg,
    cell_deg = ifelse(is.na(ix), NA_real_, cell_deg),
    cell_ix = as.integer(ix),
    cell_iy = as.integer(iy),
    cell_id = ifelse(
      is.na(ix), NA_character_,
      sprintf("%s_%d_%d", format(cell_deg), as.integer(ix), as.integer(iy))
    ),
    stringsAsFactors = FALSE
  )
}

#' Tamaño de celda según nivel de sensibilidad
#'
#' @param sensitivity Niveles 0, 1 o 2 (otros valores se tratan como 0).
#' @param base_deg Tamaño base de celda en grados.
#' @return Vector de tamaños de celda.
sensitivity_to_cell <- function(sensitivity, base_deg) {
  s <- suppressWarnings(as.integer(sensitivity))
  s[is.na(s) | !s %in% c(0L, 1L, 2L)] <- 0L
  base_deg * unname(GRID_MULTIPLIER[as.character(s)])
}

#' Detiene la ejecución si los datos traen columnas de coordenadas exactas
#'
#' @param df `data.frame` que consumirá la app.
#' @return `TRUE` de forma invisible si no hay columnas prohibidas.
assert_no_raw_coordinates <- function(df) {
  forbidden <- c(COORD_COLS, "latitude", "longitude", "lat", "lon", "lng")
  bad <- intersect(tolower(names(df)), tolower(forbidden))
  if (length(bad)) {
    stop(
      "Los datos de la app contienen columnas de coordenadas exactas (",
      paste(bad, collapse = ", "), "). Ejecute scripts/01_build_data.R para generar ",
      "la versión generalizada.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# Grados con hemisferio (N/S/E/O) y un decimal.
fmt_deg <- function(v, pos, neg) sprintf("%.1f°%s", abs(v), ifelse(v >= 0, pos, neg))

#' Texto legible de los límites de una celda
#'
#' @param cell_deg Tamaño de celda en grados.
#' @param ix,iy Índices de celda en longitud y latitud.
#' @return Vector de texto.
cell_label <- function(cell_deg, ix, iy) {
  x0 <- ix * cell_deg
  y0 <- iy * cell_deg
  sprintf(
    "%s a %s, %s a %s (celda de %s°)",
    fmt_deg(y0, "N", "S"), fmt_deg(y0 + cell_deg, "N", "S"),
    fmt_deg(x0, "E", "O"), fmt_deg(x0 + cell_deg, "E", "O"), format(cell_deg)
  )
}

#' Riqueza y registros por celda de la cuadrícula
#'
#' @param df Registros con `cell_id`, `cell_deg`, `cell_ix`, `cell_iy`, `speciesName`.
#' @return `data.frame` por celda (o `NULL` si no hay registros ubicables).
richness_by_cell <- function(df) {
  d <- df[!is.na(df$cell_id), , drop = FALSE]
  if (!nrow(d)) {
    return(NULL)
  }
  agg <- dplyr::summarise(
    dplyr::group_by(d, cell_id, cell_deg, cell_ix, cell_iy),
    n_records = dplyr::n(),
    n_species = dplyr::n_distinct(speciesName, na.rm = TRUE),
    .groups = "drop"
  )
  agg$label <- cell_label(agg$cell_deg, agg$cell_ix, agg$cell_iy)
  agg
}

#' Convierte celdas de cuadrícula en polígonos `sf` (EPSG:4326)
#'
#' @param cells Resultado de [richness_by_cell()].
#' @return Objeto `sf` o `NULL` si `cells` está vacío.
cells_to_sf <- function(cells) {
  if (is.null(cells) || !nrow(cells)) {
    return(NULL)
  }
  polys <- lapply(seq_len(nrow(cells)), function(i) {
    g <- cells$cell_deg[i]
    x0 <- cells$cell_ix[i] * g
    y0 <- cells$cell_iy[i] * g
    corners <- c(x0, y0, x0 + g, y0, x0 + g, y0 + g, x0, y0 + g, x0, y0)
    ring <- matrix(corners, ncol = 2, byrow = TRUE)
    sf::st_polygon(list(ring))
  })
  sf::st_sf(cells, geometry = sf::st_sfc(polys, crs = 4326))
}

# Ejecuta `fn` con la geometría esférica (s2) desactivada y la restaura al terminar.
with_planar_geometry <- function(fn) {
  old <- suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(old)), add = TRUE)
  fn()
}

#' Máscara biorregional: el mundo menos el Neotrópico
#'
#' @param neotropic Objeto `sf` con el polígono del Neotrópico.
#' @return Objeto `sf` con la geometría fuera del Neotrópico.
build_bioregion_mask <- function(neotropic) {
  with_planar_geometry(function() {
    bbox <- c(xmin = -180, ymin = -85, xmax = 180, ymax = 85)
    world <- sf::st_as_sfc(sf::st_bbox(bbox, crs = sf::st_crs(4326)))
    inside <- sf::st_union(sf::st_make_valid(sf::st_geometry(neotropic)))
    sf::st_sf(name = "Fuera del Neotrópico", geometry = sf::st_difference(world, inside))
  })
}

#' Disuelve y simplifica un polígono para reducir el peso que viaja al navegador
#'
#' Todas las unidades del objeto (p. ej. las provincias biogeográficas) se unen en un único
#' polígono del Neotrópico.
#'
#' @param shape Objeto `sf`.
#' @param tolerance_m Tolerancia de simplificación en metros.
#' @return Objeto `sf` en EPSG:4326.
simplify_shape <- function(shape, tolerance_m = 5000) {
  with_planar_geometry(function() {
    g <- sf::st_make_valid(sf::st_transform(sf::st_geometry(shape), 3857))
    g <- sf::st_union(g)
    g <- sf::st_simplify(g, preserveTopology = TRUE, dTolerance = tolerance_m)
    sf::st_sf(name = "Neotrópico", geometry = sf::st_transform(g, 4326))
  })
}

#' Nombres de las provincias biogeográficas de un polígono del Neotrópico
#'
#' Se usan para el desplegable de "región neotrópica" de la verificación taxonómica. Si el
#' polígono no trae la columna de provincia (p. ej. el contorno esquemático de demostración),
#' devuelve un vector vacío y la Vista debe ofrecer un campo de texto libre en su lugar.
#'
#' @param shape Objeto `sf` recién leído (antes de [simplify_shape()], que descarta los atributos).
#' @param col Nombre de la columna con el nombre de la provincia.
#' @return Vector de caracteres, ordenado alfabéticamente y sin repetidos.
extract_neotropical_regions <- function(shape, col = "Province_1") {
  if (!col %in% names(shape)) {
    return(character(0))
  }
  vals <- as.character(shape[[col]])
  sort(unique(vals[!is.na(vals) & nzchar(vals)]))
}
