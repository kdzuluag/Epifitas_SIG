# ETL: datos crudos -> data/processed/{occurrences,neotropic,bioregion_mask}.rds
# Ejecutar desde la raíz del proyecto:  Rscript scripts/01_build_data.R
# Variables opcionales: EPIG_RAW_FILE (ruta .xlsx/.csv), EPIG_RAW_SHAPE (.shp),
# EPIG_GRID_DEG (tamaño base de celda, def. 0.5).
# Importante: los datos crudos (con coordenadas exactas) viven en data/raw y NO se despliegan.

suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
})
source("scripts/load_project.R")
load_project(c("core", "model"))

env_or <- function(k, d) {
  v <- Sys.getenv(k, "")
  if (nzchar(v)) v else d
}
grid_deg <- as.numeric(env_or("EPIG_GRID_DEG", "0.5"))

# ---- 1. Datos de ocurrencia ----
raw_file <- Sys.getenv("EPIG_RAW_FILE", "")
if (!nzchar(raw_file)) {
  real <- list.files("data/raw", pattern = "\\.xlsx$", full.names = TRUE)
  raw_file <- if (length(real)) real[1] else "data/raw/DEMO_Occurrences.csv"
}
stopifnot(file.exists(raw_file))
message("Leyendo: ", raw_file)
raw <- if (grepl("\\.xlsx?$", raw_file, ignore.case = TRUE)) {
  readxl::read_excel(raw_file, col_types = "text")
} else {
  utils::read.csv(raw_file, colClasses = "character", fileEncoding = "UTF-8", na.strings = "")
}
occ <- clean_occurrences(raw, grid_deg = grid_deg)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
saveRDS(occ, "data/processed/occurrences.rds", compress = "xz")

# ---- 2. Capas SIG ----
shp_file <- Sys.getenv("EPIG_RAW_SHAPE", "")
if (!nzchar(shp_file)) {
  cand <- list.files("data/raw", pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)
  shp_file <- if (length(cand)) cand[1] else ""
}
if (nzchar(shp_file)) {
  message("Leyendo shapefile: ", shp_file)
  shape <- sf::st_transform(sf::st_read(shp_file, quiet = TRUE), 4326)
} else {
  message("Sin shapefile real: usando polígono ESQUEMÁTICO de demostración.")
  hull <- matrix(c(
    -117, 32, -97, 27, -81, 25.5, -72, 20, -60, 11, -50, 2, -35, -6, -40, -23, -48, -28, -58, -38,
    -65, -42, -72, -52, -75, -45, -73, -30, -71, -18, -80, -5, -81, 4, -86, 10, -92, 14, -105, 20,
    -112, 27, -117, 32
  ), ncol = 2, byrow = TRUE)
  shape <- sf::st_sf(name = "DEMO", geometry = sf::st_sfc(sf::st_polygon(list(hull)), crs = 4326))
}
regions <- extract_neotropical_regions(shape)
neotropic <- simplify_shape(shape)
attr(neotropic, "source") <- if (nzchar(shp_file)) basename(shp_file) else "esquemático (demo)"
saveRDS(neotropic, "data/processed/neotropic.rds")
saveRDS(build_bioregion_mask(neotropic), "data/processed/bioregion_mask.rds")
saveRDS(regions, "data/processed/neotropic_regions.rds")

# ---- 3. Resumen ----
cat(sprintf(
  "\nRegistros: %d | Especies: %d | Con coordenadas utilizables: %d | Con alertas: %d\n",
  nrow(occ), n_distinct(occ$speciesName, na.rm = TRUE), sum(occ$has_coords),
  sum(nzchar(occ$quality_flags))
))
size_kb <- file.size("data/processed/occurrences.rds") / 1024
cat(sprintf("Celda base: %s° | Tamaño occurrences.rds: %.0f KB\n", format(grid_deg), size_kb))
