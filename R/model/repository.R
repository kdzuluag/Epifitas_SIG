# Repositorio de datos: se carga UNA vez al iniciar el proceso (no por sesión).

#' Carga los datos procesados que consume la app
#'
#' Verifica que no existan coordenadas exactas antes de aceptarlos.
#'
#' @param cfg Configuración ([epig_config()]).
#' @return Lista con `occ`, `neotropic`, `mask`, `admin1`, `regions`, `meta`, `countries` y
#'   `year_range`. `admin1` es `NULL` y `regions` es `character(0)` si sus archivos no existen
#'   (la Vista debe ofrecer entonces un campo de texto libre en su lugar).
load_repository <- function(cfg) {
  need <- c(cfg$occ_path, cfg$neotropic_path, cfg$mask_path)
  missing <- need[!file.exists(need)]
  if (length(missing)) {
    stop(
      "Faltan datos procesados: ", paste(missing, collapse = ", "),
      ". Ejecute scripts/00_make_demo_data.R (demo) y scripts/01_build_data.R.",
      call. = FALSE
    )
  }
  occ <- readRDS(cfg$occ_path)
  assert_no_raw_coordinates(occ)
  neotropic <- readRDS(cfg$neotropic_path)
  list(
    occ = occ,
    neotropic = neotropic,
    mask = readRDS(cfg$mask_path),
    admin1 = if (file.exists(cfg$admin1_path)) readRDS(cfg$admin1_path) else NULL,
    regions = if (file.exists(cfg$regions_path)) readRDS(cfg$regions_path) else character(0),
    meta = list(
      demo = isTRUE(attr(occ, "demo")), built_at = attr(occ, "built_at") %||% "?",
      grid_deg = attr(occ, "grid_deg") %||% NA_real_,
      shape_source = attr(neotropic, "source") %||% "desconocida"
    ),
    countries = sort(unique(occ$countryCode[!is.na(occ$countryCode)])),
    year_range = suppressWarnings(range(occ$year, na.rm = TRUE))
  )
}
