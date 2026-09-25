# Preparación común de las pruebas. También la usa covr (scripts/qa_all.R) fuera de testthat.
root <- if (file.exists("app.R")) {
  getwd()
} else {
  normalizePath(file.path(testthat::test_path(), "..", ".."))
}

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(leaflet)
  library(dplyr)
  library(ggplot2)
})

# Cada capa se carga solo si aún no está disponible (con covr ya viene instrumentada, y
# scripts/qa_all.R ya cargó núcleo y modelo).
layer_marker <- c(
  core = "epig_config", model = "qa_data_gates", controller = "data_controller", view = "main_ui"
)
for (d in names(layer_marker)) {
  if (exists(layer_marker[[d]], mode = "function")) next
  files <- sort(list.files(file.path(root, "R", d), "\\.R$", full.names = TRUE))
  for (f in files) source(f, local = globalenv(), encoding = "UTF-8")
}

occ_path <- function() file.path(root, "data", "processed", "occurrences.rds")
RAW_DEMO <- function() file.path(root, "data", "raw", "DEMO_Occurrences.csv")
skip_if_no_data <- function() {
  skip_if_not(file.exists(occ_path()), "ejecute scripts/00 y 01 primero")
}

mini_raw <- function() {
  data.frame(
    datasetID = "D1", recordID = c("A1", "A2", "A3", "A3", "A5", "A6"),
    order = "asparagales",
    family = c(
      "orchidaceae", "Orchidaceae", "Orchidaceae", "Orchidaceae", "Bromeliaceae", "Orchidaceae"
    ),
    genus = c("epidendrum", "Epidendrum", "Epidendrum", "Epidendrum", "Tillandsia", "Epidendrum"),
    speciesName = c(
      "Epidendrum alba", "Epidendrum rosa", "Pleurothallis x", "Epidendrum nigra",
      "Tillandsia rubra", "Epidendrum lutea"
    ),
    scientificName = NA, countryCode = c("co", "CO", "EC", "PE", "BR", "XXX"), locality = "L",
    decimalLatitude = c("4.6512", "0", "-1,25", NA, "45.0", "91"),
    decimalLongitude = c("-74.0831", "0", "-78,5", NA, "10.0", "10"),
    verbatimElevation = c("1,200 m", "1200-1500", "c. 800", "7000", NA, "2500"),
    eventDate = c("2019-05-14", "1999", "sin fecha", NA, "2030-01-01", "14/05/1985"),
    basisOfRecord = "PreservedSpecimen", institutionCode = "I", collectionCode = "C",
    catalogNumber = "1", recordedBy = "X", recordNumber = "1", identifiedBy = "Y",
    stringsAsFactors = FALSE
  )
}

# Configuración aislada: rutas reales de solo lectura + estado en archivos temporales.
test_cfg <- function(...) {
  vars <- utils::modifyList(list(EPIG_AUTH_MODE = "none"), list(...))
  cfg <- epig_config(env = function(k, d = "") if (!is.null(vars[[k]])) vars[[k]] else d)
  cfg$occ_path <- occ_path()
  cfg$neotropic_path <- file.path(root, "data", "processed", "neotropic.rds")
  cfg$mask_path <- file.path(root, "data", "processed", "bioregion_mask.rds")
  cfg$admin1_path <- file.path(root, "data", "processed", "admin1.rds")
  cfg$regions_path <- file.path(root, "data", "processed", "neotropic_regions.rds")
  cfg$verification_db <- tempfile(fileext = ".sqlite")
  cfg$audit_log <- tempfile(fileext = ".jsonl")
  cfg
}

# Código fuente sin comentarios (una fila por línea), para las pruebas de guardia.
read_code <- function(files) {
  dplyr::bind_rows(lapply(files, function(f) {
    x <- readLines(f, warn = FALSE, encoding = "UTF-8")
    text <- sub("\\s+#.*$", "", sub("^\\s*#.*$", "", x))
    data.frame(file = f, line = seq_along(x), text = text, stringsAsFactors = FALSE)
  }))
}

r_files <- function(dirs = c("R"), extra = character(0)) {
  found <- lapply(dirs, function(d) {
    list.files(file.path(root, d), "\\.R$", recursive = TRUE, full.names = TRUE)
  })
  c(unlist(found), file.path(root, extra))
}

# Texto de una función de nivel superior de un archivo (por su nombre), o "" si no existe.
function_body <- function(file, name) {
  exprs <- parse(file, keep.source = TRUE, encoding = "UTF-8")
  refs <- attr(exprs, "srcref")
  for (i in seq_along(exprs)) {
    e <- exprs[[i]]
    is_def <- is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name(name))
    if (is_def) {
      return(paste(as.character(refs[[i]]), collapse = "\n"))
    }
  }
  ""
}
