# EpIG · Visualizador de registros de epífitas vasculares del Neotrópico
# Arquitectura MVC sobre Shiny (ver docs/arquitectura.qmd):
#   R/core        configuración, esquema Darwin Core, utilidades
#   R/model       reglas de negocio puras: SIG, ETL, taxonomía, estadística, seguridad, persistencia
#   R/controller  reactividad: orquesta Modelo <-> Vista (filtros, datos, selección, verificación)
#   R/view        interfaz: módulos Shiny sin lógica de negocio
# Ejecutar la app con shiny::runApp(); la QA completa con scripts/qa_all.R.

options(
  shiny.maxRequestSize = 1024^2, # sin subida de archivos: 1 MB basta para la app
  shiny.sanitize.errors = TRUE, # no filtrar trazas internas al navegador
  shiny.autoreload = FALSE
)

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(leaflet)
  library(dplyr)
  library(ggplot2)
})

for (d in c("core", "model", "controller", "view")) {
  for (f in sort(list.files(file.path("R", d), "\\.R$", full.names = TRUE))) {
    source(f, local = globalenv(), encoding = "UTF-8")
  }
}

cfg <- epig_config()
repo <- load_repository(cfg) # una sola vez por proceso, no por sesión

shinyApp(
  ui = main_ui(cfg, repo),
  server = function(input, output, session) main_server(input, output, session, cfg, repo)
)
