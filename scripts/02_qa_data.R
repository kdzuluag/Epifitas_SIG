# Compuertas de QA de datos. Sale con estado 1 si falla alguna compuerta CRÍTICA (bloquea el
# despliegue). Ejecutar desde la raíz:  Rscript scripts/02_qa_data.R
suppressPackageStartupMessages(library(dplyr))
source("scripts/load_project.R")
load_project(c("core", "model"))

path <- Sys.getenv("EPIG_OCC_PATH", "data/processed/occurrences.rds")
if (!file.exists(path)) stop("No existe ", path, ". Ejecute scripts/01_build_data.R primero.")
res <- qa_data_gates(readRDS(path), rds_path = path)

show <- res
show$estado <- ifelse(show$ok, "OK", ifelse(show$severidad == "critico", "FALLA", "AVISO"))
cols <- c("id", "severidad", "estado", "descripcion", "detalle")
print(show[, cols], row.names = FALSE, right = FALSE)

critical <- res$severidad == "critico"
cat(sprintf(
  "\nCríticas: %d/%d OK | Avisos con hallazgos: %d\n",
  sum(res$ok[critical]), sum(critical), sum(!res$ok[!critical])
))
if (!qa_gate_passed(res)) {
  cat("COMPUERTA BLOQUEANTE: no despliegue.\n")
  quit(status = 1)
}
cat("Compuerta de datos superada.\n")
