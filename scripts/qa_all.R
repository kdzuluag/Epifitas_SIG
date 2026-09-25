# Orquestador de QA: una sola orden para decidir si el código y los datos están listos.
#   Rscript scripts/qa_all.R
# Etapas: (1) compuertas de datos, (2) estilo con lintr, (3) pruebas automatizadas,
# (4) cobertura con covr (informativa con piso mínimo), (5) veredicto con código de salida.
suppressPackageStartupMessages(library(dplyr))
source("scripts/load_project.R")
load_project(c("core", "model"))

COVERAGE_FLOOR <- 90 # % mínimo de cobertura de líneas; por debajo la etapa falla

# ---- Registro de etapas (sin estado global: cada etapa devuelve su fila) ----
stage <- function(name, ok, detail = "") {
  data.frame(etapa = name, resultado = if (ok) "OK" else "FALLA", detalle = detail)
}

stage_data_gates <- function(path = "data/processed/occurrences.rds") {
  name <- "Compuertas de datos (críticas)"
  if (!file.exists(path)) {
    return(stage(name, FALSE, "faltan datos procesados (scripts/01_build_data.R)"))
  }
  g <- qa_data_gates(readRDS(path), rds_path = path)
  critical <- g$severidad == "critico"
  detail <- sprintf(
    "%d/%d críticas OK; %d aviso(s)", sum(g$ok[critical]), sum(critical), sum(!g$ok[!critical])
  )
  stage(name, qa_gate_passed(g), detail)
}

stage_lint <- function() {
  targets <- c("app.R", "R", "scripts", "tests", "deploy")
  n <- sum(vapply(targets, function(t) {
    l <- if (dir.exists(t)) lintr::lint_dir(t, pattern = "\\.[Rr]$") else lintr::lint(t)
    length(l)
  }, integer(1)))
  stage("Estilo (lintr, líneas ≤ 100)", n == 0, sprintf("%d aviso(s)", n))
}

run_tests <- function() {
  as.data.frame(testthat::test_dir("tests/testthat", reporter = "silent", stop_on_failure = FALSE))
}

stage_tests <- function(t) {
  detail <- sprintf(
    "%d pruebas, %d expectativas, %d fallos, %d errores, %d omitidas",
    nrow(t), sum(t$passed), sum(t$failed), sum(t$error), sum(t$skipped)
  )
  rbind(
    stage("Pruebas automatizadas", sum(t$failed) == 0 && sum(t$error) == 0, detail),
    stage("Sin pruebas omitidas", sum(t$skipped) == 0, sprintf("%d omitidas", sum(t$skipped)))
  )
}

stage_coverage <- function() {
  suppressPackageStartupMessages(library(testthat)) # covr ejecuta las pruebas fuera de testthat
  Sys.setenv(EPIG_COVERAGE = "1") # las pruebas de rendimiento se omiten con código instrumentado
  on.exit(Sys.unsetenv("EPIG_COVERAGE"), add = TRUE)
  src <- list.files("R", "\\.R$", recursive = TRUE, full.names = TRUE)
  tests <- list.files("tests/testthat", "^test-.*\\.R$", full.names = TRUE)
  tst <- c("tests/testthat/setup.R", tests)
  measured <- tryCatch(
    list(pct = covr::percent_coverage(covr::file_coverage(src, tst)), reason = NA_character_),
    error = function(e) list(pct = NA_real_, reason = conditionMessage(e))
  )
  pct <- measured$pct
  detail <- if (is.na(pct)) {
    paste("no se pudo medir:", substr(measured$reason, 1, 200))
  } else {
    sprintf("%.1f %% (piso %d %%)", pct, COVERAGE_FLOOR)
  }
  stage("Cobertura de líneas (covr)", !is.na(pct) && pct >= COVERAGE_FLOOR, detail)
}

t <- run_tests()
report <- dplyr::bind_rows(stage_data_gates(), stage_lint(), stage_tests(t), stage_coverage())

cat("\n==== INFORME QA ====\n")
print(report, row.names = FALSE, right = FALSE)
bad <- t[t$failed > 0 | t$error, c("file", "test")]
if (nrow(bad)) {
  cat("\nPruebas con problemas:\n")
  print(bad, row.names = FALSE)
}
ok <- all(report$resultado == "OK")
verdict <- if (ok) {
  "APTO PARA REVISIÓN/DESPLIEGUE (falta la lista de verificación manual de docs/qa.qmd)"
} else {
  "NO APTO"
}
cat(sprintf("\nVeredicto: %s\n", verdict))
quit(status = if (ok) 0 else 1)
