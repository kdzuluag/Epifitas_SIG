# Ejecuta un archivo de pruebas y muestra el detalle de cada fallo.
# Uso: Rscript scripts/run_test_file.R test-controllers.R
f <- commandArgs(trailingOnly = TRUE)[1]
res <- testthat::test_file(
  file.path("tests/testthat", f),
  reporter = "silent", stop_on_failure = FALSE
)
n_ok <- 0
for (t in res) {
  for (r in t$results) {
    if (inherits(r, "expectation_success")) {
      n_ok <- n_ok + 1
      next
    }
    cat("\n[", t$test, "]\n", substr(conditionMessage(r), 1, 700), "\n", sep = "")
  }
}
cat("\nExpectativas OK:", n_ok, "\n")
