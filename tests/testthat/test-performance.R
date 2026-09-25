# Presupuestos de rendimiento (holgados para no ser frágiles en CI): detectan regresiones grandes.
# Se omiten al medir cobertura (el código instrumentado es más lento).
elapsed <- function(expr) unname(system.time(expr)[["elapsed"]])
skip_if_coverage <- function() skip_if(nzchar(Sys.getenv("EPIG_COVERAGE")), "midiendo cobertura")

test_that("PERF: la carga del repositorio es rápida y el objeto es pequeño", {
  skip_if_coverage()
  skip_if_no_data()
  cfg <- test_cfg()
  expect_lt(elapsed(repo <- load_repository(cfg)), 3)
  expect_lt(as.numeric(object.size(repo$occ)) / 1024^2, 25)
  expect_lt(file.size(cfg$occ_path) / 1024^2, 5)
})

test_that("PERF: filtrar el dataset completo tarda < 0,25 s", {
  skip_if_coverage()
  skip_if_no_data()
  cfg <- test_cfg()
  occ <- readRDS(cfg$occ_path)
  request <- list(
    level = "genus", values = c("Tillandsia", "Epidendrum"), text = "alba",
    countries = c("CO", "EC")
  )
  f <- sanitize_filters(request, occ, cfg)
  expect_lt(elapsed(for (i in 1:5) filter_occurrences(occ, f)) / 5, 0.25)
})

test_that("PERF: estadísticas y capas sobre todo el dataset caben en el presupuesto", {
  skip_if_coverage()
  skip_if_no_data()
  occ <- readRDS(occ_path())
  expect_lt(elapsed(species_accumulation(occ$speciesName)), 3)
  expect_lt(elapsed(chao1(occ$speciesName)), 0.5)
  expect_lt(elapsed(cells_to_sf(richness_by_cell(occ))), 3)
  expect_lt(elapsed(quality_summary(occ)), 1)
})

test_that("PERF: el ETL procesa 6.428 registros en < 30 s", {
  skip_if_coverage()
  skip_if_not(file.exists(RAW_DEMO()))
  raw <- utils::read.csv(RAW_DEMO(), colClasses = "character", na.strings = "")
  expect_lt(elapsed(out <- clean_occurrences(raw)), 30)
  expect_equal(nrow(out), nrow(raw))
})
