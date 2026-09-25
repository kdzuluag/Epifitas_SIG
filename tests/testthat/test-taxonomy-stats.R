occ <- function() {
  out <- clean_occurrences(mini_raw())
  out
}
cfg <- epig_config(env = function(k, d = "") d)

test_that("la búsqueda de texto ignora tildes y mayúsculas, y exige todos los términos", {
  o <- occ()
  f <- sanitize_filters(list(text = "EPIDENDRUM Alba"), o, cfg)
  expect_equal(nrow(filter_occurrences(o, f)), 1)
  f2 <- sanitize_filters(list(text = "epidendrum"), o, cfg)
  expect_gte(nrow(filter_occurrences(o, f2)), 3)
})

test_that("sanitize_filters descarta valores ilegítimos y limita tamaños", {
  o <- occ()
  f <- sanitize_filters(list(
    level = "'; DROP", values = as.character(1:1000), text = strrep("a ", 500),
    elev_bands = c("x", ELEV_LABELS[1]), countries = c("co", "ZZ"), years = c("2000", "1990")
  ), o, cfg)
  expect_equal(f$level, "family")
  expect_length(f$values, cfg$max_filter_values)
  expect_lte(length(f$tokens), cfg$max_search_tokens)
  expect_equal(f$elev_bands, ELEV_LABELS[1])
  expect_equal(f$countries, "CO")
  expect_equal(f$years, c(1990L, 2000L))
})

test_that("los filtros taxonómico, geográfico y de georreferencia funcionan", {
  o <- occ()
  f <- sanitize_filters(list(level = "family", values = "Bromeliaceae"), o, cfg)
  expect_equal(nrow(filter_occurrences(o, f)), 1)
  f <- sanitize_filters(list(only_georef = TRUE), o, cfg)
  expect_equal(nrow(filter_occurrences(o, f)), 2)
  f <- sanitize_filters(list(countries = "EC"), o, cfg)
  expect_equal(nrow(filter_occurrences(o, f)), 1)
})

test_that("chao1 reproduce un cálculo manual", {
  sp <- c(rep("a", 5), rep("b", 2), "c", "d", "e") # S=5, f1=3, f2=1
  r <- chao1(sp)
  expect_equal(r$observed, 5L)
  expect_equal(r$estimated, 5 + 3 * 2 / (2 * 2))
  expect_equal(r$completeness, 5 / 6.5)
})

test_that("la curva de acumulación es monótona, reproducible y termina en la riqueza observada", {
  sp <- rep(paste0("s", 1:20), times = c(rep(1, 10), rep(5, 10)))
  a <- species_accumulation(sp, n_perm = 10, seed = 7)
  b <- species_accumulation(sp, n_perm = 10, seed = 7)
  expect_identical(a, b)
  expect_true(all(diff(a$mean) >= 0))
  expect_equal(tail(a$mean, 1), 20)
})

test_that("count_by_level cae a registros cuando la métrica no aplica a especie", {
  o <- occ()
  r <- count_by_level(o, "speciesName", "species")
  expect_equal(attr(r, "metric"), "records")
  r2 <- count_by_level(o, "genus", "species")
  expect_equal(attr(r2, "metric"), "species")
})

test_that("quality_summary cuenta alertas y completitud", {
  q <- quality_summary(occ())
  expect_true(all(c("flags", "completeness", "n_with_flags") %in% names(q)))
  expect_gt(q$n_with_flags, 0)
})
