# Modelo puro de la verificación de ubicación: referencia geográfica y validación del formulario.

test_that("departments_for_country filtra por país y no falla sin datos", {
  admin1 <- data.frame(
    countryCode = c("CO", "CO", "PE"), department = c("Antioquia", "Chocó", "Lima")
  )
  expect_equal(departments_for_country(admin1, "CO"), c("Antioquia", "Chocó"))
  expect_equal(departments_for_country(admin1, "BR"), character(0))
  expect_equal(departments_for_country(admin1, NA), character(0))
  expect_equal(departments_for_country(NULL, "CO"), character(0))
})

test_that("extract_neotropical_regions lee la columna de provincia (o cae con elegancia)", {
  con_provincias <- sf::st_sf(
    Province_1 = c("Chocó-Darién province", "Cauca province", "Cauca province", NA),
    geometry = sf::st_sfc(rep(list(sf::st_point(c(0, 0))), 4), crs = 4326)
  )
  expect_equal(
    extract_neotropical_regions(con_provincias), c("Cauca province", "Chocó-Darién province")
  )
  pt <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)
  sin_provincias <- sf::st_sf(name = "DEMO", geometry = pt)
  expect_equal(extract_neotropical_regions(sin_provincias), character(0))
})

verdict_cfg <- function() list(identity_max_chars = 150, detail_max_chars = 500)

test_that("build_verdict acepta 'bien ubicado' solo con nombre y profesión", {
  ok <- build_verdict(
    list(well_placed = TRUE, reviewer_name = "Ana", reviewer_profession = "Bióloga"), verdict_cfg()
  )
  expect_true(ok$ok)
  expect_true(ok$verdict$well_placed)
  expect_null(ok$verdict$issue_type)

  sin_nombre <- build_verdict(
    list(well_placed = TRUE, reviewer_name = "  ", reviewer_profession = "Bióloga"), verdict_cfg()
  )
  expect_false(sin_nombre$ok)
})

test_that("build_verdict exige qué está incorrecto cuando no está bien ubicado", {
  base <- list(well_placed = FALSE, reviewer_name = "Ana", reviewer_profession = "Bióloga")
  sin_tipo <- build_verdict(base, verdict_cfg())
  expect_false(sin_tipo$ok)
  tipo_invalido <- build_verdict(utils::modifyList(base, list(issue_type = "otro")), verdict_cfg())
  expect_false(tipo_invalido$ok)
})

test_that("build_verdict: la corrección de ubicación exige departamento y municipio", {
  base <- list(
    well_placed = FALSE, issue_type = "ubicacion", reviewer_name = "Ana",
    reviewer_profession = "Bióloga"
  )
  expect_false(build_verdict(base, verdict_cfg())$ok)
  con_depto <- utils::modifyList(base, list(department = "Antioquia"))
  expect_false(build_verdict(con_depto, verdict_cfg())$ok) # falta municipalidad
  detalle <- list(municipality = "Medellín", detail = "cerca del río")
  completo <- utils::modifyList(con_depto, detalle)
  res <- build_verdict(completo, verdict_cfg())
  expect_true(res$ok)
  expect_equal(res$verdict$department, "Antioquia")
  expect_equal(res$verdict$municipality, "Medellín")
  expect_equal(res$verdict$detail, "cerca del río")
  expect_null(res$verdict$neotropical_region)
})

test_that("build_verdict: el detalle es opcional y se sanea (sin inyección ni control)", {
  base <- list(
    well_placed = FALSE, issue_type = "ubicacion", reviewer_name = "Ana",
    reviewer_profession = "Bióloga", department = "Antioquia", municipality = "Medellín"
  )
  sin_detalle <- build_verdict(base, verdict_cfg())
  expect_true(sin_detalle$ok)
  expect_null(sin_detalle$verdict$detail)
  con_control <- build_verdict(
    utils::modifyList(base, list(detail = "x\u0007y'); DROP TABLE verifications;--")), verdict_cfg()
  )
  expect_true(con_control$ok)
  expect_false(grepl("\u0007", con_control$verdict$detail))
})

test_that("build_verdict: la corrección de región exige la región neotrópica", {
  base <- list(
    well_placed = FALSE, issue_type = "region", reviewer_name = "Ana",
    reviewer_profession = "Bióloga"
  )
  expect_false(build_verdict(base, verdict_cfg())$ok)
  ok <- build_verdict(
    utils::modifyList(base, list(neotropical_region = "Chocó-Darién province")), verdict_cfg()
  )
  expect_true(ok$ok)
  expect_equal(ok$verdict$neotropical_region, "Chocó-Darién province")
  expect_null(ok$verdict$department)
})
