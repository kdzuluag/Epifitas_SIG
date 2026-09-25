small_occ <- function() clean_occurrences(mini_raw())
square <- function() {
  ring <- matrix(c(-80, 0, -70, 0, -70, 10, -80, 10, -80, 0), ncol = 2, byrow = TRUE)
  sf::st_sf(name = "x", geometry = sf::st_sfc(sf::st_polygon(list(ring)), crs = 4326))
}

test_that("utilidades de formato y de texto", {
  expect_equal(fmt_int(1234567), "1.234.567")
  expect_equal(fmt_pct(0.1234), "12,3 %")
  expect_equal(norm_text("Ñandú ÁÉ"), "nandu ae")
  expect_equal(NULL %||% "x", "x")
  expect_equal(NA %||% "x", "x")
  expect_equal(c(1, 2) %||% 0, c(1, 2))
  expect_equal(country_name(c("CO", "ZZ")), c("Colombia", "ZZ"))
})

test_that("estadísticas por década, piso, país y calidad sobre un conjunto pequeño", {
  df <- small_occ()
  expect_equal(by_decade(df)$decade, c(1980L, 1990L, 2010L))
  expect_equal(by_elevation(df)$n, c(1L, 2L, 1L, 1L))
  expect_equal(by_country(df)$country[1], "Colombia")
  q <- quality_summary(df)
  expect_gt(q$n_with_flags, 0)
  expect_length(q$completeness$pct, 4)
  ov <- summarise_overview(df)
  expect_equal(ov$n_records, 6L)
  expect_equal(ov$pct_georef, 2 / 6)
})

test_that("las estadísticas manejan conjuntos vacíos sin fallar", {
  df <- small_occ()[0, ]
  expect_null(species_accumulation("a"))
  expect_equal(chao1(character(0))$observed, 0L)
  expect_equal(nrow(count_by_level(df, "family")), 0)
  expect_equal(attr(count_by_level(df, "family"), "metric"), "records")
  expect_equal(nrow(by_decade(df)), 0)
  expect_equal(nrow(by_country(df)), 0)
  expect_true(is.na(summarise_overview(df)$pct_georef))
  expect_null(default_filters(df)$years)
})

test_that("filtros por años y resumen de filtros activos", {
  df <- small_occ()
  cfg <- test_cfg()
  f <- sanitize_filters(list(years = c(1990, 2000)), df, cfg)
  expect_equal(nrow(filter_occurrences(df, f)), 1)
  many <- sanitize_filters(list(
    level = "genus", values = "Epidendrum", text = "alba", elev_bands = ELEV_LABELS[1],
    countries = "CO", years = c(1990, 2000), only_georef = TRUE
  ), df, cfg)
  labels <- filter_summary(many)
  expect_length(labels, 6)
  expect_true(any(grepl("Género: 1 seleccionado", labels)))
  expect_equal(filter_summary(sanitize_filters(list(), df, cfg)), character(0))
})

test_that("las opciones del selector taxonómico llevan el conteo", {
  choices <- taxon_choices(small_occ(), "family")
  expect_equal(names(choices)[1], "Orchidaceae (5)")
  expect_equal(unname(choices)[1], "Orchidaceae")
})

test_that("SIG: etiqueta de celda, máscara biorregional y simplificación", {
  expect_equal(cell_label(0.5, 1L, 2L), "1.0°N a 1.5°N, 0.5°E a 1.0°E (celda de 0.5°)")
  shape <- square()
  mask <- build_bioregion_mask(shape)
  expect_s3_class(mask, "sf")
  inside <- sf::st_sfc(sf::st_point(c(-75, 5)), crs = 4326)
  outside <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)
  touches <- function(p) with_planar_geometry(function() lengths(sf::st_intersects(mask, p)))
  expect_equal(touches(inside), 0L)
  expect_equal(touches(outside), 1L)
  simple <- simplify_shape(shape)
  expect_equal(sf::st_crs(simple)$epsg, 4326L)
  expect_equal(simple$name, "Neotrópico")
  two <- sf::st_sf(geometry = sf::st_sfc(
    sf::st_polygon(list(rbind(c(-80, 0), c(-70, 0), c(-70, 10), c(-80, 10), c(-80, 0)))),
    sf::st_polygon(list(rbind(c(-70, 0), c(-60, 0), c(-60, 10), c(-70, 10), c(-70, 0)))),
    crs = 4326
  ))
  expect_equal(nrow(simplify_shape(two)), 1L) # varias unidades se disuelven en un solo polígono
})

test_that("load_repository falla con un mensaje claro si faltan los datos procesados", {
  cfg <- test_cfg()
  cfg$occ_path <- file.path(tempdir(), "no-existe.rds")
  expect_error(load_repository(cfg), "Faltan datos procesados")
})

test_that("el ETL marca datos sintéticos y conserva atributos de construcción", {
  out <- small_occ()
  expect_false(attr(out, "demo"))
  expect_equal(attr(out, "grid_deg"), 0.5)
  raw <- mini_raw()
  raw$datasetID <- "DEMO_X"
  expect_true(attr(clean_occurrences(raw), "demo"))
})
