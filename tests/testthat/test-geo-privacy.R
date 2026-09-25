test_that("la generalización queda dentro de la celda y es determinista", {
  lat <- c(4.6512, -1.25, 10.9)
  lon <- c(-74.0831, -78.5, -84.2)
  a <- generalize_coords(lat, lon, c("a", "b", "c"), 0.5)
  b <- generalize_coords(lat, lon, c("a", "b", "c"), 0.5)
  expect_identical(a, b)
  expect_true(all(floor(a$lat_gen / 0.5) == floor(lat / 0.5)))
  expect_true(all(floor(a$lon_gen / 0.5) == floor(lon / 0.5)))
})

test_that("la posición mostrada NO depende de la posición real dentro de la celda", {
  a <- generalize_coords(4.51, -74.01, "r1", 0.5)
  b <- generalize_coords(4.99, -74.49, "r1", 0.5)
  expect_equal(a$lat_gen, b$lat_gen)
  expect_equal(a$lon_gen, b$lon_gen)
  expect_true(abs(a$lat_gen - 4.51) > 0 || abs(a$lon_gen - (-74.01)) > 0)
})

test_that("mayor sensibilidad usa celdas más gruesas y NA se propaga", {
  expect_equal(sensitivity_to_cell(c(0, 1, 2, NA, 9), 0.5), c(0.5, 1, 2, 0.5, 0.5))
  g <- generalize_coords(c(NA, 1), c(NA, 1), c("x", "y"), 0.5)
  expect_true(is.na(g$lat_gen[1]) && is.na(g$cell_id[1]))
  expect_false(is.na(g$cell_id[2]))
})

test_that("assert_no_raw_coordinates detecta columnas de coordenadas exactas", {
  expect_error(assert_no_raw_coordinates(data.frame(decimalLatitude = 1)), "coordenadas exactas")
  expect_error(assert_no_raw_coordinates(data.frame(Lon = 1)), "coordenadas exactas")
  expect_silent(assert_no_raw_coordinates(data.frame(lat_gen = 1, lon_gen = 1)))
})

test_that("cells_to_sf construye celdas cuadradas en EPSG:4326", {
  cells <- richness_by_cell(data.frame(
    cell_id = c("0.5_1_1", "0.5_1_1"), cell_deg = 0.5, cell_ix = 1L, cell_iy = 1L,
    speciesName = c("a b", "a c")
  ))
  s <- cells_to_sf(cells)
  expect_equal(sf::st_crs(s)$epsg, 4326L)
  expect_equal(cells$n_species, 2L)
  expect_equal(as.numeric(sf::st_bbox(s)), c(0.5, 0.5, 1, 1))
})
