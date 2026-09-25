# Listado y ficha de especies (Modelo puro) + módulo de la Vista.

sp_df <- function() {
  data.frame(
    speciesName = c("Aa bb", "Aa bb", "Cc dd", NA), family = c("F1", "F1", "F2", "F3"),
    genus = c("Aa", "Aa", "Cc", "Zz"), order = "O", countryCode = c("CO", "EC", "CO", "CO"),
    cell_id = c("c1", "c2", NA, "c3"), elev_m = c(100, 900, NA, 5),
    year = c(1990L, 2010L, NA, 2000L),
    institutionCode = c("I1", "I2", "I1", "I3"), quality_flags = c("", "x", "", ""),
    stringsAsFactors = FALSE
  )
}

test_that("species_list agrupa, ordena y omite registros sin especie", {
  l <- species_list(sp_df())
  expect_equal(l$speciesName, c("Aa bb", "Cc dd"))
  expect_equal(l$n_records, c(2L, 1L))
  expect_equal(nrow(species_list(sp_df()[0, ])), 0)
})

test_that("species_profile resume una especie y devuelve NULL si no existe", {
  p <- species_profile(sp_df(), "Aa bb")
  expect_equal(p$n_records, 2L)
  expect_equal(p$n_cells, 2L)
  expect_equal(p$elev_range, c(100, 900))
  expect_equal(p$n_flags, 1L)
  expect_null(species_profile(sp_df(), "Nada nada"))
  expect_null(species_profile(sp_df(), "Cc dd")$elev_range)
})

test_that("la ficha no expone coordenadas", {
  expect_false(any(grepl("lat|lon", names(species_profile(sp_df(), "Aa bb")), ignore.case = TRUE)))
})

test_that("mod_species_server muestra la ficha de la fila elegida", {
  st <- reactiveVal(species_list(sp_df()))
  testServer(mod_species_server, args = list(
    species_table = st, profile = function(n) species_profile(sp_df(), n)
  ), {
    session$setInputs(tbl_rows_selected = 1)
    expect_match(as.character(output$profile$html), "Aa bb")
    session$setInputs(tbl_rows_selected = NULL)
    expect_match(as.character(output$profile$html), "Seleccione")
  })
})

test_that("range_text formatea rangos, valores únicos y ausencia de dato", {
  expect_equal(range_text(c(1981L, 1993L), fmt = as.character), "1981–1993")
  expect_equal(range_text(c(5, 5), " m"), "5 m")
  expect_equal(range_text(NULL), "sin dato")
})
