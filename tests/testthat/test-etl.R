test_that("parse_elevation entiende los formatos habituales de etiqueta", {
  x <- c("1,200 m", "1200-1500", "c. 800", "2500 m s.n.m.", "1200,5", NA, "sin dato")
  expect_equal(parse_elevation(x), c(1200, 1350, 800, 2500, 1200, NA, NA))
})

test_that("parse_year acepta fechas parciales y descarta las imposibles", {
  dates <- c("2019-05-14", "1999", "14/05/1985", "sin fecha", "2999-01-01", NA)
  expect_equal(parse_year(dates, max_year = 2026L), c(2019L, 1999L, 1985L, NA, NA, NA))
})

test_that("clean_occurrences elimina coordenadas exactas y valida el esquema", {
  out <- clean_occurrences(mini_raw())
  expect_false(any(COORD_COLS %in% names(out)))
  expect_error(clean_occurrences(mini_raw()[, -1]), "Faltan columnas")
})

test_that("clean_occurrences marca coordenadas inválidas/fuera y conserva las válidas", {
  out <- clean_occurrences(mini_raw())
  expect_equal(out$has_coords, c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE))
  expect_match(out$quality_flags[2], "coordenadas_invalidas")
  expect_match(out$quality_flags[5], "coordenadas_fuera_neotropico")
  expect_match(out$quality_flags[6], "coordenadas_invalidas")
})

test_that("clean_occurrences normaliza taxonomía y marca inconsistencias", {
  out <- clean_occurrences(mini_raw())
  expect_equal(out$family[1], "Orchidaceae")
  expect_equal(out$order[1], "Asparagales")
  expect_match(out$quality_flags[3], "genero_no_coincide")
  expect_match(out$quality_flags[6], "pais_invalido")
  expect_match(out$quality_flags[4], "elevacion_atipica")
})

test_that("recordID duplicado genera clave única y alerta", {
  out <- clean_occurrences(mini_raw())
  expect_false(anyDuplicated(out$record_key) > 0)
  expect_match(out$quality_flags[3], "recordID_duplicado")
  expect_match(out$quality_flags[4], "recordID_duplicado")
})

test_that("pisos altitudinales y búsqueda precomputada", {
  out <- clean_occurrences(mini_raw())
  expect_equal(as.character(out$elev_band[1]), "Montano bajo (1000-2000 m)")
  expect_true(grepl("epidendrum alba", out$search_txt[1], fixed = TRUE))
})

test_that("la familia atípica para un género se marca solo en la minoría", {
  raw <- mini_raw()[rep(2, 6), ]
  raw$recordID <- paste0("R", 1:6)
  raw$family[6] <- "Araceae"
  out <- clean_occurrences(raw)
  expect_equal(grepl("genero_multiples_familias", out$quality_flags), c(rep(FALSE, 5), TRUE))
})

test_that("los ayudantes del ETL se comportan en los casos límite", {
  expect_equal(clean_str(c("  a   b ", "", NA)), c("a b", NA, NA))
  expect_equal(cap_first(c("ORCHIDACEAE", NA)), c("Orchidaceae", NA))
  expect_equal(to_num(c("1,5", "x", NA)), c(1.5, NA, NA))
  keys <- make_record_keys(c("A", "A", NA))
  expect_equal(keys$duplicated, c(TRUE, TRUE, FALSE))
  expect_equal(keys$key, c("A", "A#1", "fila3"))
})
