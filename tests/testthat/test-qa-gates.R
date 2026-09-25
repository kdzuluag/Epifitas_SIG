occ_real <- function() readRDS(occ_path())

test_that("las compuertas críticas de datos pasan con los datos procesados actuales", {
  skip_if_no_data()
  res <- qa_data_gates(occ_real(), rds_path = occ_path())
  failed <- res$id[!res$ok & res$severidad == "critico"]
  expect_true(qa_gate_passed(res), info = paste(failed, collapse = ", "))
  expect_true(all(c("Q01", "Q17") %in% res$id))
})

test_that("las compuertas DETECTAN datos defectuosos (se prueba la prueba)", {
  skip_if_no_data()
  ok <- occ_real()
  failing <- function(d) {
    res <- qa_data_gates(d)
    res$id[!res$ok]
  }

  bad <- ok
  bad$decimalLatitude <- 1
  expect_true("Q02" %in% failing(bad)) # coordenada exacta filtrada

  bad <- ok
  bad$record_key[2] <- bad$record_key[1]
  expect_true("Q04" %in% failing(bad)) # clave duplicada

  bad <- ok
  i <- which(bad$has_coords)[1]
  bad$lat_gen[i] <- bad$lat_gen[i] + 5
  expect_true("Q06" %in% failing(bad)) # punto fuera de su celda

  bad <- ok
  i <- which(bad$has_coords & bad$sensitivityLevel == 1L)[1]
  bad$cell_deg[i] <- 0.5
  expect_true(any(c("Q06", "Q08") %in% failing(bad))) # sensible con celda fina

  bad <- ok
  bad$year[1] <- 1500L
  expect_true("Q10" %in% failing(bad)) # año imposible

  bad <- ok[0, ]
  expect_true("Q01" %in% failing(bad)) # sin registros
})

test_that("faltar columnas obligatorias detiene las demás compuertas", {
  skip_if_no_data()
  bad <- occ_real()
  bad$search_txt <- NULL
  res <- qa_data_gates(bad)
  expect_equal(res$id, c("Q01", "Q02", "Q03"))
  expect_false(qa_gate_passed(res))
})

test_that("las advertencias no bloquean pero quedan registradas", {
  skip_if_no_data()
  d <- occ_real()
  d$quality_flags <- "pais_invalido" # 100 % con alertas
  res <- qa_data_gates(d)
  expect_false(res$ok[res$id == "Q12"])
  expect_true(qa_gate_passed(res))
})
