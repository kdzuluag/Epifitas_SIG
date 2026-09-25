test_that("los datos procesados NO contienen las coordenadas exactas del archivo crudo", {
  skip_if_not(file.exists(RAW_DEMO()) && file.exists(occ_path()), "ejecute scripts/00 y 01 primero")

  raw <- utils::read.csv(RAW_DEMO(), colClasses = "character", na.strings = "")
  occ <- readRDS(occ_path())
  assert_no_raw_coordinates(occ)

  # Unir por clave: la posición mostrada debe estar en la celda real, pero nunca ser igual a ella.
  ids <- ifelse(is.na(raw$recordID), paste0("fila", seq_len(nrow(raw))), raw$recordID)
  raw$key <- make.unique(ids, sep = "#")
  shown <- occ[occ$has_coords, c("record_key", "lat_gen", "lon_gen", "cell_deg")]
  m <- merge(shown, raw[, c("key", "decimalLatitude", "decimalLongitude")],
    by.x = "record_key", by.y = "key"
  )
  lat <- as.numeric(m$decimalLatitude)
  lon <- as.numeric(m$decimalLongitude)
  expect_gt(nrow(m), 600)
  expect_true(all(floor(m$lat_gen / m$cell_deg) == floor(lat / m$cell_deg))) # misma celda
  expect_true(all(floor(m$lon_gen / m$cell_deg) == floor(lon / m$cell_deg)))
  same_point <- abs(m$lat_gen - lat) < 1e-6 & abs(m$lon_gen - lon) < 1e-6
  expect_false(any(same_point)) # ninguna coincide con la real
  # el error de posición debe ser sustancial (mediana > 5 km ≈ 0.045°)
  expect_gt(stats::median(sqrt((m$lat_gen - lat)^2 + (m$lon_gen - lon)^2)), 0.045)
})

test_that("el archivo procesado no tiene columnas de coordenadas exactas y es liviano", {
  skip_if_no_data()
  occ <- readRDS(occ_path())
  expect_false(any(c("decimalLatitude", "decimalLongitude") %in% names(occ)))
  expect_lt(file.size(occ_path()) / 1024^2, 5)
})
