small_occ <- function() clean_occurrences(mini_raw())

test_that("collapse_levels agrupa lo poco frecuente en 'Otros' y respeta los NA", {
  f <- collapse_levels(c(rep("a", 5), rep("b", 3), "c", NA), max_levels = 2)
  expect_equal(levels(f), c("a", "b", "Otros"))
  expect_equal(as.character(f[9:10]), c("Otros", "Otros"))
})

test_that("category_colors usa paleta apta para daltonismo y gris para 'Otros'", {
  cols <- category_colors(c("a", "b", "Otros"))
  expect_equal(names(cols), c("a", "b", "Otros"))
  expect_equal(unname(cols[["a"]]), PALETTE_CAT[1])
  expect_equal(unname(cols[["Otros"]]), OTHER_COLOR)
})

test_that("los constructores de gráficos devuelven ggplot renderizable, también sin datos", {
  df <- small_occ()
  plots <- list(
    bar_plot(count_by_level(df, "genus"), "genus", "Registros"),
    bar_plot(count_by_level(df[0, ], "genus"), "genus", "Registros"),
    plot_accumulation(species_accumulation(rep(c("a", "b", "c"), 4))),
    plot_accumulation(NULL),
    plot_elevation(by_elevation(df)),
    plot_decades(by_decade(df)),
    plot_decades(by_decade(df[0, ])),
    plot_countries(by_country(df)),
    plot_countries(by_country(df[0, ])),
    plot_completeness(quality_summary(df)$completeness),
    empty_plot("sin datos")
  )
  for (p in plots) {
    expect_true(ggplot2::is_ggplot(p))
    expect_no_error(ggplot2::ggplot_build(p))
  }
})

test_that("la ficha del registro no expone coordenadas y distingue la ubicación", {
  df <- small_occ()
  with_coords <- as.character(record_info_ui(df[1, ]))
  expect_match(with_coords, "Epidendrum alba")
  expect_match(with_coords, "Ubicación aproximada")
  expect_false(grepl("decimal|lat_gen|lon_gen", with_coords))
  expect_match(as.character(record_info_ui(df[2, ])), "Sin coordenadas utilizables")
})

test_that("el campo de detalle limita la longitud también en el navegador", {
  html <- as.character(location_fields_ui(NS("x"), character(0), 500))
  expect_match(html, "maxlength=\"500\"")
})

test_that("el formulario de verificación no ofrece una respuesta marcada por defecto", {
  html <- as.character(well_placed_question_ui(NS("x"), step_wp = NULL))
  expect_match(html, "¿Considera que este registro está bien ubicado")
  expect_false(grepl("btn-success|btn-danger", html)) # ninguna opción activa antes de elegir
  expect_match(as.character(well_placed_question_ui(NS("x"), "si")), "btn-success")
})

test_that("los desplegables de departamento y región caen a texto libre sin datos", {
  con_datos <- as.character(location_fields_ui(NS("x"), c("Antioquia", "Chocó"), 200))
  expect_match(con_datos, "<select")
  sin_datos <- as.character(location_fields_ui(NS("x"), character(0), 200))
  expect_false(grepl("<select", sin_datos))
  expect_match(sin_datos, "escríbalo|escríbala")
  expect_match(as.character(region_field_ui(NS("x"), c("Chocó-Darién province"))), "<select")
  expect_false(grepl("<select", as.character(region_field_ui(NS("x"), character(0)))))
})

test_that("insignias, campos y avisos de la interfaz", {
  expect_null(flag_badges(""))
  expect_match(as.character(flag_badges("pais_invalido")), "Código de país no válido")
  expect_null(detail_field("Campo", NA))
  expect_match(as.character(detail_field("Campo", "valor")), "valor")
  expect_match(as.character(demo_banner_ui(list(meta = list(demo = TRUE)))), "SINTÉTICOS")
  expect_null(demo_banner_ui(list(meta = list(demo = FALSE))))
  expect_match(as.character(filter_pills_ui(c("a", "b"), 1234)), "1.234 registros")
})

test_that("la tabla visible nunca incluye coordenadas y añade el conteo de verificaciones", {
  df <- small_occ()
  counts <- data.frame(record_key = df$record_key[1], n = 3L)
  tv <- table_view(df, counts)
  expect_false(any(c("decimalLatitude", "decimalLongitude", "lat_gen", "lon_gen") %in% names(tv)))
  expect_equal(tv$Verificaciones, c(3L, rep(0L, 5)))
  expect_equal(tv$quality_flags[1], "")
})

test_that("las capas del mapa se construyen sin coordenadas exactas y respetan la visibilidad", {
  skip_if_no_data()
  repo <- load_repository(test_cfg())
  base <- map_base(repo)
  calls <- vapply(base$x$calls, function(cl) cl$method, "")
  expect_true(all(c("addPolygons", "addLayersControl", "hideGroup") %in% calls))

  pts <- repo$occ[repo$occ$has_coords, MAP_COLS]
  drawn <- draw_points(leaflet::leaflet(), pts, "family", TRUE)
  methods <- vapply(drawn$x$calls, function(cl) cl$method, "")
  expect_true(all(c("addCircleMarkers", "addLegend") %in% methods))
  none <- draw_points(leaflet::leaflet(), pts[0, ], "family", FALSE)
  expect_false("addCircleMarkers" %in% vapply(none$x$calls, function(cl) cl$method, ""))

  cells <- cells_to_sf(richness_by_cell(repo$occ))
  hidden <- vapply(draw_cells(leaflet::leaflet(), cells, FALSE)$x$calls, function(cl) cl$method, "")
  shown <- vapply(draw_cells(leaflet::leaflet(), cells, TRUE)$x$calls, function(cl) cl$method, "")
  expect_true("hideGroup" %in% hidden)
  expect_false("hideGroup" %in% shown)
  legend_on <- draw_cells_legend(leaflet::leaflet(), cells, TRUE)
  legend_off <- draw_cells_legend(leaflet::leaflet(), cells, FALSE)
  expect_true("addLegend" %in% vapply(legend_on$x$calls, function(cl) cl$method, ""))
  expect_false("addLegend" %in% vapply(legend_off$x$calls, function(cl) cl$method, ""))
})
