small_occ <- function() clean_occurrences(mini_raw())

test_that("mod_filters_server: aplicar emite los filtros y restablecer vuelve a los de defecto", {
  defaults <- default_filters(small_occ())
  choices_fn <- function(level) c("Epidendrum (4)" = "Epidendrum")
  testServer(mod_filters_server,
    args = list(choices_fn = choices_fn, defaults = defaults),
    {
      expect_equal(session$returned()$level, "family") # antes de aplicar: defecto
      session$setInputs(
        level = "genus", values = "Epidendrum", text = "alba", elev = ELEV_LABELS[1],
        countries = "CO", years = c(1990, 2000), georef = TRUE, apply = 1
      )
      applied <- session$returned()
      expect_equal(applied$level, "genus")
      expect_equal(applied$values, "Epidendrum")
      expect_true(applied$only_georef)
      session$setInputs(reset = 1)
      expect_equal(session$returned(), defaults)
    }
  )
})

test_that("mod_dashboard_server: indicadores formateados y gráficos con métrica efectiva", {
  overview <- reactive(list(
    n_records = 6428L, n_species = 2095L, n_localities = 84L, n_no_coords = 5724L, pct_georef = 0.11
  ))
  chart_data <- function(level, metric) {
    structure(data.frame(taxon = c("A", "B"), n = c(3L, 1L)), metric = metric)
  }
  testServer(mod_dashboard_server,
    args = list(overview = overview, chart_data = chart_data),
    {
      session$setInputs(
        level_a = "family", metric_a = "records", level_b = "genus", metric_b = "species"
      )
      expect_equal(output$v_records, "6.428")
      expect_equal(output$v_species, "2.095")
      expect_equal(output$v_nocoords, "5.724")
      expect_match(output$v_georef, "11,0 %")
    }
  )
})

# Controlador de verificación de mentira para las pruebas de la Vista.
fake_verification <- function(can_verify = TRUE, submit_result = TRUE, regions = character(0),
                              departments = character(0)) {
  log <- new.env()
  log$calls <- list()
  list(
    can_verify = reactive(can_verify),
    departments_for = function(cc) departments,
    regions = regions,
    count = function(key) 0L,
    submit = function(payload) {
      log$calls[[length(log$calls) + 1]] <- payload
      submit_result
    },
    log = log
  )
}

test_that("mod_detail_server: placeholder y ficha del registro seleccionado", {
  df <- small_occ()
  record <- reactiveVal(NULL)
  testServer(mod_detail_server,
    args = list(record = record, verification = fake_verification(), max_chars = 100),
    {
      expect_match(output$info$html, "Seleccione un punto")
      expect_true(is.null(output$verify) || !grepl("bien ubicado", output$verify$html))
      record(df[1, ])
      session$flushReact()
      expect_match(output$info$html, "Epidendrum alba")
      expect_match(output$verify$html, "¿Considera que este registro está bien ubicado")
    }
  )
})

test_that("mod_detail_server: el rol de solo lectura no ve el formulario", {
  record <- reactiveVal(small_occ()[1, ])
  ver <- fake_verification(can_verify = FALSE)
  testServer(mod_detail_server,
    args = list(record = record, verification = ver, max_chars = 100),
    {
      expect_match(output$verify$html, "solo lectura")
      expect_false(grepl("bien ubicado", output$verify$html))
    }
  )
})

test_that("mod_detail_server: 'Sí' pide identidad; 'No' despliega el menú y la corrección", {
  record <- reactiveVal(small_occ()[1, ])
  ver <- fake_verification()
  testServer(mod_detail_server, args = list(record = record, verification = ver, max_chars = 100), {
    session$setInputs(wp_si = 1)
    expect_match(output$verify$html, "Su nombre")
    expect_false(grepl("incorrecto", output$verify$html))

    session$setInputs(wp_no = 1)
    expect_match(output$verify$html, "incorrecto")
    expect_false(grepl("Su nombre", output$verify$html)) # aún no eligió qué está incorrecto

    session$setInputs(issue_ubicacion = 1)
    expect_match(output$verify$html, "Departamento")
    expect_match(output$verify$html, "Municipalidad")
    expect_match(output$verify$html, "Su nombre")

    session$setInputs(
      department = "Antioquia", municipality = "Medellín", reviewer_name = "Ana",
      reviewer_profession = "Bióloga", verify_submit = 1
    )
    sent <- ver$log$calls[[1]]
    expect_false(sent$well_placed)
    expect_equal(sent$issue_type, "ubicacion")
    expect_equal(sent$department, "Antioquia")
    expect_equal(sent$reviewer_name, "Ana")
    # tras el envío el formulario vuelve a la pregunta inicial
    expect_false(grepl("Departamento", output$verify$html))
  })
})

test_that("mod_detail_server: cambiar de registro reinicia el formulario", {
  df <- small_occ()
  record <- reactiveVal(df[1, ])
  ver <- fake_verification()
  testServer(mod_detail_server, args = list(record = record, verification = ver, max_chars = 100), {
    session$setInputs(wp_no = 1, issue_region = 1)
    expect_match(output$verify$html, "Región del Neotrópico")
    record(df[2, ])
    session$flushReact()
    expect_false(grepl("Región del Neotrópico", output$verify$html))
  })
})

test_that("mod_table_server: seleccionar una fila emite la clave del registro", {
  df <- small_occ()
  picked <- new.env()
  picked$key <- NULL
  testServer(mod_table_server,
    args = list(
      table_df = reactive(df),
      counts = reactive(data.frame(record_key = character(0), n = integer(0))),
      on_select = function(k) picked$key <- k
    ),
    {
      session$setInputs(tbl_rows_selected = 3)
      expect_equal(picked$key, df$record_key[3])
    }
  )
})

test_that("mod_analysis_server: Chao1 y tabla de alertas con los datos filtrados", {
  df <- small_occ()
  analysis <- list(
    accum = reactive(species_accumulation(rep(c("a", "b", "c"), 4))),
    chao = reactive(chao1(c(rep("a", 3), "b", "c"))),
    elevation = reactive(by_elevation(df)), decade = reactive(by_decade(df)),
    country = reactive(by_country(df)), quality = reactive(quality_summary(df))
  )
  testServer(mod_analysis_server, args = list(a = analysis), {
    expect_match(output$chao$html, "Estimada \\(Chao1\\)")
    expect_match(output$chao$html, "Completitud")
  })
  no_data <- modifyList(analysis, list(chao = reactive(chao1(character(0)))))
  testServer(mod_analysis_server, args = list(a = no_data), {
    expect_match(output$chao$html, "Sin datos")
  })
})

test_that("main_server: con acceso muestra el aviso de demostración y los filtros activos", {
  skip_if_no_data()
  cfg <- test_cfg()
  repo <- load_repository(cfg)
  testServer(function(input, output, session) main_server(input, output, session, cfg, repo), {
    expect_match(output$filter_pills$html, "registros")
    if (isTRUE(repo$meta$demo)) expect_match(output$demo_banner$html, "SINTÉTICOS")
  })
})
