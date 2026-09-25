# Contrato de interfaz (accesibilidad y requisitos visibles) sobre el HTML generado por main_ui().

ui_html <- function() {
  cfg <- test_cfg()
  repo <- load_repository(cfg)
  as.character(htmltools::renderTags(main_ui(cfg, repo))$html)
}

# Valores de un atributo (id o for) capturados por una expresión regular.
capture_ids <- function(html, pattern, attribute_regex) {
  found <- unlist(regmatches(html, gregexpr(pattern, html)))
  sub(attribute_regex, "\\1", found)
}

test_that("A11Y: el idioma de la página es español", {
  ui_file <- file.path(root, "R", "view", "ui_main.R")
  src <- paste(readLines(ui_file, encoding = "UTF-8"), collapse = "\n")
  expect_match(src, "lang\\s*=\\s*\"es\"")
})

test_that("A11Y: todo control de formulario tiene etiqueta asociada", {
  skip_if_no_data()
  html <- ui_html()
  controls <- unlist(regmatches(
    html, gregexpr("<(input|select|textarea)[^>]*\\sid=\"[^\"]+\"[^>]*>", html)
  ))
  controls <- controls[!grepl("type=\"(hidden|button|submit)\"", controls)]
  ids <- sub(".*\\sid=\"([^\"]+)\".*", "\\1", controls)
  labelled <- capture_ids(html, "for=\"[^\"]+\"", "for=\"([^\"]+)\"")
  # una etiqueta que envuelve al control también lo etiqueta (checkbox de Shiny)
  wrapped <- capture_ids(
    html, "<label[^>]*>\\s*<input[^>]*\\sid=\"[^\"]+\"", ".*\\sid=\"([^\"]+)\".*"
  )
  missing_label <- setdiff(ids, c(labelled, wrapped))
  # los grupos radio/checkbox etiquetan por opción; el buscador de DT se genera en el cliente
  missing_label <- missing_label[!grepl("-(level|elev)$|-tbl", missing_label)]
  expect_length(missing_label, 0)
  if (length(missing_label)) {
    fail(paste("Controles sin etiqueta:", paste(missing_label, collapse = ", ")))
  }
})

test_that("A11Y: cada gráfico renderPlot declara texto alternativo (alt)", {
  code <- read_code(file.path(root, "R", "view", c("mod_dashboard.R", "mod_analysis.R")))
  n_plots <- sum(grepl("renderPlot\\(", code$text))
  n_alt <- sum(grepl("\\balt\\s*=", code$text))
  expect_gte(n_plots, 6)
  expect_equal(n_alt, n_plots)
})

test_that("REQ: la interfaz no ofrece descargas y expone las cinco secciones", {
  skip_if_no_data()
  html <- ui_html()
  expect_false(grepl("download|\\.csv|\\.xlsx", html, ignore.case = TRUE))
  for (s in c("Resumen", "Especies", "Análisis", "Mapa", "Tabla", "Acerca de")) {
    expect_true(grepl(s, html, fixed = TRUE), info = s)
  }
  expect_true(grepl("Aplicar filtros", html, fixed = TRUE))
  expect_true(grepl("Restablecer", html, fixed = TRUE))
})

test_that("REQ: ambos gráficos ofrecen el selector de nivel taxonómico independiente", {
  skip_if_no_data()
  html <- ui_html()
  for (id in c("dash-level_a", "dash-level_b")) {
    expect_true(grepl(id, html, fixed = TRUE), info = id)
  }
  expect_equal(unname(TAX_LEVELS), c("order", "family", "genus", "speciesName"))
})
