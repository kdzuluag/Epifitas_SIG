# Guardias de regresión: convierten en pruebas automáticas los defectos ya corregidos (04/08/2026)
# y los requisitos de privacidad/arquitectura, inspeccionando el código fuente.

app_code <- function() read_code(r_files("R", "app.R"))

# Ubicaciones "archivo:línea" de las líneas que cumplen `pattern` (opcionalmente solo en `files`).
hits <- function(code, pattern, files = NULL) {
  d <- code[grepl(pattern, code$text, perl = TRUE), ]
  if (!is.null(files)) d <- d[grepl(files, d$file), ]
  if (nrow(d)) paste0(basename(d$file), ":", d$line, collapse = ", ") else character(0)
}

test_that("REQ: sin descargas de datos (ni handlers, ni botones, ni extensión Buttons)", {
  pattern <- "downloadHandler|downloadButton|downloadLink|extensions\\s*=|dom\\s*=\\s*\"[^\"]*B"
  expect_length(hits(app_code(), pattern), 0)
})

test_that("BUG 04/08: no hay install.packages() en el código de la app", {
  expect_length(hits(app_code(), "install\\.packages\\("), 0)
})

test_that("BUG 04/08: no se recarga global.R por sesión ni se usa aes_string() obsoleto", {
  code <- app_code()
  expect_length(hits(code, "source\\(\\s*[\"']global\\.R[\"']"), 0)
  expect_length(hits(code, "aes_string\\("), 0)
})

test_that("BUG 04/08: los datos se cargan una vez por proceso, no por sesión", {
  code <- app_code()
  expect_length(hits(code, "load_repository\\(", "controller|view"), 0)
  expect_length(hits(code, "readRDS\\(", "controller|view"), 0)
  expect_length(hits(code, "^\\s*repo <- load_repository\\(", "app\\.R"), 1)
})

test_that("BUG 04/08: los marcadores solo se dibujan con leafletProxy, nunca en el mapa base", {
  map_file <- file.path(root, "R", "view", "mod_map.R")
  markers <- "addCircleMarkers|addMarkers"
  expect_false(grepl(markers, function_body(map_file, "map_base")))
  expect_true(grepl(markers, function_body(map_file, "draw_points")))
  expect_true(grepl("leafletProxy", paste(readLines(map_file), collapse = "\n")))
})

test_that("REQ privacidad: solo el ETL y el SIG mencionan coordenadas exactas", {
  code <- app_code()
  allowed <- "R[/\\\\]core[/\\\\](schema|config)|R[/\\\\]model[/\\\\](etl|geo|qa)"
  d <- code[grepl("decimalLatitude|decimalLongitude|COORD_COLS", code$text), ]
  offenders <- basename(d$file[!grepl(allowed, d$file)])
  expect_true(all(grepl(allowed, d$file)), info = paste(offenders, collapse = ", "))
  expect_length(hits(code, "decimalLat|decimalLon", "view|controller"), 0)
})

test_that("ARQ: la Vista no contiene lógica de negocio ni acceso a datos/seguridad", {
  business <- paste0(
    "\\bvs_(add|counts|count_one|open)\\(|filter_occurrences\\(|generalize_coords\\(|",
    "resolve_user\\(|sanitize_(text|filters)\\(|audit_log\\(|build_verdict\\("
  )
  expect_length(hits(app_code(), business, "view"), 0)
})

test_that("ARQ: el Modelo es puro (sin shiny, reactivos ni sesión salvo resolve_user)", {
  shiny_use <- paste0(
    "shiny::|\\breactive\\(|\\bobserve(Event)?\\(|\\brender[A-Z]\\w*\\(|",
    "showNotification|\\binput\\$|\\boutput\\$"
  )
  expect_length(hits(app_code(), shiny_use, "model"), 0)
})

test_that("SEG: la Vista nunca inyecta HTML crudo (HTML() con datos abre XSS)", {
  expect_length(hits(app_code(), "\\bHTML\\(", "view"), 0)
})

test_that("SEG: sin secretos ni rutas absolutas codificadas en el código", {
  code <- app_code()
  expect_length(hits(code, "(password|passwd|secret|token)\\s*(<-|=)\\s*[\"'][^\"']+[\"']"), 0)
  expect_length(hits(code, "[A-Za-z]:[/\\\\]Users"), 0)
})

test_that("SEG: el .gitignore protege datos crudos, estado privado y secretos", {
  gi <- readLines(file.path(root, ".gitignore"))
  expect_true(all(c("data/raw/", "data/private/", ".Renviron") %in% gi))
})

test_that("todos los archivos R se analizan sin errores de sintaxis", {
  for (f in r_files(c("R", "scripts", "tests", "deploy"), "app.R")) {
    expect_no_error(parse(f, encoding = "UTF-8"))
  }
})
