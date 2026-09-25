test_that("data_controller: filtra, cuenta y no expone coordenadas exactas al mapa", {
  skip_if_no_data()
  cfg <- test_cfg()
  repo <- load_repository(cfg)
  testServer(function(id) {
    moduleServer(id, function(input, output, session) {
      flt <- reactiveVal(default_filters(repo$occ))
      list(flt = flt, dc = data_controller(cfg, repo, flt, "tester"))
    })
  }, {
    r <- session$returned
    base <- default_filters(repo$occ)
    pts <- r$dc$map_points()
    expect_equal(r$dc$overview()$n_records, nrow(repo$occ))
    expect_false(any(c("decimalLatitude", "decimalLongitude") %in% names(pts)))
    expect_true(all(pts$lat_gen >= -70 & pts$lat_gen <= 40))

    r$flt(modifyList(base, list(text = "TILLANDSIA")))
    session$flushReact()
    n_t <- r$dc$overview()$n_records
    expect_lt(n_t, nrow(repo$occ))
    expect_gt(n_t, 0)
    expect_true(all(grepl("tillandsia", r$dc$filtered()$search_txt, fixed = TRUE)))
    expect_true(any(grepl("Texto", r$dc$pills())))

    r$flt(modifyList(base, list(level = "family", values = "Orchidaceae", only_georef = TRUE)))
    session$flushReact()
    expect_true(all(r$dc$filtered()$family == "Orchidaceae" & r$dc$filtered()$has_coords))

    r$flt(modifyList(base, list(text = "zzzz-no-existe")))
    session$flushReact()
    expect_equal(r$dc$overview()$n_records, 0)
    expect_null(r$dc$map_cells())
  })
  expect_true(any(grepl("\"filtro\"", readLines(cfg$audit_log)))) # cada consulta queda auditada
})

test_that("data_controller: filtros malformados se sanean sin romper la sesión", {
  skip_if_no_data()
  cfg <- test_cfg()
  repo <- load_repository(cfg)
  testServer(function(id) {
    moduleServer(id, function(input, output, session) {
      flt <- reactiveVal(default_filters(repo$occ))
      list(flt = flt, dc = data_controller(cfg, repo, flt))
    })
  }, {
    r <- session$returned
    r$flt(list(
      level = "'; DROP TABLE x;--", values = NULL, text = strrep("x", 10000),
      elev_bands = "??", countries = "ZZ", years = c(NA, "a")
    ))
    session$flushReact()
    expect_gte(r$dc$overview()$n_records, 0)
  })
})

ok_verdict <- function(n = "Ana") {
  list(well_placed = TRUE, reviewer_name = n, reviewer_profession = "Bióloga, PhD")
}

test_that("verification_controller: guarda, valida, limita tasa y respeta el rol", {
  skip_if_no_data()
  cfg <- test_cfg(EPIG_VERIFICATION_RATE_MAX = "3")
  repo <- load_repository(cfg)
  msgs <- new.env()
  msgs$all <- character(0)
  notify <- function(text, ...) msgs$all <- c(msgs$all, text)
  reviewer_auth <- list(ok = TRUE, user = "ana", role = "reviewer")
  viewer_auth <- list(ok = TRUE, user = "eve", role = "viewer")
  testServer(function(id) {
    moduleServer(id, function(input, output, session) {
      key <- reactiveVal(NULL)
      list(
        key = key,
        reviewer = verification_controller(cfg, repo, reviewer_auth, key, notify),
        viewer = verification_controller(cfg, repo, viewer_auth, key, notify)
      )
    })
  }, {
    r <- session$returned
    k <- repo$occ$record_key[5]
    expect_false(r$reviewer$submit(ok_verdict())) # sin registro seleccionado
    r$key(k)
    expect_false(r$viewer$submit(ok_verdict())) # rol de solo lectura
    vacio <- list(well_placed = TRUE, reviewer_name = "", reviewer_profession = "x")
    expect_false(r$reviewer$submit(vacio))
    expect_true(r$reviewer$submit(ok_verdict("Ana")))
    expect_true(r$reviewer$submit(ok_verdict("Beto")))
    expect_true(r$reviewer$submit(ok_verdict("Cora")))
    expect_false(r$reviewer$submit(ok_verdict("Dana"))) # límite de tasa (3)
    expect_equal(r$reviewer$count(k), 3L)
    counts <- r$reviewer$counts()
    expect_equal(counts$n[counts$record_key == k], 3L)
  })
  expect_true(any(grepl("Demasiadas", msgs$all)))
  events <- vapply(readLines(cfg$audit_log), function(l) jsonlite::fromJSON(l)$event, "")
  expect_equal(sum(events == "verificacion"), 3L)
  expect_equal(sum(events == "verificacion_limitada"), 1L)
})

test_that("verification_controller: la corrección de ubicación exige departamento y municipio", {
  skip_if_no_data()
  cfg <- test_cfg()
  repo <- load_repository(cfg)
  reviewer_auth <- list(ok = TRUE, user = "ana", role = "reviewer")
  testServer(function(id) {
    moduleServer(id, function(input, output, session) {
      key <- reactiveVal(NULL)
      noop <- function(...) NULL
      list(key = key, ver = verification_controller(cfg, repo, reviewer_auth, key, noop))
    })
  }, {
    r <- session$returned
    r$key(repo$occ$record_key[5])
    incomplete <- list(
      well_placed = FALSE, issue_type = "ubicacion", reviewer_name = "Ana",
      reviewer_profession = "Bióloga"
    )
    expect_false(r$ver$submit(incomplete))
    completo <- list(department = "Antioquia", municipality = "Medellín")
    complete <- utils::modifyList(incomplete, completo)
    expect_true(r$ver$submit(complete))
    expect_false(r$ver$submit(list(
      well_placed = FALSE, issue_type = "region", reviewer_name = "Ana",
      reviewer_profession = "Bióloga"
    )))
    region_ok <- list(
      well_placed = FALSE, issue_type = "region",
      neotropical_region = "Chocó-Darién province", reviewer_name = "Ana",
      reviewer_profession = "Bióloga"
    )
    expect_true(r$ver$submit(region_ok))
    expect_equal(r$ver$count(repo$occ$record_key[5]), 2L)
  })
})

test_that("selection_controller solo acepta claves de registros existentes", {
  skip_if_no_data()
  repo <- load_repository(test_cfg())
  testServer(function(id) {
    moduleServer(id, function(input, output, session) selection_controller(repo))
  }, {
    s <- session$returned
    s$select("NO-EXISTE")
    expect_null(s$key())
    s$select(c("a", "b"))
    expect_null(s$key())
    s$select(repo$occ$record_key[3])
    expect_equal(s$key(), repo$occ$record_key[3])
    expect_equal(s$record()$record_key, repo$occ$record_key[3])
  })
})
