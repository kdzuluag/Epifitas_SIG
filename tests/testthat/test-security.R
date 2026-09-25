cfg_of <- function(...) {
  vars <- utils::modifyList(list(EPIG_AUTH_MODE = "none"), list(...))
  epig_config(env = function(k, d = "") if (!is.null(vars[[k]])) vars[[k]] else d)
}

test_that("sanitize_text limpia, recorta y rechaza vacío", {
  expect_null(sanitize_text("   \n  "))
  expect_null(sanitize_text(NULL))
  expect_equal(sanitize_text("  hola\007 mundo "), "hola mundo") # elimina caracteres de control
  expect_equal(nchar(sanitize_text(strrep("a", 5000), 1000)), 1000)
  html <- "<script>alert(1)</script>"
  expect_equal(sanitize_text(html), html) # se escapa al mostrar, no al guardar
})

test_that("el limitador de tasa bloquea el exceso y libera tras la ventana", {
  now <- 0
  lim <- make_rate_limiter(3, 60, clock = function() now)
  expect_equal(c(lim(), lim(), lim(), lim()), c(TRUE, TRUE, TRUE, FALSE))
  now <- 61
  expect_true(lim())
})

test_that("cada limitador de tasa lleva su propio estado", {
  a <- make_rate_limiter(1, 60)
  b <- make_rate_limiter(1, 60)
  expect_true(a())
  expect_false(a())
  expect_true(b())
})

test_that("la configuración de producción no admite auth 'none' ni proxy sin token", {
  expect_error(cfg_of(EPIG_ENV = "production"), "producción")
  expect_error(cfg_of(EPIG_AUTH_MODE = "proxy"), "EPIG_PROXY_TOKEN")
  expect_error(cfg_of(EPIG_AUTH_MODE = "otro"), "debe ser")
  expect_silent(cfg_of(EPIG_ENV = "production", EPIG_AUTH_MODE = "connect"))
})

test_that("modo connect: sin identidad se deniega; con grupos se asigna rol", {
  cfg <- cfg_of(
    EPIG_AUTH_MODE = "connect", EPIG_ADMIN_GROUPS = "epig-admin",
    EPIG_ALLOWED_GROUPS = "epig-admin,epig-taxa"
  )
  expect_false(resolve_user(list(user = NULL, groups = NULL), cfg)$ok)
  expect_equal(resolve_user(list(user = "ana", groups = "epig-taxa"), cfg)$role, "reviewer")
  expect_equal(resolve_user(list(user = "bob", groups = c("epig-admin")), cfg)$role, "admin")
  expect_equal(resolve_user(list(user = "eve", groups = "otros"), cfg)$reason, "grupo_no_permitido")
})

test_that("modo none solo para desarrollo y modo desconocido se deniega", {
  expect_equal(resolve_user(list(), cfg_of())$role, "reviewer")
  expect_equal(resolve_user(list(), list(auth_mode = "x"))$reason, "modo_desconocido")
})

test_that("modo proxy exige usuario y token correctos", {
  cfg <- cfg_of(EPIG_AUTH_MODE = "proxy", EPIG_PROXY_TOKEN = "s3cr3t")
  ok <- list(request = list(HTTP_X_FORWARDED_USER = "ana", HTTP_X_EPIG_PROXY_TOKEN = "s3cr3t"))
  expect_true(resolve_user(ok, cfg)$ok)
  bad_token <- ok
  bad_token$request$HTTP_X_EPIG_PROXY_TOKEN <- "x"
  expect_equal(resolve_user(bad_token, cfg)$reason, "token_proxy_invalido")
  no_user <- list(request = list(HTTP_X_EPIG_PROXY_TOKEN = "s3cr3t"))
  expect_equal(resolve_user(no_user, cfg)$reason, "sin_identidad")
})

test_that("verificaciones: SQL parametrizado (una inyección se guarda como texto inerte)", {
  skip_if_not_installed("RSQLite")
  db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db))
  evil <- "x'); DROP TABLE verifications;--"
  vs_add(db, "K1", "ana", list(
    well_placed = FALSE, reviewer_name = evil, reviewer_profession = "Bióloga",
    issue_type = "ubicacion", department = "Antioquia", municipality = "Medellín"
  ))
  vs_add(db, "K1", "bob", list(
    well_placed = TRUE, reviewer_name = "Bob", reviewer_profession = "PhD"
  ))
  expect_equal(vs_count_one(db, "K1"), 2L)
  expect_equal(vs_counts(db)$n, 2L)
  expect_equal(vs_count_one(db, "otro"), 0L)
  stored <- with_vs(db, function(con) {
    DBI::dbGetQuery(con, "SELECT reviewer_name FROM verifications")
  })
  expect_equal(stored$reviewer_name, c(evil, "Bob")) # tabla intacta, texto guardado verbatim
})

test_that("la bitácora de auditoría escribe JSON por línea", {
  f <- tempfile()
  on.exit(unlink(f))
  audit_log(list(audit_log = f), "login", "ana", list(x = 1))
  audit_log(list(audit_log = f), "denied", NA, list(reason = "sin_identidad"))
  lines <- readLines(f)
  expect_length(lines, 2)
  expect_equal(jsonlite::fromJSON(lines[1])$event, "login")
})

test_that("la bitácora AVISA cuando no puede escribir (no falla en silencio)", {
  blocker <- tempfile()
  writeLines("no soy una carpeta", blocker)
  on.exit(unlink(blocker))
  unwritable <- list(audit_log = file.path(blocker, "sub", "audit.jsonl"))
  expect_message(audit_log(unwritable, "evento"), "No se pudo escribir la bitácora")
})
