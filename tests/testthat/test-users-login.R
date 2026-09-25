# Inicio de sesión local: hash de contraseñas, autenticación, límites y flujo del controlador.

quick_hash <- function(pw) hash_password(pw, salt = as.raw(1:16), rounds = 4L)

test_that("el hash es salado, verificable y no contiene la contraseña", {
  h1 <- hash_password("una-clave-larga-1", rounds = 4L)
  h2 <- hash_password("una-clave-larga-1", rounds = 4L)
  expect_false(identical(h1, h2)) # sal distinta
  expect_true(verify_password("una-clave-larga-1", h1))
  expect_false(verify_password("otra-clave-larga", h1))
  expect_false(grepl("una-clave", h1, fixed = TRUE))
  expect_false(verify_password("x", "formato-invalido"))
})

test_that("users_add valida datos y users_authenticate concede solo credenciales correctas", {
  db <- tempfile(fileext = ".sqlite")
  expect_error(users_add(db, "a", "clave-larga-123"))
  expect_error(users_add(db, "ana", "corta"))
  expect_error(users_add(db, "ana", "clave-larga-123", role = "root"))
  users_add(db, "ana", "clave-larga-123", "admin")
  ok <- users_authenticate(db, "ANA", "clave-larga-123") # usuario sin distinguir mayúsculas
  expect_true(ok$ok)
  expect_equal(ok$role, "admin")
  expect_false(users_authenticate(db, "ana", "incorrecta")$ok)
  expect_equal(users_authenticate(db, "nadie", "clave-larga-123")$reason, "credenciales_invalidas")
  expect_equal(
    users_authenticate(db, "ana'; DROP TABLE users;--", "x")$reason,
    "credenciales_invalidas"
  )
  expect_true(users_authenticate(db, "ana", "clave-larga-123")$ok) # la tabla sigue intacta
  expect_equal(users_authenticate(tempfile(), "ana", "x")$reason, "sin_usuarios")
})

test_that("la configuración por defecto exige iniciar sesión", {
  cfg <- epig_config(env = function(k, d = "") d)
  expect_equal(cfg$auth_mode, "local")
})

test_that("login_controller no arranca la app con credenciales malas y sí con buenas", {
  cfg <- test_cfg(EPIG_AUTH_MODE = "local")
  cfg$users_db <- tempfile(fileext = ".sqlite")
  users_add(cfg$users_db, "ana", "clave-larga-123")
  started <- new.env()
  started$n <- 0
  testServer(function(input, output, session) {
    login_controller(input, output, session, cfg, function(auth) started$n <- started$n + 1)
  }, {
    session$setInputs(login_user = "ana", login_pass = "mala", login_go = 1)
    expect_equal(started$n, 0)
    session$setInputs(login_pass = "clave-larga-123", login_go = 2)
    expect_equal(started$n, 1)
    session$setInputs(login_go = 3) # un segundo clic no vuelve a arrancar la app
    expect_equal(started$n, 1)
  })
  events <- vapply(readLines(cfg$audit_log), function(l) jsonlite::fromJSON(l)$event, "")
  expect_equal(unname(events), c("login_fallido", "login_ok"))
  expect_false(any(grepl("clave-larga", readLines(cfg$audit_log), fixed = TRUE)))
})

test_that("tras demasiados intentos fallidos se bloquea incluso la contraseña correcta", {
  cfg <- test_cfg(EPIG_AUTH_MODE = "local", EPIG_LOGIN_MAX_ATTEMPTS = "2")
  cfg$users_db <- tempfile(fileext = ".sqlite")
  users_add(cfg$users_db, "ana", "clave-larga-123")
  started <- new.env()
  started$n <- 0
  testServer(function(input, output, session) {
    login_controller(input, output, session, cfg, function(auth) started$n <- started$n + 1)
  }, {
    session$setInputs(login_user = "ana", login_pass = "mala1", login_go = 1)
    session$setInputs(login_pass = "mala2", login_go = 2)
    session$setInputs(login_pass = "clave-larga-123", login_go = 3)
    expect_equal(started$n, 0)
  })
})

test_that("antes de iniciar sesión solo se envía la portada, sin datos ni secciones", {
  skip_if_no_data()
  cfg <- test_cfg(EPIG_AUTH_MODE = "local")
  repo <- load_repository(cfg)
  pre <- as.character(htmltools::renderTags(main_ui(cfg, repo))$html)
  expect_false(any(vapply(
    c("Resumen", "Análisis", "Aplicar filtros", "Cerrar sesión", "dash-level_a", "leaflet"),
    function(x) grepl(x, pre, fixed = TRUE), logical(1)
  )))
  landing <- as.character(landing_ui())
  expect_match(landing, "EpIG")
  expect_match(landing, "type=\"password\"")
  expect_match(as.character(login_error_ui("Error de prueba")), "Error de prueba")
  shell <- as.character(htmltools::renderTags(app_shell_ui(cfg, repo))$html)
  expect_match(shell, "Cerrar sesión")
})
