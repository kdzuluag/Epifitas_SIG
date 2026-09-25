test_that("auth_controller concede o deniega el acceso y lo deja auditado", {
  cfg <- test_cfg(EPIG_AUTH_MODE = "connect")
  granted <- auth_controller(list(user = "ana", groups = "g"), cfg)
  denied <- auth_controller(list(user = NULL, groups = NULL), cfg)
  expect_true(granted$ok)
  expect_false(denied$ok)
  events <- vapply(readLines(cfg$audit_log), function(l) jsonlite::fromJSON(l)$event, "")
  expect_equal(unname(events), c("acceso", "acceso_denegado"))
})

test_that("el aviso de acceso denegado bloquea la interfaz y no ofrece cierre", {
  html <- as.character(denied_modal())
  expect_match(html, "Acceso restringido")
  expect_false(grepl("modal-footer", html, fixed = TRUE))
  expect_false(grepl("data-dismiss|data-bs-dismiss", html))
})
