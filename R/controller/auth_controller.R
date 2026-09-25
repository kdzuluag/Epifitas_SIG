# Controlador de acceso: resuelve la identidad de la sesión y la registra en la bitácora.

#' Resuelve el acceso de la sesión y lo audita
#'
#' @param session Sesión de Shiny.
#' @param cfg Configuración.
#' @return Lista con `ok`, `user`, `role` y `reason` (ver [resolve_user()]).
auth_controller <- function(session, cfg) {
  u <- resolve_user(session, cfg)
  audit_log(
    cfg, if (u$ok) "acceso" else "acceso_denegado", u$user,
    list(rol = u$role, motivo = u$reason)
  )
  u
}

#' Controlador de inicio de sesión local (usuario y contraseña)
#'
#' Gestiona el formulario de la portada; tras un inicio válido llama a `on_success(auth)`.
#' Limita los intentos fallidos por sesión y audita cada intento sin registrar la contraseña.
#'
#' @param input,output,session Argumentos estándar de Shiny.
#' @param cfg Configuración.
#' @param on_success Función `function(auth)` que arranca la app con la identidad validada.
#' @return Nada: registra los observadores.
login_controller <- function(input, output, session, cfg, on_success) {
  limiter <- make_rate_limiter(cfg$login_max_attempts, cfg$login_window)
  started <- reactiveVal(FALSE)
  message_text <- reactiveVal(NULL)
  output$login_error <- renderUI(login_error_ui(message_text()))
  observeEvent(input$login_go, {
    if (started()) {
      return()
    }
    if (!limiter()) {
      audit_log(cfg, "login_bloqueado", input$login_user, list())
      return(message_text("Demasiados intentos. Espere unos minutos."))
    }
    auth <- users_authenticate(cfg$users_db, input$login_user, input$login_pass)
    audit_log(
      cfg, if (auth$ok) "login_ok" else "login_fallido", auth$user %||% input$login_user,
      list(motivo = auth$reason)
    )
    if (!auth$ok) {
      return(message_text("Usuario o contraseña incorrectos."))
    }
    started(TRUE)
    on_success(auth)
  })
}

#' Ventana modal que bloquea la interfaz cuando el acceso es denegado
#'
#' @return Objeto `shiny::modalDialog`.
denied_modal <- function() {
  modalDialog(
    title = "Acceso restringido",
    tags$p(
      "Esta herramienta es de uso exclusivo para taxónomos y especialistas autorizados ",
      "del proyecto EpIG."
    ),
    tags$p(
      "Inicie sesión con su cuenta autorizada o solicite acceso a la coordinación del proyecto."
    ),
    footer = NULL, easyClose = FALSE
  )
}
