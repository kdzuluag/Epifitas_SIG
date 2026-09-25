# Vista: portada de EpIG con el acceso (modo de autenticación "local"). Es lo único que se envía
# al navegador antes de iniciar sesión.

#' Mensaje de error del formulario de acceso
#'
#' @param msg Texto plano o `NULL`.
#' @return Etiquetas HTML (vacío si no hay mensaje).
login_error_ui <- function(msg = NULL) {
  if (is.null(msg)) {
    return(NULL)
  }
  tags$div(class = "alert alert-danger py-2 mb-3", role = "alert", msg)
}

#' Portada de EpIG con el formulario de acceso
#'
#' @return Etiquetas HTML de la portada (sin datos ni elementos de la aplicación).
landing_ui <- function() {
  div(
    class = "landing",
    div(
      class = "landing-brand",
      div(class = "landing-emblem", `aria-hidden` = "true", bsicons::bs_icon("flower1")),
      tags$h1("EpIG"),
      tags$p(class = "landing-tag", "Epífitas vasculares del Neotrópico"),
      tags$p(
        class = "landing-sub",
        "Plataforma de consulta y revisión para taxónomos y especialistas."
      )
    ),
    div(
      class = "landing-card",
      tags$h2("Iniciar sesión"),
      tags$p(class = "text-muted small", "Acceso exclusivo para revisores autorizados."),
      textInput("login_user", "Usuario"),
      passwordInput("login_pass", "Contraseña"),
      uiOutput("login_error"),
      actionButton("login_go", "Entrar", class = "btn-primary w-100")
    ),
    tags$footer(class = "landing-foot", "Proyecto EpIG · Prototipo")
  )
}
