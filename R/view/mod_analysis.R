# Vista: análisis estadístico (acumulación de especies, Chao1, ecología altitudinal, temporal,
# país, calidad).

#' Interfaz del análisis estadístico
#'
#' @param id Identificador del módulo.
#' @return Etiquetas HTML.
mod_analysis_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(8, 4),
      card(
        full_screen = TRUE,
        card_header("Curva de acumulación de especies (rarefacción por permutación)"),
        plotOutput(ns("accum"), height = "360px"),
        card_footer(
          class = "small text-body-secondary",
          "Media y banda del 5–95 % de 30 permutaciones del orden de los registros. ",
          "Un registro no equivale a un individuo."
        )
      ),
      card(card_header("Riqueza estimada (Chao1)"), uiOutput(ns("chao")))
    ),
    layout_columns(
      col_widths = c(4, 4, 4),
      card(card_header("Registros por piso altitudinal"), plotOutput(ns("elev"), height = "300px")),
      card(card_header("Registros por década"), plotOutput(ns("decade"), height = "300px")),
      card(card_header("Riqueza por país"), plotOutput(ns("country"), height = "300px"))
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(card_header("Alertas de calidad para revisión taxonómica"), tableOutput(ns("flags"))),
      card(card_header("Completitud de campos"), plotOutput(ns("complete"), height = "240px"))
    )
  )
}

# ---- Constructores de gráficos (funciones puras: datos -> ggplot) ----

# Curva de acumulación de especies con su banda de permutaciones.
plot_accumulation <- function(d) {
  if (is.null(d)) {
    return(empty_plot("Datos insuficientes"))
  }
  ggplot2::ggplot(d, ggplot2::aes(records, mean)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi), fill = BRAND_GREEN, alpha = 0.18) +
    ggplot2::geom_line(colour = BRAND_GREEN, linewidth = 1.1) +
    ggplot2::scale_x_continuous(labels = function(x) fmt_int(x)) +
    ggplot2::scale_y_continuous(labels = function(x) fmt_int(x)) +
    ggplot2::labs(x = "Registros acumulados", y = "Especies acumuladas") +
    theme_epig() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_line(colour = "#e5e5e5"))
}

# Barras de registros por piso altitudinal.
plot_elevation <- function(d) {
  d$band <- sub(" \\(", "\n(", as.character(d$band))
  hbar_plot(d, x = "n", y = "band", fill = "#0072B2", x_lab = "Registros", size = 11)
}

# Barras de registros por década de colecta.
plot_decades <- function(d) {
  if (!nrow(d)) {
    return(empty_plot("Sin fechas"))
  }
  ggplot2::ggplot(d, ggplot2::aes(factor(decade), n)) +
    ggplot2::geom_col(fill = "#D55E00", width = 0.75) +
    ggplot2::labs(x = "Década", y = "Registros") +
    theme_epig(11) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 60, hjust = 1),
      panel.grid.major.y = ggplot2::element_line(colour = "#e5e5e5")
    )
}

# Barras de especies únicas por país.
plot_countries <- function(d) {
  if (!nrow(d)) {
    return(empty_plot("Sin países"))
  }
  hbar_plot(d,
    x = "n_species", y = "country", fill = "#009E73", x_lab = "Especies únicas", size = 11
  )
}

# Barras de completitud (porcentaje) de los campos clave.
plot_completeness <- function(d) {
  hbar_plot(d,
    x = "pct", y = "campo",
    x_lab = NULL, inside = TRUE, x_limits = c(0, 1), size = 11,
    label_fn = function(v) fmt_pct(v, 0)
  )
}

# ---- Piezas de interfaz ----

# Panel con el estimador Chao1 y su advertencia de interpretación.
chao_ui <- function(r) {
  if (is.na(r$estimated)) {
    return(tags$p("Sin datos."))
  }
  row <- function(label, value, strong = TRUE) {
    tagList(
      tags$dt(class = "col-7", label),
      tags$dd(class = paste("col-5 text-end", if (strong) "fw-semibold"), value)
    )
  }
  tagList(
    tags$dl(
      class = "row mb-2",
      row("Observada", fmt_int(r$observed)),
      row("Estimada (Chao1)", fmt_int(round(r$estimated))),
      row("Completitud", fmt_pct(r$completeness)),
      row("Singletons (f1)", fmt_int(r$f1), strong = FALSE),
      row("Doubletons (f2)", fmt_int(r$f2), strong = FALSE)
    ),
    tags$p(
      class = "small text-body-secondary mb-0",
      "Estimador con corrección de sesgo (Chao 1987) usando registros como unidad. ",
      "Es indicativo del esfuerzo de colecta, no un censo."
    )
  )
}

# Tabla legible del conteo de alertas de calidad.
flags_table <- function(flags) {
  data.frame(
    Alerta = flags$label, Registros = fmt_int(flags$n), `%` = fmt_pct(flags$pct, 2),
    check.names = FALSE
  )
}

#' Servidor del análisis estadístico
#'
#' @param id Identificador del módulo.
#' @param a Lista de reactivos `accum`, `chao`, `elevation`, `decade`, `country`, `quality`.
#' @return Nada: registra las salidas y observadores del módulo.
mod_analysis_server <- function(id, a) {
  moduleServer(id, function(input, output, session) {
    output$accum <- renderPlot(plot_accumulation(a$accum()),
      res = 110, alt = "Curva de acumulación de especies según registros acumulados."
    )
    output$chao <- renderUI(chao_ui(a$chao()))
    output$elev <- renderPlot(plot_elevation(a$elevation()),
      res = 110, alt = "Registros por piso altitudinal."
    )
    output$decade <- renderPlot(plot_decades(a$decade()),
      res = 110, alt = "Registros por década de colecta."
    )
    output$country <- renderPlot(plot_countries(a$country()),
      res = 110, alt = "Especies únicas por país."
    )
    output$flags <- renderTable(flags_table(a$quality()$flags),
      striped = TRUE, spacing = "s", width = "100%", align = "lrr"
    )
    output$complete <- renderPlot(plot_completeness(a$quality()$completeness),
      res = 110, alt = "Porcentaje de completitud de coordenadas, elevación, fecha y taxonomía."
    )
  })
}
