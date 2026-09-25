# Vista: resumen (value boxes + dos gráficos de barras con selector taxonómico independiente).

METRIC_CHOICES <- c("Registros" = "records", "Especies" = "species")

# Tarjeta con un gráfico y sus dos selectores (nivel taxonómico y métrica).
chart_card <- function(ns, suffix, title, default_level) {
  level_id <- ns(paste0("level_", suffix))
  metric_id <- ns(paste0("metric_", suffix))
  card(
    full_screen = TRUE,
    card_header(
      class = "d-flex flex-wrap justify-content-between align-items-center gap-2", title,
      div(
        class = "d-flex gap-2",
        selectInput(level_id, tags$span(class = "visually-hidden", "Nivel taxonómico"),
          choices = TAX_LEVELS, selected = default_level, selectize = FALSE, width = "140px"
        ),
        selectInput(metric_id, tags$span(class = "visually-hidden", "Métrica"),
          choices = METRIC_CHOICES, selectize = FALSE, width = "120px"
        )
      )
    ),
    plotOutput(ns(paste0("plot_", suffix)), height = "440px")
  )
}

#' Interfaz del resumen (indicadores y dos gráficos)
#'
#' @param id Identificador del módulo.
#' @return Etiquetas HTML.
mod_dashboard_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      fill = FALSE, col_widths = c(3, 3, 3, 3),
      value_box("Registros", textOutput(ns("v_records"))),
      value_box("Especies únicas", textOutput(ns("v_species"))),
      value_box("Localidades únicas", textOutput(ns("v_localities"))),
      value_box("Sin coordenadas", textOutput(ns("v_nocoords")), textOutput(ns("v_georef")))
    ),
    layout_columns(
      col_widths = c(6, 6),
      chart_card(ns, "a", "Distribución taxonómica A", "family"),
      chart_card(ns, "b", "Distribución taxonómica B", "genus")
    )
  )
}

#' Gráfico de barras de un nivel taxonómico
#'
#' @param d Resultado de [count_by_level()].
#' @param level Columna taxonómica (género y especie se dibujan en cursiva).
#' @param metric_label Título del eje de valores.
#' @return Objeto `ggplot`.
bar_plot <- function(d, level, metric_label) {
  if (!nrow(d)) {
    return(empty_plot("Sin datos para los filtros actuales"))
  }
  italic <- level %in% c("genus", "speciesName")
  hbar_plot(d, x = "n", y = "taxon", x_lab = metric_label, italic = italic)
}

#' Servidor del resumen
#'
#' @param id Identificador del módulo.
#' @param overview Reactivo con los indicadores generales.
#' @param chart_data Función `function(level, metric)` que devuelve los datos de un gráfico.
#' @return Nada: registra las salidas y observadores del módulo.
mod_dashboard_server <- function(id, overview, chart_data) {
  moduleServer(id, function(input, output, session) {
    output$v_records <- renderText(fmt_int(overview()$n_records))
    output$v_species <- renderText(fmt_int(overview()$n_species))
    output$v_localities <- renderText(fmt_int(overview()$n_localities))
    output$v_nocoords <- renderText(fmt_int(overview()$n_no_coords))
    output$v_georef <- renderText(paste(fmt_pct(overview()$pct_georef), "con ubicación"))

    render_chart <- function(suffix) {
      level <- reactive(input[[paste0("level_", suffix)]])
      metric <- reactive(input[[paste0("metric_", suffix)]])
      data <- reactive(chart_data(level(), metric()))
      alt_text <- reactive({
        what <- if (metric() == "species") "especies" else "registros"
        lvl <- names(TAX_LEVELS)[match(level(), TAX_LEVELS)]
        sprintf(
          "Gráfico de barras: los %d taxones de nivel %s con más %s.",
          nrow(data()), lvl, what
        )
      })
      renderPlot(
        {
          d <- data()
          label <- if (attr(d, "metric") == "species") "Especies únicas" else "Registros"
          bar_plot(d, level(), label)
        },
        res = 110,
        alt = alt_text
      )
    }
    output$plot_a <- render_chart("a")
    output$plot_b <- render_chart("b")
  })
}
