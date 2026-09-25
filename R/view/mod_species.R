# Vista: pestaña "Especies" (lista buscable + ficha por especie).

#' Interfaz de la pestaña Especies
#'
#' @param id Identificador del módulo.
#' @return Etiquetas HTML.
mod_species_ui <- function(id) {
  ns <- NS(id)
  layout_columns(
    col_widths = c(5, 7),
    card(
      full_screen = TRUE, min_height = 520,
      card_header("Especies"),
      DT::DTOutput(ns("tbl"))
    ),
    card(card_header("Ficha de la especie"), uiOutput(ns("profile")))
  )
}

# Rango "a–b" o texto de ausencia de datos; `fmt` da formato a cada extremo.
range_text <- function(r, unit = "", fmt = fmt_int) {
  if (is.null(r)) {
    return("sin dato")
  }
  ends <- unique(fmt(r))
  paste0(paste(ends, collapse = "–"), unit)
}

# Ficha de una especie: taxonomía, conteos y cobertura. Nunca muestra coordenadas.
profile_ui <- function(p) {
  if (is.null(p)) {
    return(tags$p(class = "text-muted", "Seleccione una especie de la lista."))
  }
  tagList(
    tags$h3(class = "species-name", tags$em(p$name)),
    tags$p(class = "text-muted", paste(p$order, "·", p$family, "·"), tags$em(p$genus)),
    layout_columns(
      fill = FALSE, col_widths = c(4, 4, 4),
      div(
        class = "statbox",
        tags$span(class = "stat-label", "Registros"), tags$b(fmt_int(p$n_records))
      ),
      div(
        class = "statbox",
        tags$span(class = "stat-label", "Celdas con registro"), tags$b(fmt_int(p$n_cells))
      ),
      div(class = "statbox", tags$span(class = "stat-label", "Alertas"), tags$b(fmt_int(p$n_flags)))
    ),
    tags$dl(
      class = "mt-3",
      tags$dt("Países"), tags$dd(paste(p$countries, collapse = ", ")),
      tags$dt("Elevación"), tags$dd(range_text(p$elev_range, " m")),
      tags$dt("Años de colecta"), tags$dd(range_text(p$year_range, fmt = as.character)),
      tags$dt("Instituciones"), tags$dd(paste(p$institutions, collapse = ", "))
    ),
    tags$p(
      class = "small text-muted",
      "Las ubicaciones se muestran solo como celdas generalizadas en la pestaña Mapa."
    )
  )
}

#' Servidor de la pestaña Especies
#'
#' @param id Identificador del módulo.
#' @param species_table Reactivo con el listado de especies ([species_list()]).
#' @param profile Función `function(nombre)` que devuelve la ficha ([species_profile()]).
#' @return Nada: registra las salidas y observadores del módulo.
mod_species_server <- function(id, species_table, profile) {
  moduleServer(id, function(input, output, session) {
    output$tbl <- DT::renderDT({
      DT::datatable(
        species_table()[, c("speciesName", "family", "n_records")],
        rownames = FALSE, selection = "single", class = "compact stripe hover",
        colnames = c("Nombre de la especie", "Familia", "Registros"),
        options = list(pageLength = 15, dom = "ftip", language = DT_ES, order = list())
      ) |> DT::formatStyle("speciesName", fontStyle = "italic")
    })
    output$profile <- renderUI({
      i <- input$tbl_rows_selected
      profile_ui(if (length(i) == 1) profile(species_table()$speciesName[i]))
    })
  })
}
