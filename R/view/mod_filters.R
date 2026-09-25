# Vista: barra lateral de filtros. Emite un estado "aplicado"; no contiene lógica de negocio.

#' Interfaz de la barra lateral de filtros
#'
#' @param id Identificador del módulo.
#' @param repo Repositorio (solo se usa para poblar catálogos).
#' @param cfg Configuración.
#' @return Etiquetas HTML.
mod_filters_ui <- function(id, repo, cfg) {
  ns <- NS(id)
  tagList(
    accordion(
      id = ns("acc"), multiple = TRUE, open = c("tax", "geo"),
      taxonomy_panel(ns, cfg),
      geography_panel(ns, repo)
    ),
    filter_buttons(ns),
    tags$p(
      class = "small text-body-secondary mt-3 mb-0",
      bsicons::bs_icon("shield-lock"),
      " Las ubicaciones se muestran generalizadas; no hay descarga de datos."
    )
  )
}

# Panel de filtros taxonómicos: nivel, valores y búsqueda libre.
taxonomy_panel <- function(ns, cfg) {
  accordion_panel(
    "Taxonomía",
    value = "tax", icon = bsicons::bs_icon("diagram-3"),
    radioButtons(ns("level"), "Nivel taxonómico",
      choices = TAX_LEVELS, selected = "family", inline = TRUE
    ),
    selectizeInput(ns("values"), "Valores",
      choices = NULL, multiple = TRUE,
      options = list(
        placeholder = "Escriba para buscar…", plugins = list("remove_button"),
        maxItems = cfg$max_filter_values
      )
    ),
    textInput(ns("text"), "Búsqueda libre (todos los niveles)",
      placeholder = "p. ej. tillandsia"
    )
  )
}

# Panel de filtros geográficos y ecológicos: país, piso altitudinal, años y georreferencia.
geography_panel <- function(ns, repo) {
  years <- if (all(is.finite(repo$year_range))) repo$year_range else c(1900L, 2025L)
  countries <- stats::setNames(repo$countries, country_name(repo$countries))
  accordion_panel(
    "Geografía y ecología",
    value = "geo", icon = bsicons::bs_icon("globe-americas"),
    selectizeInput(ns("countries"), "País",
      choices = countries, multiple = TRUE,
      options = list(placeholder = "Todos", plugins = list("remove_button"))
    ),
    checkboxGroupInput(ns("elev"), "Piso altitudinal", choices = ELEV_LABELS),
    sliderInput(ns("years"), "Año de colecta",
      min = years[1], max = years[2], value = years, step = 1, sep = ""
    ),
    checkboxInput(ns("georef"), "Solo registros con ubicación en el mapa", FALSE)
  )
}

# Botones "Aplicar filtros" y "Restablecer".
filter_buttons <- function(ns) {
  div(
    class = "d-grid gap-2 mt-3",
    actionButton(ns("apply"), "Aplicar filtros", icon = icon("filter"), class = "btn-primary"),
    actionButton(ns("reset"), "Restablecer",
      icon = icon("rotate-left"),
      class = "btn-outline-secondary"
    )
  )
}

# Vuelve todos los controles de filtro a sus valores por defecto.
reset_filter_inputs <- function(session, defaults) {
  updateRadioButtons(session, "level", selected = defaults$level)
  updateSelectizeInput(session, "values", selected = character(0))
  updateTextInput(session, "text", value = "")
  updateSelectizeInput(session, "countries", selected = character(0))
  updateCheckboxGroupInput(session, "elev", selected = character(0))
  if (!is.null(defaults$years)) updateSliderInput(session, "years", value = defaults$years)
  updateCheckboxInput(session, "georef", value = FALSE)
}

#' Servidor de la barra lateral de filtros
#'
#' @param id Identificador del módulo.
#' @param choices_fn Función `function(level)` que devuelve las opciones del selector.
#' @param defaults Filtros por defecto ([default_filters()]).
#' @return Reactivo con los filtros aplicados (cambia solo al pulsar "Aplicar" o "Restablecer").
mod_filters_server <- function(id, choices_fn, defaults) {
  moduleServer(id, function(input, output, session) {
    applied <- reactiveVal(defaults)

    observeEvent(input$level, {
      updateSelectizeInput(session, "values",
        choices = choices_fn(input$level), selected = character(0), server = TRUE
      )
    })

    observeEvent(input$apply, {
      applied(list(
        level = input$level, values = input$values, text = input$text, elev_bands = input$elev,
        countries = input$countries, years = input$years, only_georef = isTRUE(input$georef)
      ))
    })

    observeEvent(input$reset, {
      reset_filter_inputs(session, defaults)
      applied(defaults)
    })

    applied
  })
}
