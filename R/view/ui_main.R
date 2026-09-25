# Vista principal: ensambla el layout. Sin lógica de datos.

# Panel lateral derecho con el detalle del registro (mapa y tabla lo comparten).
detail_sidebar <- function(id) {
  sidebar(position = "right", width = 380, open = "desktop", title = "Detalle", mod_detail_ui(id))
}

#' Interfaz principal de la aplicación
#'
#' En modo `local` solo devuelve la portada de acceso: la interfaz de la aplicación se genera en
#' el servidor y se envía únicamente tras un inicio de sesión válido.
#'
#' @param cfg Configuración.
#' @param repo Repositorio de datos.
#' @return Página `bslib`.
main_ui <- function(cfg, repo) {
  if (!identical(cfg$auth_mode, "local")) {
    return(app_shell_ui(cfg, repo))
  }
  page_fillable(
    theme = epig_theme(), lang = "es", title = cfg$app_title, padding = 0, gap = 0,
    tags$head(tags$link(rel = "stylesheet", href = "custom.css")),
    uiOutput("root", fill = TRUE)
  )
}

#' Interfaz completa de la aplicación (navegación, filtros y secciones)
#'
#' @param cfg Configuración.
#' @param repo Repositorio de datos.
#' @return Página `bslib` con las secciones de la aplicación.
app_shell_ui <- function(cfg, repo) {
  page_navbar(
    title = tags$span(bsicons::bs_icon("flower1"), " EpIG"),
    id = "nav", lang = "es", theme = epig_theme(), window_title = cfg$app_title,
    fillable = c("Mapa", "Tabla"),
    navbar_options = navbar_options(theme = "dark", bg = BRAND_GREEN),
    sidebar = sidebar(
      id = "sb", width = 330, open = "desktop", title = "Filtros",
      mod_filters_ui("filters", repo, cfg)
    ),
    header = tagList(
      tags$head(tags$link(rel = "stylesheet", href = "custom.css")),
      uiOutput("demo_banner"),
      group_hero_ui(),
      uiOutput("filter_pills")
    ),
    nav_panel("Resumen", icon = bsicons::bs_icon("speedometer2"), mod_dashboard_ui("dash")),
    nav_panel("Especies", icon = bsicons::bs_icon("list-ul"), mod_species_ui("species")),
    nav_panel("Análisis", icon = bsicons::bs_icon("graph-up"), mod_analysis_ui("analysis")),
    nav_panel("Mapa",
      icon = bsicons::bs_icon("map"),
      layout_sidebar(sidebar = detail_sidebar("detail_map"), mod_map_ui("map", repo$meta$grid_deg))
    ),
    nav_panel("Tabla",
      icon = bsicons::bs_icon("table"),
      layout_sidebar(sidebar = detail_sidebar("detail_tbl"), mod_table_ui("table"))
    ),
    nav_panel("Acerca de", icon = bsicons::bs_icon("info-circle"), mod_about_ui(repo, cfg)),
    nav_spacer(),
    if (identical(cfg$auth_mode, "local")) {
      nav_item(actionLink("logout", "Cerrar sesión", icon = bsicons::bs_icon("box-arrow-right")))
    }
  )
}

# Cabecera de grupo (estilo de página de grupo): título, emblema y coordinación.
group_hero_ui <- function() {
  div(
    class = "group-hero",
    div(
      class = "hero-text",
      tags$h1("Plantas epífitas del Neotrópico"),
      tags$p(
        class = "hero-sub", "Grupo de expertos en taxonomía y distribución de epífitas vasculares"
      ),
      tags$p(class = "hero-mod", tags$span("Prototipo"), "Versión de demostración")
    ),
    div(class = "hero-emblem", `aria-hidden` = "true", bsicons::bs_icon("flower1"))
  )
}

# Pie de página institucional.
site_footer_ui <- function() {
  tags$footer(
    class = "site-footer",
    div(tags$b("Proyecto EpIG"), tags$br(), "Epífitas vasculares del Neotrópico"),
    div(
      class = "footer-links",
      "Ubicaciones generalizadas · sin descargas · uso solo para revisión experta"
    )
  )
}

# Aviso visible cuando los datos son sintéticos de demostración.
demo_banner_ui <- function(repo) {
  if (isTRUE(repo$meta$demo)) {
    div(
      class = "alert alert-warning rounded-0 mb-0 py-1 small text-center",
      bsicons::bs_icon("exclamation-triangle"),
      " DATOS SINTÉTICOS DE DEMOSTRACIÓN: los nombres de especies son inventados."
    )
  }
}

# Contador de registros y etiquetas de los filtros activos.
filter_pills_ui <- function(pills, n_records) {
  div(
    class = "px-3 pt-2 d-flex flex-wrap align-items-center gap-2",
    tags$span(class = "fw-semibold", sprintf("%s registros", fmt_int(n_records))),
    lapply(pills, function(x) tags$span(class = "badge rounded-pill text-bg-light border", x))
  )
}

#' Servidor principal: conecta controladores y vistas
#'
#' Si el acceso es denegado (o aún no hay inicio de sesión) no se inicializa ningún controlador
#' de datos.
#'
#' @param input,output,session Argumentos estándar de Shiny.
#' @param cfg Configuración.
#' @param repo Repositorio de datos.
#' @return Nada: registra las salidas y observadores del módulo.
main_server <- function(input, output, session, cfg, repo) {
  if (identical(cfg$auth_mode, "local")) {
    observeEvent(input$logout, session$reload())
    output$root <- renderUI(landing_ui())
    login_controller(input, output, session, cfg, function(auth) {
      output$root <- renderUI(app_shell_ui(cfg, repo))
      start_app(input, output, session, cfg, repo, auth)
    })
    return(invisible())
  }
  auth <- auth_controller(session, cfg)
  if (!auth$ok) {
    showModal(denied_modal())
    return(invisible())
  }
  start_app(input, output, session, cfg, repo, auth)
}

#' Inicializa controladores y vistas para una sesión ya autenticada
#'
#' @param input,output,session Argumentos estándar de Shiny.
#' @param cfg Configuración.
#' @param repo Repositorio de datos.
#' @param auth Identidad validada (`user`, `role`).
#' @return Nada: registra las salidas y observadores.
start_app <- function(input, output, session, cfg, repo, auth) {
  filters <- mod_filters_server(
    "filters", function(level) taxon_choices(repo$occ, level), default_filters(repo$occ)
  )
  data <- data_controller(cfg, repo, filters, auth$user)
  sel <- selection_controller(repo)
  ver <- verification_controller(cfg, repo, auth, sel$key)

  mod_dashboard_server("dash", data$overview, data$chart_data)
  mod_analysis_server("analysis", data$analysis)
  mod_species_server("species", data$species_table, data$species_profile)
  mod_map_server("map", repo, data$map_points, data$map_cells, sel$select)
  mod_table_server("table", data$table_df, ver$counts, sel$select)
  for (id in c("detail_map", "detail_tbl")) {
    mod_detail_server(id, sel$record, ver, cfg$detail_max_chars)
  }

  output$demo_banner <- renderUI(demo_banner_ui(repo))
  output$filter_pills <- renderUI(filter_pills_ui(data$pills(), nrow(data$filtered())))
}
