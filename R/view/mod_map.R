# Vista: mapa Leaflet. Solo recibe coordenadas GENERALIZADAS; el clic emite el id del registro.

COLOR_CHOICES <- c(
  Especie = "speciesName", `Género` = "genus", Familia = "family", Orden = "order",
  Localidad = "locality"
)
GROUP_POINTS <- "Ocurrencias"
GROUP_CELLS <- "Riqueza por celda"
GROUP_BORDER <- "Límite del Neotrópico"
GROUP_MASK <- "Máscara biorregional"
BASE_GROUPS <- c("Mapa claro", "Satélite", "Topográfico")

#' Interfaz del mapa
#'
#' @param id Identificador del módulo.
#' @param grid_deg Tamaño de celda de generalización (se muestra en el aviso de privacidad).
#' @return Etiquetas HTML.
mod_map_ui <- function(id, grid_deg) {
  ns <- NS(id)
  notice <- sprintf(
    paste0(
      " Ubicaciones generalizadas a celdas de %s° (≈%d km); cada punto está en algún lugar de ",
      "su celda, no en el sitio exacto. Las especies sensibles usan celdas mayores."
    ),
    format(grid_deg), round(grid_deg * 111)
  )
  card(
    full_screen = TRUE, min_height = 560,
    card_header(
      class = "d-flex flex-wrap align-items-center gap-3", "Mapa de ocurrencias",
      selectInput(ns("color_by"), "Colorear por",
        choices = COLOR_CHOICES, selected = "family", selectize = FALSE, width = "170px"
      ),
      input_switch(ns("cluster"), "Agrupar puntos", TRUE)
    ),
    leafletOutput(ns("map"), height = "100%"),
    card_footer(class = "small text-body-secondary", bsicons::bs_icon("shield-lock"), notice)
  )
}

# Paleta secuencial para la riqueza de especies por celda.
richness_palette <- function(cs) leaflet::colorNumeric("YlGn", domain = cs$n_species)

# Mapa base: mapas de fondo, máscara, límite y control de capas (sin datos de ocurrencia).
map_base <- function(repo) {
  bb <- sf::st_bbox(repo$neotropic)
  leaflet(options = leafletOptions(minZoom = 2, preferCanvas = TRUE)) |>
    addProviderTiles(providers$Esri.WorldGrayCanvas, group = BASE_GROUPS[1]) |>
    addProviderTiles(providers$Esri.WorldImagery, group = BASE_GROUPS[2]) |>
    addProviderTiles(providers$OpenTopoMap, group = BASE_GROUPS[3]) |>
    addPolygons(
      data = repo$mask, group = GROUP_MASK, stroke = FALSE, fillColor = "#1b1f1d", fillOpacity = 0.4
    ) |>
    addPolygons(
      data = repo$neotropic, group = GROUP_BORDER, color = BRAND_GREEN, weight = 2, fill = FALSE
    ) |>
    addLayersControl(
      baseGroups = BASE_GROUPS,
      overlayGroups = c(GROUP_POINTS, GROUP_CELLS, GROUP_BORDER, GROUP_MASK),
      options = layersControlOptions(collapsed = TRUE)
    ) |>
    hideGroup(GROUP_CELLS) |>
    fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
}

# Dibuja los puntos (coloreados por categoría, opcionalmente agrupados) y su leyenda.
draw_points <- function(proxy, d, by, cluster) {
  proxy <- proxy |>
    clearMarkers() |>
    clearMarkerClusters() |>
    removeControl("legend_pts")
  if (!nrow(d)) {
    return(invisible(proxy))
  }
  grp <- collapse_levels(d[[by]])
  cols <- category_colors(levels(grp))
  cluster_opts <- if (cluster) {
    markerClusterOptions(showCoverageOnHover = FALSE, maxClusterRadius = 45)
  }
  proxy |>
    addCircleMarkers(
      data = d, lng = ~lon_gen, lat = ~lat_gen, layerId = ~record_key, group = GROUP_POINTS,
      radius = 7, weight = 1.2, color = "#FFFFFF", fillColor = unname(cols[as.character(grp)]),
      fillOpacity = 0.9, label = ~speciesName, labelOptions = labelOptions(textsize = "13px"),
      clusterOptions = cluster_opts
    ) |>
    addLegend(
      position = "bottomright", colors = unname(cols), labels = names(cols), opacity = 1,
      title = names(COLOR_CHOICES)[match(by, COLOR_CHOICES)], layerId = "legend_pts"
    )
}

# Dibuja la capa de riqueza por celda; la deja oculta si el usuario la tenía desactivada.
draw_cells <- function(proxy, cs, visible) {
  proxy <- proxy |> clearGroup(GROUP_CELLS)
  if (is.null(cs)) {
    return(invisible(proxy))
  }
  pal <- richness_palette(cs)
  proxy <- proxy |>
    addPolygons(
      data = cs, group = GROUP_CELLS, stroke = TRUE, color = "#FFFFFF", weight = 1,
      fillColor = pal(cs$n_species), fillOpacity = 0.65,
      label = sprintf("%s · %d especies · %d registros", cs$label, cs$n_species, cs$n_records)
    )
  if (!visible) proxy <- proxy |> hideGroup(GROUP_CELLS)
  invisible(proxy)
}

# La leyenda de riqueza solo se muestra mientras la capa está activa.
draw_cells_legend <- function(proxy, cs, visible) {
  proxy <- proxy |> removeControl("legend_cells")
  if (is.null(cs) || !visible) {
    return(invisible(proxy))
  }
  proxy |>
    addLegend(
      position = "bottomleft", pal = richness_palette(cs), values = cs$n_species,
      title = "Especies por celda", layerId = "legend_cells", opacity = 0.9
    )
}

#' Servidor del mapa
#'
#' @param id Identificador del módulo.
#' @param repo Repositorio (polígonos del Neotrópico y máscara).
#' @param points Reactivo con los puntos generalizados a dibujar.
#' @param cells Reactivo con la capa `sf` de riqueza por celda (o `NULL`).
#' @param on_select Función `function(record_key)` que se llama al hacer clic en un punto.
#' @return Nada: registra las salidas y observadores del módulo.
mod_map_server <- function(id, repo, points, cells, on_select) {
  moduleServer(id, function(input, output, session) {
    # El mapa está en una pestaña oculta: Shiny no lo renderiza hasta que se abre y los mensajes
    # de leafletProxy enviados antes se pierden. Se espera al primer evento de límites.
    map_ready <- reactiveVal(FALSE)
    observeEvent(input$map_bounds, map_ready(TRUE), once = TRUE)
    proxy <- function() leafletProxy("map", session)

    output$map <- renderLeaflet(map_base(repo))

    observe({
      req(map_ready())
      draw_points(proxy(), points(), input$color_by, isTRUE(input$cluster))
    })
    observe({
      req(map_ready())
      draw_cells(proxy(), cells(), GROUP_CELLS %in% isolate(input$map_groups))
    })
    observeEvent(list(input$map_groups, cells(), map_ready()),
      {
        req(map_ready())
        draw_cells_legend(proxy(), cells(), GROUP_CELLS %in% input$map_groups)
      },
      ignoreNULL = FALSE
    )
    observeEvent(input$map_marker_click, on_select(input$map_marker_click$id))
  })
}
