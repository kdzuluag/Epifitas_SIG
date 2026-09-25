# Controlador de datos: convierte filtros de la vista en datos/estadísticas usando el Modelo.
# Optimización: los cálculos caros se cachean por filtros saneados (bindCache, compartido entre
# sesiones).

# Columnas mínimas que necesita el mapa (todas generalizadas; nunca coordenadas exactas).
MAP_COLS <- c(
  "record_key", "speciesName", "genus", "family", "order", "locality", "lon_gen", "lat_gen"
)

#' Controlador de datos: filtros -> registros, estadísticas y capas del mapa
#'
#' @param cfg Configuración.
#' @param repo Repositorio ([load_repository()]).
#' @param filters Reactivo con los filtros aplicados desde la vista.
#' @param user Identidad del usuario (para la bitácora).
#' @return Lista de reactivos y funciones: `filtered`, `pills`, `overview`, `chart_data`,
#'   `analysis`, `map_points`, `map_cells`, `species_table`, `species_profile` y `table_df`.
data_controller <- function(cfg, repo, filters, user = NA_character_) {
  occ <- repo$occ

  clean <- reactive(sanitize_filters(filters(), occ, cfg))
  filtered <- bindCache(reactive(filter_occurrences(occ, clean())), clean())

  observeEvent(clean(),
    {
      f <- clean()
      audit_log(cfg, "filtro", user, list(
        nivel = f$level, n_valores = length(f$values), n_terminos = length(f$tokens),
        n_resultado = nrow(filtered())
      ))
    },
    ignoreInit = TRUE
  )

  species <- reactive(filtered()$speciesName)
  list(
    filtered = filtered,
    pills = reactive(filter_summary(clean())),
    overview = reactive(summarise_overview(filtered())),
    chart_data = function(level, metric) {
      count_by_level(filtered(), level, metric, cfg$top_n_default)
    },
    analysis = list(
      accum = bindCache(reactive(species_accumulation(species())), clean()),
      chao = bindCache(reactive(chao1(species())), clean()),
      elevation = reactive(by_elevation(filtered())),
      decade = reactive(by_decade(filtered())),
      country = reactive(by_country(filtered(), cfg$top_n_default)),
      quality = reactive(quality_summary(filtered()))
    ),
    map_points = reactive({
      d <- filtered()
      d[d$has_coords, MAP_COLS, drop = FALSE]
    }),
    map_cells = bindCache(reactive(cells_to_sf(richness_by_cell(filtered()))), clean()),
    species_table = reactive(species_list(filtered())),
    species_profile = function(name) species_profile(filtered(), name),
    table_df = filtered
  )
}
