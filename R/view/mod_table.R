# Vista: tabla DT con procesamiento en servidor (solo viaja la página visible), sin botones de
# exportación.

DT_ES <- list(
  search = "Buscar:", lengthMenu = "Mostrar _MENU_", info = "_START_–_END_ de _TOTAL_ registros",
  infoEmpty = "Sin registros", infoFiltered = "(filtrado de _MAX_)", emptyTable = "Sin datos",
  zeroRecords = "Sin coincidencias", paginate = list(previous = "Anterior", `next` = "Siguiente")
)

#' Interfaz de la tabla de registros
#'
#' @param id Identificador del módulo.
#' @return Etiquetas HTML.
mod_table_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE, min_height = 560,
    card_header("Registros (seleccione una fila para ver el detalle y verificar su ubicación)"),
    DT::DTOutput(ns("tbl"))
  )
}

# Prepara la tabla visible: columnas permitidas (sin coordenadas), alertas legibles y nº de
# verificaciones de ubicación.
table_view <- function(d, counts) {
  out <- d[, TABLE_COLS, drop = FALSE]
  n <- counts$n[match(d$record_key, counts$record_key)]
  out$Verificaciones <- ifelse(is.na(n), 0L, as.integer(n))
  out$quality_flags <- vapply(strsplit(out$quality_flags, ";", fixed = TRUE), function(z) {
    paste(unname(QUALITY_FLAGS[z]), collapse = "; ")
  }, character(1))
  out
}

#' Servidor de la tabla de registros
#'
#' @param id Identificador del módulo.
#' @param table_df Reactivo con los registros filtrados.
#' @param counts Reactivo con el número de verificaciones de ubicación por registro.
#' @param on_select Función `function(record_key)` que se llama al seleccionar una fila.
#' @return Nada: registra las salidas y observadores del módulo.
mod_table_server <- function(id, table_df, counts, on_select) {
  moduleServer(id, function(input, output, session) {
    proxy <- DT::dataTableProxy("tbl")

    output$tbl <- DT::renderDT({
      DT::datatable(
        table_view(table_df(), isolate(counts())),
        rownames = FALSE, selection = "single",
        colnames = c(TABLE_LABELS, "Verificaciones"), class = "compact stripe hover",
        options = list(
          pageLength = 25, lengthMenu = c(10, 25, 50), scrollX = TRUE, dom = "lftip",
          language = DT_ES, order = list()
        )
      )
    })

    observeEvent(counts(),
      {
        DT::replaceData(proxy, table_view(table_df(), counts()),
          resetPaging = FALSE, clearSelection = "none", rownames = FALSE
        )
      },
      ignoreInit = TRUE
    )

    observeEvent(input$tbl_rows_selected, {
      i <- input$tbl_rows_selected
      if (length(i) == 1) on_select(table_df()$record_key[i])
    })
  })
}
