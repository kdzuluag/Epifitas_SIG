# Vista: detalle del registro (sin coordenadas) + verificación de ubicación por el taxónomo.
# El taxónomo ya no escribe comentarios libres: confirma si el punto está bien ubicado y, si no,
# corrige el departamento/municipio o la región neotrópica (R/model/verification.R valida).
# Todo texto de usuario se muestra vía tags$*/textInput, que escapan HTML; nunca se usa HTML().

# Par etiqueta/valor de una lista de definición; se omite si no hay valor.
detail_field <- function(label, value) {
  if (is.null(value) || length(value) == 0 || is.na(value) || !nzchar(as.character(value))) {
    return(NULL)
  }
  tagList(
    tags$dt(class = "col-5 text-body-secondary fw-normal", label),
    tags$dd(class = "col-7", as.character(value))
  )
}

# Ubicación aproximada (solo la celda) o aviso de que no hay coordenadas utilizables.
location_note <- function(r) {
  if (isTRUE(r$has_coords)) {
    tags$p(
      class = "small mb-2", bsicons::bs_icon("geo-alt"), " Ubicación aproximada: ",
      cell_label(r$cell_deg, r$cell_ix, r$cell_iy)
    )
  } else {
    tags$p(
      class = "small text-body-secondary mb-2", bsicons::bs_icon("slash-circle"),
      " Sin coordenadas utilizables"
    )
  }
}

# Insignias de las alertas de calidad de un registro.
flag_badges <- function(quality_flags) {
  flags <- QUALITY_FLAGS[strsplit(quality_flags, ";", fixed = TRUE)[[1]]]
  if (!length(flags)) {
    return(NULL)
  }
  div(class = "mb-2", lapply(unname(flags), function(f) {
    tags$span(class = "badge text-bg-warning me-1 mb-1", f)
  }))
}

# Elevación tal como se escribió más su piso altitudinal, si se conoce.
elevation_text <- function(r) {
  band <- if (!is.na(r$elev_band)) paste0("[", as.character(r$elev_band), "]")
  paste(stats::na.omit(c(r$verbatimElevation, band)), collapse = " ")
}

# Ficha completa del registro (sin coordenadas).
record_info_ui <- function(r) {
  tagList(
    tags$h5(class = "mb-0", tags$em(r$speciesName)),
    tags$p(
      class = "text-body-secondary",
      paste(stats::na.omit(c(r$family, r$order)), collapse = " · ")
    ),
    location_note(r),
    flag_badges(r$quality_flags),
    tags$dl(
      class = "row small mb-0",
      detail_field("Nombre científico", r$scientificName),
      detail_field("País", country_name(r$countryCode)),
      detail_field("Localidad", r$locality),
      detail_field("Elevación", elevation_text(r)),
      detail_field("Fecha", r$eventDate),
      detail_field("Tipo de registro", r$basisOfRecord),
      detail_field("Institución", r$institutionCode),
      detail_field("Colección", r$collectionCode),
      detail_field("N.º catálogo", r$catalogNumber),
      detail_field("Colector", r$recordedBy),
      detail_field("N.º colecta", r$recordNumber),
      detail_field("Determinó", r$identifiedBy),
      detail_field("Dataset", r$datasetID),
      detail_field("ID registro", r$recordID)
    )
  )
}

# Clase Bootstrap de un botón de dos estados (activo o no).
step_btn_class <- function(active, tone) {
  paste("btn", if (active) paste0("btn-", tone) else paste0("btn-outline-", tone))
}

# Pregunta inicial: ¿está bien ubicado? Dos botones (no un radio): así no hay una respuesta
# marcada por defecto que se pudiera enviar sin querer.
well_placed_question_ui <- function(ns, step_wp) {
  div(
    class = "mb-2",
    tags$p(
      class = "mb-1 fw-semibold",
      "¿Considera que este registro está bien ubicado geográficamente?"
    ),
    div(
      class = "btn-group btn-group-sm", role = "group",
      actionButton(ns("wp_si"), "Sí", class = step_btn_class(identical(step_wp, "si"), "success")),
      actionButton(ns("wp_no"), "No", class = step_btn_class(identical(step_wp, "no"), "danger"))
    )
  )
}

# Menú de qué está incorrecto, solo tras responder "No".
issue_menu_ui <- function(ns, step_issue) {
  div(
    class = "mb-2 mt-2",
    tags$p(class = "mb-1 fw-semibold", "¿Qué está incorrecto?"),
    div(
      class = "btn-group btn-group-sm", role = "group",
      actionButton(ns("issue_ubicacion"), "Ubicación geográfica",
        class = step_btn_class(identical(step_issue, "ubicacion"), "primary")
      ),
      actionButton(ns("issue_region"), "Región neotrópica",
        class = step_btn_class(identical(step_issue, "region"), "primary")
      )
    )
  )
}

# Departamento (desplegable si hay referencia para el país del registro; si no, texto libre),
# municipalidad (siempre texto libre: no hay un listado mundial fiable y liviano) y un campo
# adicional de texto libre para precisar la ubicación correcta.
location_fields_ui <- function(ns, departments, max_chars) {
  dept_input <- if (length(departments)) {
    selectInput(ns("department"), "Departamento / Estado / Provincia",
      choices = c("Seleccione…" = "", departments)
    )
  } else {
    textInput(ns("department"), "Departamento / Estado / Provincia (escríbalo)")
  }
  area <- textAreaInput(ns("detail"), "Detalle adicional (opcional)",
    rows = 2, width = "100%", placeholder = "Precisiones sobre la ubicación correcta…"
  )
  area <- htmltools::tagQuery(area)$find("textarea")$addAttrs(maxlength = max_chars)$allTags()
  tagList(dept_input, textInput(ns("municipality"), "Municipalidad"), area)
}

# Región neotrópica (provincias biogeográficas de Morrone, 2014; el mismo límite del mapa).
region_field_ui <- function(ns, regions) {
  if (length(regions)) {
    selectInput(ns("region"), "Región del Neotrópico (Morrone, 2014)",
      choices = c("Seleccione…" = "", regions)
    )
  } else {
    textInput(ns("region"), "Región del Neotrópico (escríbala)")
  }
}

# Identidad del revisor: se pide siempre al cerrar la verificación (esté o no bien ubicado).
identity_fields_ui <- function(ns) {
  tagList(
    tags$hr(),
    textInput(ns("reviewer_name"), "Su nombre"),
    textInput(ns("reviewer_profession"), "Su profesión (incluya doctorado, si aplica)")
  )
}

# Formulario completo de verificación, construido según lo que ya se respondió.
verify_form_ui <- function(ns, n, step_wp, step_issue, departments, regions, max_chars) {
  ready <- identical(step_wp, "si") || (identical(step_wp, "no") && !is.null(step_issue))
  tagList(
    tags$hr(),
    tags$p(class = "small text-body-secondary mb-2", sprintf("Verificaciones registradas: %d", n)),
    well_placed_question_ui(ns, step_wp),
    if (identical(step_wp, "no")) issue_menu_ui(ns, step_issue),
    if (identical(step_issue, "ubicacion")) location_fields_ui(ns, departments, max_chars),
    if (identical(step_issue, "region")) region_field_ui(ns, regions),
    if (ready) identity_fields_ui(ns),
    if (ready) submit_button_ui(ns)
  )
}

# Botón de enviar el formulario de verificación.
submit_button_ui <- function(ns) {
  div(
    class = "mt-2",
    actionButton(ns("verify_submit"), "Enviar verificación",
      icon = icon("paper-plane"), class = "btn-primary btn-sm"
    )
  )
}

#' Interfaz del detalle de registro y su verificación de ubicación
#'
#' @param id Identificador del módulo.
#' @return Etiquetas HTML.
mod_detail_ui <- function(id) {
  ns <- NS(id)
  card(
    card_header(bsicons::bs_icon("card-text"), " Registro seleccionado"),
    uiOutput(ns("info")), uiOutput(ns("verify"))
  )
}

# Limpia los campos de texto del formulario (los pasos y botones se resetean aparte).
reset_verify_inputs <- function(session) {
  ids <- c("department", "municipality", "detail", "region", "reviewer_name", "reviewer_profession")
  for (id in ids) updateTextInput(session, id, value = "")
}

# Payload crudo del formulario, tal como lo entiende `build_verdict()` (Modelo).
collect_payload <- function(input, step_wp, step_issue) {
  list(
    well_placed = identical(step_wp, "si"), issue_type = step_issue,
    department = input$department, municipality = input$municipality, detail = input$detail,
    neotropical_region = input$region, reviewer_name = input$reviewer_name,
    reviewer_profession = input$reviewer_profession
  )
}

#' Servidor del detalle de registro y su verificación de ubicación
#'
#' @param id Identificador del módulo.
#' @param record Reactivo con la fila del registro seleccionado (o `NULL`).
#' @param verification Controlador de verificación ([verification_controller()]).
#' @param max_chars Longitud máxima del campo de detalle adicional.
#' @return Nada: registra las salidas y observadores del módulo.
mod_detail_server <- function(id, record, verification, max_chars) {
  moduleServer(id, function(input, output, session) {
    placeholder <- tags$p(
      class = "text-body-secondary mb-0",
      "Seleccione un punto del mapa o una fila de la tabla para ver el detalle del registro ",
      "y verificar su ubicación."
    )
    step_wp <- reactiveVal(NULL)
    step_issue <- reactiveVal(NULL)
    reset_steps <- function() {
      step_wp(NULL)
      step_issue(NULL)
      reset_verify_inputs(session)
    }

    observeEvent(record(), reset_steps(), ignoreNULL = FALSE)
    observeEvent(input$wp_si, step_wp("si"))
    observeEvent(input$wp_no, step_wp("no"))
    observeEvent(input$issue_ubicacion, step_issue("ubicacion"))
    observeEvent(input$issue_region, step_issue("region"))

    output$info <- renderUI({
      r <- record()
      if (is.null(r)) placeholder else record_info_ui(r)
    })

    output$verify <- renderUI({
      r <- record()
      if (is.null(r)) {
        return(NULL)
      }
      if (!verification$can_verify()) {
        return(tags$p(class = "small text-body-secondary", "Su rol es de solo lectura."))
      }
      verify_form_ui(
        session$ns, verification$count(r$record_key), step_wp(), step_issue(),
        verification$departments_for(r$countryCode), verification$regions, max_chars
      )
    })

    observeEvent(input$verify_submit, {
      payload <- collect_payload(input, step_wp(), step_issue())
      if (isTRUE(verification$submit(payload))) reset_steps()
    })
  })
}
