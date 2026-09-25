# Vista: acerca de, privacidad y metodología (HTML generado con Quarto, docs/metodologia.qmd).

METHODOLOGY_HTML <- "www/docs/metodologia.html"

# Tarjeta con la política de privacidad y uso responsable.
privacy_card <- function(meta) {
  card(
    card_header(bsicons::bs_icon("shield-lock"), " Privacidad y uso responsable"),
    tags$ul(
      tags$li(
        "Herramienta de visualización y revisión para taxónomos y especialistas en epífitas ",
        "del Neotrópico."
      ),
      tags$li(sprintf(
        paste0(
          "Las ubicaciones se generalizan a celdas de %s° (≈%d km); las coordenadas exactas ",
          "no forman parte de esta aplicación."
        ),
        format(meta$grid_deg), round(meta$grid_deg * 111)
      )),
      tags$li(
        "No hay descarga de datos. Se registra la actividad (accesos, filtros, verificaciones) ",
        "con fines de auditoría."
      ),
      tags$li(
        "Cite a los colectores e instituciones de cada registro; no redistribuya el contenido ",
        "sin autorización de la coordinación del proyecto."
      ),
      tags$li(
        "Al verificar un punto se registra su nombre y profesión junto con el veredicto; no ",
        "reemplaza una determinación taxonómica formal."
      )
    )
  )
}

# Tarjeta con la versión y el estándar de los datos.
data_card <- function(repo) {
  meta <- repo$meta
  card(
    card_header(bsicons::bs_icon("database"), " Datos"),
    tags$dl(
      class = "row mb-0",
      tags$dt(class = "col-5", "Registros"), tags$dd(class = "col-7", fmt_int(nrow(repo$occ))),
      tags$dt(class = "col-5", "Versión de los datos"), tags$dd(class = "col-7", meta$built_at),
      tags$dt(class = "col-5", "Estándar"),
      tags$dd(class = "col-7", "Darwin Core (subconjunto de 20 campos)"),
      tags$dt(class = "col-5", "Referencia espacial"),
      tags$dd(class = "col-7", "WGS 84 (EPSG:4326)"),
      tags$dt(class = "col-5", "Límite del Neotrópico"),
      tags$dd(class = "col-7", "Morrone (2014), shapefile de Löwenberg-Neto (2014), CC BY 4.0"),
      tags$dt(class = "col-5", "Generalización"),
      tags$dd(class = "col-7", "Cuadrícula (ver metodología)")
    )
  )
}

# Tarjeta con la metodología (HTML de Quarto incrustado) o un aviso si no existe.
methodology_card <- function() {
  body <- if (file.exists(METHODOLOGY_HTML)) {
    tags$iframe(
      src = "docs/metodologia.html", title = "Metodología",
      style = "width:100%;height:100%;min-height:440px;border:0;"
    )
  } else {
    tags$p("Genere la metodología con scripts/03_render_docs.R.")
  }
  card(
    card_header(bsicons::bs_icon("journal-text"), " Metodología"),
    full_screen = TRUE, min_height = 480, body
  )
}

#' Contenido de la pestaña "Acerca de"
#'
#' @param repo Repositorio de datos.
#' @param cfg Configuración.
#' @return Etiquetas HTML.
mod_about_ui <- function(repo, cfg) {
  tagList(
    layout_columns(col_widths = c(6, 6), privacy_card(repo$meta), data_card(repo)),
    methodology_card(),
    site_footer_ui()
  )
}
