# Vista: tema, paleta accesible (Okabe-Ito + Tol, apta para daltonismo) y tema ggplot.

PALETTE_CAT <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00",
  "#CC79A7", "#882255", "#44AA99", "#DDCC77", "#AA4499", "#117733"
)
OTHER_COLOR <- "#9AA0A6"
BRAND_GREEN <- "#2B2D42" # índigo pizarra del encabezado
ACCENT_TEAL <- "#8A5A00" # ámbar oscuro
ACCENT_RED <- "#7B2D8E" # ciruela

#' Tema Bootstrap 5 de la aplicación
#'
#' @return Objeto `bslib::bs_theme`.
epig_theme <- function() {
  system_fonts <- bslib::font_collection(
    "Lato", "system-ui", "-apple-system", "Segoe UI", "Roboto", "Helvetica Neue", "Arial",
    "sans-serif"
  )
  bslib::bs_theme(
    version = 5, primary = ACCENT_TEAL, secondary = "#5F6B63", success = "#2E7D32",
    danger = ACCENT_RED, "body-bg" = "#F7F5F0", "font-size-base" = "0.95rem",
    base_font = system_fonts, heading_font = system_fonts
  )
}

#' Tema ggplot2 sobrio para los gráficos
#'
#' @param base_size Tamaño base de fuente.
#' @return Objeto `ggplot2::theme`.
theme_epig <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(colour = "#444"),
      axis.text = ggplot2::element_text(colour = "#222"),
      plot.margin = ggplot2::margin(6, 12, 6, 6)
    )
}

#' Agrupa las categorías poco frecuentes en "Otros"
#'
#' @param x Vector de categorías.
#' @param max_levels Número máximo de categorías distintas de "Otros".
#' @return Factor con las categorías más frecuentes (y "Otros" si hace falta).
collapse_levels <- function(x, max_levels = length(PALETTE_CAT)) {
  x <- as.character(x)
  x[is.na(x)] <- "(sin dato)"
  keep <- utils::head(names(sort(table(x), decreasing = TRUE)), max_levels)
  x[!x %in% keep] <- "Otros"
  factor(x, levels = c(keep, if (any(x == "Otros")) "Otros"))
}

#' Color por categoría (paleta apta para daltonismo; "Otros" en gris)
#'
#' @param levels Categorías.
#' @return Vector de colores con nombres.
category_colors <- function(levels) {
  cols <- PALETTE_CAT[seq_len(min(length(levels), length(PALETTE_CAT)))]
  cols <- c(cols, rep(OTHER_COLOR, length(levels) - length(cols)))
  cols[levels == "Otros"] <- OTHER_COLOR
  stats::setNames(cols, levels)
}

#' Gráfico vacío con un mensaje
#'
#' @param msg Texto a mostrar.
#' @return Objeto `ggplot`.
empty_plot <- function(msg) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = msg, colour = "#666", size = 5) +
    ggplot2::theme_void()
}

#' Gráfico de barras horizontales con etiquetas (base de los gráficos de la app)
#'
#' @param d `data.frame` con las columnas indicadas en `x` e `y`.
#' @param x,y Nombres de las columnas de valor y de categoría (en el orden en que vienen).
#' @param fill Color de las barras.
#' @param x_lab Título del eje de valores.
#' @param italic Si `TRUE`, las categorías van en cursiva (género y especie).
#' @param label_fn Función que da formato a las etiquetas de valor.
#' @param inside Si `TRUE`, la etiqueta va dentro de la barra (en blanco).
#' @param x_limits Límites del eje de valores (opcional).
#' @param size Tamaño de fuente base.
#' @return Objeto `ggplot`.
hbar_plot <- function(d, x, y, fill = BRAND_GREEN, x_lab = NULL, italic = FALSE,
                      label_fn = fmt_int, inside = FALSE, x_limits = NULL, size = 12) {
  d[[y]] <- factor(d[[y]], levels = rev(unique(d[[y]])))
  expansion <- if (inside) c(0, 0) else c(0, 0.16)
  ggplot2::ggplot(d, ggplot2::aes(.data[[x]], .data[[y]])) +
    ggplot2::geom_col(fill = fill, width = 0.72) +
    ggplot2::geom_text(
      ggplot2::aes(label = label_fn(.data[[x]])),
      hjust = if (inside) 1.1 else -0.15, size = 3.6, colour = if (inside) "white" else "#222"
    ) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = expansion), limits = x_limits, labels = label_fn
    ) +
    ggplot2::labs(x = x_lab, y = NULL) +
    theme_epig(size) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(face = if (italic) "italic" else "plain"))
}
