# Carga las capas del proyecto en el entorno global (usado por los scripts, no por la app).

#' Carga los archivos R de las capas indicadas
#'
#' @param dirs Subcarpetas de `R/` a cargar, en orden.
#' @param root Raíz del proyecto.
#' @return `NULL` de forma invisible.
load_project <- function(dirs = c("core", "model"), root = ".") {
  for (d in dirs) {
    files <- sort(list.files(file.path(root, "R", d), "\\.R$", full.names = TRUE))
    for (f in files) source(f, local = globalenv(), encoding = "UTF-8")
  }
  invisible(NULL)
}
