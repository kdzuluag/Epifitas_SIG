# Formatea con styler (estilo tidyverse), normaliza finales de línea a LF y revisa con lintr.
#   Rscript scripts/style_and_lint.R          # formatea y luego revisa
#   Rscript scripts/style_and_lint.R --check  # solo revisa (falla si hay avisos)
args <- commandArgs(trailingOnly = TRUE)
targets <- c("app.R", "R", "scripts", "tests", "deploy")

# styler puede escribir CRLF en Windows: se fuerza LF (el proyecto usa LF, ver .gitattributes).
normalize_eol <- function(path) {
  files <- if (dir.exists(path)) {
    list.files(path, "\\.[Rr]$", recursive = TRUE, full.names = TRUE)
  } else {
    path
  }
  for (f in files) {
    raw <- readBin(f, "raw", file.size(f))
    if (any(raw == as.raw(13))) {
      writeBin(raw[raw != as.raw(13)], f)
    }
  }
}

if (!"--check" %in% args) {
  for (t in targets) {
    if (dir.exists(t)) styler::style_dir(t, filetype = "R") else styler::style_file(t)
    normalize_eol(t)
  }
}
lints <- do.call(rbind, lapply(targets, function(t) {
  l <- if (dir.exists(t)) lintr::lint_dir(t, pattern = "\\.[Rr]$") else lintr::lint(t)
  if (length(l)) as.data.frame(l)[, c("filename", "line_number", "linter", "message")] else NULL
}))
if (is.null(lints) || !nrow(lints)) {
  cat("lintr: 0 avisos\n")
} else {
  print(lints, row.names = FALSE, right = FALSE)
  cat(sprintf("\nlintr: %d aviso(s)\n", nrow(lints)))
  quit(status = 1)
}
