# Renderiza la documentación Quarto (metodología, arquitectura, calidad de datos y QA).
# La metodología se guarda en www/docs y se incrusta en la pestaña "Acerca de";
# los demás documentos se guardan en docs/out. Calidad de datos y QA requieren data/processed.
# Ejecutar desde la raíz del proyecto:  Rscript scripts/03_render_docs.R

if (!nzchar(Sys.getenv("QUARTO_R"))) {
  Sys.setenv(QUARTO_R = file.path(R.home("bin"), "Rscript.exe"))
}
quarto <- Sys.which("quarto")
if (!nzchar(quarto)) stop("Quarto no está en el PATH (https://quarto.org).")

dir.create("www/docs", recursive = TRUE, showWarnings = FALSE)
dir.create("docs/out", recursive = TRUE, showWarnings = FALSE)

render <- function(qmd, dest_dir) {
  message("Renderizando ", qmd)
  status <- system2(quarto, c("render", qmd, "--quiet"))
  if (status != 0) stop("Falló el render de ", qmd)
  html <- sub("\\.qmd$", ".html", qmd)
  file.copy(html, file.path(dest_dir, basename(html)), overwrite = TRUE)
  unlink(html)
}

render("docs/metodologia.qmd", "www/docs")
render("docs/arquitectura.qmd", "docs/out")
if (file.exists("data/processed/occurrences.rds")) {
  render("docs/calidad_datos.qmd", "docs/out")
  render("docs/qa.qmd", "docs/out")
}
message("Listo.")
