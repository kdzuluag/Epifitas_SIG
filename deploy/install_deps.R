# Instala las dependencias de R declaradas en DESCRIPTION (Imports + Suggests).
# Para versiones exactas use renv:  renv::restore()  (lee renv.lock)
desc <- read.dcf("DESCRIPTION", fields = c("Imports", "Suggests"))
pk <- trimws(unlist(strsplit(gsub("\n", " ", desc), ",")))
pk <- unique(pk[nzchar(pk) & !is.na(pk)])

miss <- setdiff(pk, rownames(installed.packages()))
if (length(miss)) {
  install.packages(miss, repos = "https://cloud.r-project.org")
} else {
  message("Todo instalado.")
}
