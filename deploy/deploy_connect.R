# Despliegue a Posit Connect / Connect Cloud. NO probado en este entorno.
# Solo se publican los archivos necesarios: NUNCA data/raw ni data/private.
# Defina las variables de entorno en el panel de Connect: EPIG_AUTH_MODE=connect,
# EPIG_ENV=production y los grupos permitidos.
# Advertencia: el plan gratuito publica el contenido como público; use un plan con autenticación.
rsconnect::deployApp(
  appDir = ".",
  appFiles = c(
    "app.R", list.files("R", recursive = TRUE, full.names = TRUE), "www/custom.css",
    list.files("www/docs", full.names = TRUE), "data/processed/occurrences.rds",
    "data/processed/neotropic.rds", "data/processed/bioregion_mask.rds"
  ),
  appName = "epig-data-visualizer",
  forceUpdate = TRUE
)
