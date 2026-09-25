# Congela las versiones exactas de las dependencias declaradas en DESCRIPTION en renv.lock.
# No activa renv en el proyecto. Para reproducir el entorno en otra máquina: renv::restore()
# Ejecutar desde la raíz:  Rscript scripts/snapshot_deps.R
renv::snapshot(
  project = ".", lockfile = "renv.lock", type = "explicit", prompt = FALSE, force = TRUE
)
