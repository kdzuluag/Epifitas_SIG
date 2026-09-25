# Despliegue a shinyapps.io (modo de autenticación "local": usuario y contraseña propios).
#
# Antes de ejecutar este script:
#   1) Instale y autorice rsconnect UNA vez (con su token real, no lo comparta ni lo pegue en el
#      chat): vea shinyapps.io -> Account -> Tokens -> "Show" y corra en su consola de R el
#      rsconnect::setAccountInfo(...) que ahí aparece.
#   2) Cree su usuario local si aún no existe: Rscript scripts/04_add_user.R su_usuario admin
#
# shinyapps.io no da acceso a consola ni permite ejecutar scripts en el servidor: la ÚNICA forma
# de que exista una cuenta allá es incluir aquí el archivo de usuarios ya creado. El disco del
# servidor es efímero (se reinicia el proceso, no por publicación): cualquier usuario o
# verificación creados DESPUÉS de publicar se pierden en el próximo reinicio. Para producción
# real (varios revisores, persistencia garantizada) use EPIG_AUTH_MODE=connect
# (deploy/deploy_connect.R) o self-host con un volumen persistente (ver docs/arquitectura.qmd).
stopifnot(file.exists("data/private/users.sqlite")) # cree su usuario primero (paso 2 arriba)

rsconnect::deployApp(
  appDir = ".",
  appFiles = c(
    "app.R", "DESCRIPTION", list.files("R", recursive = TRUE, full.names = TRUE),
    "www/custom.css", list.files("www/docs", full.names = TRUE),
    list.files("data/processed", full.names = TRUE), # occurrences, neotropic, mask, admin1, regions
    "data/private/users.sqlite"
  ),
  appName = "epig-data-visualizer",
  forceUpdate = TRUE
)
