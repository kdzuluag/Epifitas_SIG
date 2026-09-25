# Crea o actualiza un usuario de la aplicación (modo de autenticación "local").
#   Rscript scripts/04_add_user.R <usuario> [reviewer|admin]
# La contraseña se lee de la entrada estándar (o de EPIG_NEW_PASSWORD) y NUNCA se guarda en
# claro: solo su hash, en EPIG_USERS_DB (por defecto data/private/users.sqlite).
source("scripts/load_project.R")
load_project(c("core", "model"))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  stop("Uso: Rscript scripts/04_add_user.R <usuario> [reviewer|admin]", call. = FALSE)
}
password <- Sys.getenv("EPIG_NEW_PASSWORD", "")
if (!nzchar(password)) {
  cat(sprintf("Contraseña (mínimo %d caracteres): ", PW_MIN_CHARS))
  password <- readLines(file("stdin"), n = 1)
}
role <- if (length(args) >= 2) args[2] else "reviewer"
users_add(epig_config()$users_db, args[1], password, role)
cat(sprintf("Usuario '%s' (%s) guardado.\n", args[1], role))
