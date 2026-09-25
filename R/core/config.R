# Configuración central. Todo lo sensible viene de variables de entorno, nunca del código.

# Lectores de variables de entorno con conversión y valor por defecto (cadena/número/lista).
env_readers <- function(env) {
  chr <- function(k, d = "") {
    v <- env(k, "")
    if (nzchar(v)) v else d
  }
  num <- function(k, d) {
    v <- suppressWarnings(as.numeric(chr(k, "")))
    if (is.na(v)) d else v
  }
  csv <- function(k) {
    v <- trimws(strsplit(chr(k, ""), ",", fixed = TRUE)[[1]])
    v[nzchar(v)]
  }
  list(chr = chr, num = num, csv = csv)
}

# Autenticación: local (usuario y contraseña; por defecto) | none (solo desarrollo) |
# connect (Posit Connect) | proxy (nginx + oauth2-proxy).
auth_config <- function(r) {
  list(
    auth_mode = r$chr("EPIG_AUTH_MODE", "local"),
    users_db = r$chr("EPIG_USERS_DB", "data/private/users.sqlite"),
    admin_groups = r$csv("EPIG_ADMIN_GROUPS"),
    admin_users = r$csv("EPIG_ADMIN_USERS"),
    allowed_groups = r$csv("EPIG_ALLOWED_GROUPS"),
    proxy_user_header = r$chr("EPIG_PROXY_USER_HEADER", "HTTP_X_FORWARDED_USER"),
    proxy_token_header = "HTTP_X_EPIG_PROXY_TOKEN",
    proxy_token = r$chr("EPIG_PROXY_TOKEN"),
    login_max_attempts = r$num("EPIG_LOGIN_MAX_ATTEMPTS", 5),
    login_window = r$num("EPIG_LOGIN_WINDOW", 300)
  )
}

# Rutas de datos procesados (sin coordenadas exactas) y de estado escrito por la app.
data_config <- function(r) {
  list(
    occ_path = r$chr("EPIG_OCC_PATH", "data/processed/occurrences.rds"),
    neotropic_path = r$chr("EPIG_NEOTROPIC_PATH", "data/processed/neotropic.rds"),
    mask_path = r$chr("EPIG_MASK_PATH", "data/processed/bioregion_mask.rds"),
    admin1_path = r$chr("EPIG_ADMIN1_PATH", "data/processed/admin1.rds"),
    regions_path = r$chr("EPIG_REGIONS_PATH", "data/processed/neotropic_regions.rds"),
    verification_db = r$chr("EPIG_VERIFICATION_DB", "data/private/verifications.sqlite"),
    audit_log = r$chr("EPIG_AUDIT_LOG", "data/private/audit.jsonl")
  )
}

# Límites defensivos de entrada (longitud, tasa, tamaño de listas).
limit_config <- function(r) {
  list(
    detail_max_chars = r$num("EPIG_DETAIL_MAX_CHARS", 500),
    identity_max_chars = r$num("EPIG_IDENTITY_MAX_CHARS", 150),
    verification_rate_max = r$num("EPIG_VERIFICATION_RATE_MAX", 5),
    verification_rate_window = r$num("EPIG_VERIFICATION_RATE_WINDOW", 60),
    max_filter_values = 200, max_search_chars = 100, max_search_tokens = 5, top_n_default = 15
  )
}

#' Construye la configuración de la aplicación
#'
#' Lee variables de entorno `EPIG_*`, aplica valores por defecto y valida la
#' combinación resultante (p. ej. prohíbe `auth_mode = "none"` en producción).
#'
#' @param env Función con la firma de `Sys.getenv(clave, valor_por_defecto)`;
#'   se inyecta para poder probar sin tocar el entorno real.
#' @return Lista con la configuración validada.
#' @examples
#' cfg <- epig_config(env = function(k, d = "") d)
epig_config <- function(env = Sys.getenv) {
  r <- env_readers(env)
  base <- list(
    app_title = "EpIG · Visualizador de registros de epífitas vasculares del Neotrópico",
    env_name = r$chr("EPIG_ENV", "development")
  )
  cfg <- c(base, auth_config(r), data_config(r), limit_config(r))
  validate_config(cfg)
}

#' Valida una configuración
#'
#' @param cfg Lista producida por [epig_config()].
#' @return `cfg` sin cambios; detiene la ejecución si es inválida.
validate_config <- function(cfg) {
  if (!cfg$auth_mode %in% c("local", "none", "connect", "proxy")) {
    stop("EPIG_AUTH_MODE debe ser 'local', 'none', 'connect' o 'proxy'.", call. = FALSE)
  }
  if (identical(cfg$env_name, "production") && identical(cfg$auth_mode, "none")) {
    stop("En producción EPIG_AUTH_MODE no puede ser 'none'.", call. = FALSE)
  }
  if (identical(cfg$auth_mode, "proxy") && !nzchar(cfg$proxy_token)) {
    stop("EPIG_AUTH_MODE='proxy' exige EPIG_PROXY_TOKEN.", call. = FALSE)
  }
  cfg
}
