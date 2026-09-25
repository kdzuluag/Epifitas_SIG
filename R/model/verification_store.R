# Persistencia de las verificaciones taxonómicas en SQLite (escrituras concurrentes seguras,
# consultas parametrizadas). En hostings con disco efímero (shinyapps.io / Connect Cloud) el
# archivo se pierde al reiniciar: use un volumen persistente (self-host) o cambie solo
# `vs_open()` por una BD externa.

# Valores posibles de la columna `issue_type` (solo cuando `well_placed` es falso).
ISSUE_TYPES <- c("ubicacion", "region")

#' Abre (y crea si hace falta) la base de verificaciones
#'
#' @param path Ruta del archivo SQLite.
#' @return Conexión DBI abierta (el llamador debe cerrarla).
vs_open <- function(path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbGetQuery(con, "PRAGMA journal_mode = WAL;")
  DBI::dbGetQuery(con, "PRAGMA busy_timeout = 5000;")
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS verifications (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      record_key TEXT NOT NULL,
      reviewer_user TEXT NOT NULL,
      reviewer_name TEXT NOT NULL,
      reviewer_profession TEXT NOT NULL,
      well_placed INTEGER NOT NULL,
      issue_type TEXT,
      department TEXT,
      municipality TEXT,
      detail TEXT,
      neotropical_region TEXT,
      created_utc TEXT NOT NULL)")
  DBI::dbExecute(
    con, "CREATE INDEX IF NOT EXISTS idx_verifications_record ON verifications(record_key)"
  )
  con
}

# Ejecuta `fn(con)` con una conexión que se cierra siempre al terminar.
with_vs <- function(path, fn) {
  con <- vs_open(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  fn(con)
}

#' Guarda una verificación de ubicación
#'
#' @param path Ruta de la base.
#' @param record_key Clave del registro verificado.
#' @param reviewer_user Identidad de inicio de sesión (auditoría).
#' @param verdict Lista ya saneada y validada: `well_placed` (lógico), `reviewer_name`,
#'   `reviewer_profession` y, cuando `well_placed` es falso, `issue_type` (uno de
#'   [ISSUE_TYPES]) y sus campos (`department`/`municipality`/`detail`, o `neotropical_region`).
#' @return Número de filas insertadas.
vs_add <- function(path, record_key, reviewer_user, verdict) {
  created <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  cols <- c(
    "record_key", "reviewer_user", "reviewer_name", "reviewer_profession", "well_placed",
    "issue_type", "department", "municipality", "detail", "neotropical_region", "created_utc"
  )
  vals <- list(
    record_key, reviewer_user, verdict$reviewer_name, verdict$reviewer_profession,
    as.integer(verdict$well_placed), verdict$issue_type %||% NA_character_,
    verdict$department %||% NA_character_, verdict$municipality %||% NA_character_,
    verdict$detail %||% NA_character_, verdict$neotropical_region %||% NA_character_, created
  )
  with_vs(path, function(con) {
    ph <- paste(rep("?", length(cols)), collapse = ", ")
    sql <- sprintf("INSERT INTO verifications (%s) VALUES (%s)", paste(cols, collapse = ", "), ph)
    DBI::dbExecute(con, sql, params = vals)
  })
}

#' Número de verificaciones por registro
#'
#' @param path Ruta de la base.
#' @return `data.frame` con `record_key` y `n`.
vs_counts <- function(path) {
  with_vs(path, function(con) {
    DBI::dbGetQuery(
      con, "SELECT record_key, COUNT(*) AS n FROM verifications GROUP BY record_key"
    )
  })
}

#' Número de verificaciones de un registro concreto
#'
#' @param path Ruta de la base.
#' @param record_key Clave del registro.
#' @return Entero (0 si no tiene ninguna).
vs_count_one <- function(path, record_key) {
  n <- with_vs(path, function(con) {
    DBI::dbGetQuery(
      con, "SELECT COUNT(*) AS n FROM verifications WHERE record_key = ?",
      params = list(record_key)
    )
  })
  as.integer(n$n)
}
