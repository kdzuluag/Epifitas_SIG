# Usuarios locales: contraseñas con hash (bcrypt-pbkdf de OpenSSL, sal aleatoria por usuario) en
# SQLite. Nunca se guarda ni se registra la contraseña en claro.

PW_ROUNDS <- 64L # coste del KDF (~0,3 s por intento): encarece la fuerza bruta
PW_MIN_CHARS <- 10L
USERNAME_REGEX <- "^[A-Za-z0-9._-]{3,40}$"
USER_ROLES <- c("reviewer", "admin")

#' Calcula el hash de una contraseña con sal aleatoria
#'
#' @param password Contraseña en texto plano.
#' @param salt Sal (bytes); aleatoria por defecto (se inyecta para probar).
#' @param rounds Coste del KDF.
#' @return Texto `"bcrypt-pbkdf$<rondas>$<sal hex>$<hash hex>"`.
hash_password <- function(password, salt = openssl::rand_bytes(16), rounds = PW_ROUNDS) {
  key <- openssl::bcrypt_pbkdf(
    charToRaw(enc2utf8(password)), salt,
    rounds = as.integer(rounds), size = 32L
  )
  paste("bcrypt-pbkdf", rounds, paste(salt, collapse = ""), paste(key, collapse = ""), sep = "$")
}

#' Verifica una contraseña contra un hash guardado
#'
#' @param password Contraseña en texto plano.
#' @param stored Hash producido por [hash_password()].
#' @return `TRUE` si coincide; `FALSE` si no o si el hash está mal formado.
verify_password <- function(password, stored) {
  parts <- strsplit(as.character(stored), "$", fixed = TRUE)[[1]]
  if (length(parts) != 4 || parts[1] != "bcrypt-pbkdf") {
    return(FALSE)
  }
  hex <- parts[3]
  starts <- seq(1, nchar(hex), 2)
  salt <- as.raw(strtoi(substring(hex, starts, starts + 1), 16L))
  candidate <- hash_password(password, salt, as.integer(parts[2]))
  identical(digest::digest(candidate), digest::digest(stored))
}

#' Abre (y crea si hace falta) la base de usuarios
#'
#' @param path Ruta del archivo SQLite.
#' @return Conexión DBI abierta (el llamador debe cerrarla).
users_open <- function(path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS users (
      username TEXT PRIMARY KEY COLLATE NOCASE,
      pw_hash TEXT NOT NULL,
      role TEXT NOT NULL,
      created_utc TEXT NOT NULL)")
  con
}

#' Crea o actualiza un usuario
#'
#' @param path Ruta de la base de usuarios.
#' @param username Nombre de usuario (3-40 caracteres: letras, números, `._-`).
#' @param password Contraseña (mínimo [PW_MIN_CHARS] caracteres).
#' @param role `"reviewer"` o `"admin"`.
#' @return `TRUE` de forma invisible; detiene la ejecución si los datos no son válidos.
users_add <- function(path, username, password, role = "reviewer") {
  stopifnot(
    grepl(USERNAME_REGEX, username), nchar(password) >= PW_MIN_CHARS, role %in% USER_ROLES
  )
  con <- users_open(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(
    con, "INSERT OR REPLACE INTO users (username, pw_hash, role, created_utc) VALUES (?, ?, ?, ?)",
    params = list(username, hash_password(password), role, format(Sys.time(), tz = "UTC"))
  )
  invisible(TRUE)
}

#' Autentica a un usuario
#'
#' Si el usuario no existe se calcula igualmente un hash, para que el tiempo de respuesta no
#' revele qué usuarios existen.
#'
#' @param path Ruta de la base de usuarios.
#' @param username Nombre de usuario.
#' @param password Contraseña.
#' @return Lista con `ok`, `user`, `role` y `reason` (como [resolve_user()]).
users_authenticate <- function(path, username, password) {
  username <- substr(as.character(username %||% ""), 1, 100)
  password <- substr(as.character(password %||% ""), 1, 200)
  if (!file.exists(path)) {
    return(deny_access("sin_usuarios"))
  }
  con <- users_open(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  row <- DBI::dbGetQuery(con, "SELECT username, pw_hash, role FROM users WHERE username = ?",
    params = list(username)
  )
  stored <- if (nrow(row)) row$pw_hash else hash_password("sin-usuario-relleno")
  ok <- verify_password(password, stored) && nrow(row) == 1
  if (ok) allow_access(row$username, row$role) else deny_access("credenciales_invalidas")
}
