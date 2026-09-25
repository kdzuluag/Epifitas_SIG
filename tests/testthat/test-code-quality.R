# Buenas prácticas de código verificadas automáticamente (ver CONTRIBUTING.md).

MAX_FUNCTION_LINES <- 50

# Funciones de nivel superior: nombre, argumentos, tamaño y comentario previo.
top_level_functions <- function(file) {
  exprs <- parse(file, keep.source = TRUE, encoding = "UTF-8")
  refs <- attr(exprs, "srcref")
  src <- readLines(file, warn = FALSE, encoding = "UTF-8")
  out <- list()
  for (i in seq_along(exprs)) {
    e <- exprs[[i]]
    is_fun <- is.call(e) && (identical(e[[1]], as.name("<-")) || identical(e[[1]], as.name("="))) &&
      is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function"))
    if (!is_fun) next
    first <- refs[[i]][1]
    above <- character(0)
    j <- first - 1
    while (j >= 1 && grepl("^\\s*#", src[j])) {
      above <- c(src[j], above)
      j <- j - 1
    }
    out[[length(out) + 1]] <- list(
      name = as.character(e[[2]]), file = basename(file),
      formals = names(formals(eval(e[[3]]))), n_lines = refs[[i]][3] - first + 1, above = above
    )
  }
  out
}

all_functions <- function() {
  files <- list.files(file.path(root, "R"), "\\.R$", recursive = TRUE, full.names = TRUE)
  unlist(lapply(files, top_level_functions), recursive = FALSE)
}

# Argumentos documentados en un bloque roxygen (admite "@param a,b texto").
documented_params <- function(block) {
  tags <- sub("^\\s*#'\\s*@param\\s+(\\S+).*$", "\\1", grep("@param\\s", block, value = TRUE))
  unlist(strsplit(tags, ","))
}

test_that("toda función de nivel superior tiene un comentario o bloque roxygen encima", {
  undocumented <- Filter(function(f) length(f$above) == 0, all_functions())
  names <- vapply(undocumented, function(f) paste0(f$file, "::", f$name), "")
  expect_length(names, 0)
  if (length(names)) fail(paste("Sin documentar:", paste(names, collapse = ", ")))
})

test_that("los bloques roxygen documentan todos los argumentos y el valor devuelto", {
  problems <- character(0)
  for (f in all_functions()) {
    if (!any(grepl("^\\s*#'", f$above))) next
    missing_params <- setdiff(f$formals, documented_params(f$above))
    if (length(missing_params)) {
      problems <- c(problems, sprintf(
        "%s::%s sin @param %s", f$file, f$name, paste(missing_params, collapse = ",")
      ))
    }
    if (!any(grepl("@return", f$above))) {
      problems <- c(problems, paste0(f$file, "::", f$name, " sin @return"))
    }
  }
  expect_length(problems, 0)
  if (length(problems)) fail(paste(problems, collapse = "\n"))
})

test_that(sprintf("ninguna función supera %d líneas", MAX_FUNCTION_LINES), {
  long <- Filter(function(f) f$n_lines > MAX_FUNCTION_LINES, all_functions())
  names <- vapply(long, function(f) sprintf("%s::%s (%d)", f$file, f$name, f$n_lines), "")
  expect_length(names, 0)
  if (length(names)) fail(paste("Demasiado largas:", paste(names, collapse = ", ")))
})

test_that("no se usa <<- ni se silencian errores con tryCatch(error = function(e) NULL)", {
  code <- read_code(r_files(c("R", "scripts"), "app.R"))
  bad_assign <- code[grepl("<<-", code$text, fixed = TRUE), ]
  silent_pattern <- "error\\s*=\\s*function\\(e\\)\\s*(NULL|NA|invisible\\(NULL\\))"
  silent <- code[grepl(silent_pattern, code$text), ]
  expect_equal(nrow(bad_assign), 0, info = paste(basename(bad_assign$file), bad_assign$line))
  expect_equal(nrow(silent), 0, info = paste(basename(silent$file), silent$line))
})

test_that("las dependencias usadas en el código están declaradas en DESCRIPTION", {
  desc <- read.dcf(file.path(root, "DESCRIPTION"), fields = c("Imports", "Suggests", "Depends"))
  declared <- trimws(unlist(strsplit(gsub("\n", " ", desc), ",")))
  declared <- sub("\\s*\\(.*$", "", declared)
  base_pkgs <- c("base", "stats", "utils", "methods", "tools", "graphics", "grDevices", "parallel")
  code <- read_code(r_files(c("R", "scripts", "deploy"), "app.R"))
  qualified <- gregexpr("[A-Za-z][A-Za-z0-9.]*(?=::)", code$text, perl = TRUE)
  attached <- gregexpr("(?<=library\\()[A-Za-z0-9.]+", code$text, perl = TRUE)
  used <- c(unlist(regmatches(code$text, qualified)), unlist(regmatches(code$text, attached)))
  missing <- setdiff(unique(used), c(declared, base_pkgs, "R"))
  expect_length(missing, 0)
  if (length(missing)) {
    fail(paste("Paquetes no declarados en DESCRIPTION:", paste(missing, collapse = ", ")))
  }
})

test_that("el proyecto trae los archivos de calidad y reproducibilidad", {
  # renv.lock NO se exige aquí a propósito: es solo referencia local (no se versiona, ver
  # .gitignore) porque rsconnect lo detecta por su sola presencia e intenta usarlo para
  # instalar paquetes en el servidor, rompiendo el despliegue.
  required <- c(
    "DESCRIPTION", ".lintr", ".gitignore", ".gitattributes", ".rscignore",
    "CONTRIBUTING.md", "README.md"
  )
  for (f in required) {
    expect_true(file.exists(file.path(root, f)), info = f)
  }
})

test_that("los archivos de texto usan finales de línea LF y terminan en salto de línea", {
  patterns <- paste0(
    "\\.(R|qmd|md|yml|css|conf)$|^Dockerfile$|^DESCRIPTION$|",
    "^\\.(lintr|gitignore|gitattributes)$"
  )
  files <- list.files(root, patterns, recursive = TRUE, full.names = TRUE, all.files = TRUE)
  files <- files[!grepl("[/\\\\](data|docs[/\\\\]out|www|renv)[/\\\\]", files)]
  crlf <- files[vapply(files, function(f) any(readBin(f, "raw", file.size(f)) == as.raw(13)), NA)]
  no_eol <- files[vapply(files, function(f) {
    r <- readBin(f, "raw", file.size(f))
    length(r) > 0 && r[length(r)] != as.raw(10)
  }, NA)]
  expect_length(crlf, 0)
  expect_length(no_eol, 0)
})
