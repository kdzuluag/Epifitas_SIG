# Genera data/processed/admin1.rds: nombres de departamento/estado/provincia (nivel
# administrativo 1) por país del Neotrópico, para el menú desplegable de la verificación
# taxonómica ("¿qué está incorrecto? -> ubicación geográfica"). Solo nombres, sin geometría
# (pesa unos KB) y sin ningún dato de ocurrencias. Es un paso manual y puntual (exige
# conexión a internet la primera vez); su resultado se versiona como cualquier otro dato
# procesado.
#   Rscript scripts/01b_build_admin1.R
source("scripts/load_project.R")
load_project("core")

if (!requireNamespace("rnaturalearth", quietly = TRUE)) {
  stop("Instale rnaturalearth y rnaturalearthhires para regenerar este archivo.", call. = FALSE)
}

# Fuente: Natural Earth, admin-1 (10 m), dominio público. Se usa el nombre en español
# (`name_es`) y se recurre al nombre en inglés si falta.
codes <- names(NEOTROPIC_COUNTRIES)
raw <- rnaturalearth::ne_states(iso_a2 = codes, returnclass = "sf")
d <- as.data.frame(raw)
d$geometry <- NULL
d$department <- ifelse(!is.na(d$name_es) & nzchar(d$name_es), d$name_es, d$name)
d <- d[!is.na(d$department) & nzchar(d$department), c("iso_a2", "department")]
names(d)[1] <- "countryCode"
d <- unique(d) # Natural Earth traduce a veces dos unidades distintas al mismo nombre
d <- d[order(d$countryCode, d$department), ]

# La Guayana Francesa es un departamento único de Francia: Natural Earth no la subdivide.
if (!"GF" %in% d$countryCode) {
  d <- rbind(d, data.frame(countryCode = "GF", department = "Guayana Francesa"))
}
missing <- setdiff(codes, unique(d$countryCode))
if (length(missing)) {
  message("Sin departamentos para: ", paste(missing, collapse = ", "))
}

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
saveRDS(d, "data/processed/admin1.rds")
cat(sprintf(
  "OK: %d departamentos/estados/provincias en %d países (fuente: Natural Earth admin-1).\n",
  nrow(d), length(unique(d$countryCode))
))
