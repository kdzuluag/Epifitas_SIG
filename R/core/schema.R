# Esquema de datos (Darwin Core) y constantes del dominio (taxonomía, geografía, ecología).

#' Campos Darwin Core que debe traer el archivo de entrada (20 columnas)
DWC_COLS <- c(
  "datasetID", "recordID", "order", "family", "genus", "speciesName", "scientificName",
  "countryCode", "locality", "decimalLatitude", "decimalLongitude", "verbatimElevation",
  "eventDate", "basisOfRecord", "institutionCode", "collectionCode", "catalogNumber",
  "recordedBy", "recordNumber", "identifiedBy"
)
COORD_COLS <- c("decimalLatitude", "decimalLongitude")

TAX_LEVELS <- c(Orden = "order", Familia = "family", `Género` = "genus", Especie = "speciesName")
TAX_COLS_SEARCH <- c("order", "family", "genus", "speciesName", "scientificName")

# Pisos altitudinales neotropicales (criterio práctico para epífitas)
ELEV_BREAKS <- c(-Inf, 1000, 2000, 3000, Inf)
ELEV_LABELS <- c(
  "Tierras bajas (<1000 m)", "Montano bajo (1000-2000 m)",
  "Montano alto (2000-3000 m)", "Altoandino (>3000 m)"
)

# Límites de validación de datos (un solo lugar para cambiarlos)
YEAR_MIN <- 1700L # antes de esta fecha una colecta no es plausible
ELEV_ATYPICAL_MAX <- 4500 # las epífitas vasculares rara vez superan esta elevación
ELEV_ATYPICAL_MIN <- 0
LAT_LIMIT <- 90
LON_LIMIT <- 180

# Cajón geográfico plausible del Neotrópico: fuera de él una coordenada se marca como sospechosa
NEO_BBOX <- c(xmin = -120, ymin = -58, xmax = -30, ymax = 33)

# Columnas visibles en la tabla (nunca coordenadas)
TABLE_COLS <- c(
  "datasetID", "recordID", "order", "family", "genus", "speciesName", "scientificName",
  "countryCode", "locality", "verbatimElevation", "eventDate", "basisOfRecord",
  "institutionCode", "collectionCode", "catalogNumber", "recordedBy",
  "recordNumber", "identifiedBy", "quality_flags"
)
TABLE_LABELS <- c(
  "Dataset", "ID registro", "Orden", "Familia", "Género", "Especie",
  "Nombre científico", "País", "Localidad", "Elevación (verbatim)", "Fecha",
  "Tipo de registro", "Institución", "Colección", "N.º catálogo", "Colector",
  "N.º colecta", "Determinó", "Alertas de calidad"
)

QUALITY_FLAGS <- c(
  taxon_incompleto = "Falta familia, género o especie",
  genero_no_coincide = "El género no coincide con el nombre de la especie",
  genero_multiples_familias = "La familia difiere de la más frecuente para ese género",
  pais_invalido = "Código de país no válido",
  elevacion_atipica = sprintf(
    "Elevación atípica (<%d o >%d m)", ELEV_ATYPICAL_MIN, as.integer(ELEV_ATYPICAL_MAX)
  ),
  fecha_invalida = "Fecha no interpretable o fuera de rango",
  coordenadas_invalidas = "Coordenadas no válidas (no se muestran)",
  coordenadas_fuera_neotropico = "Coordenadas fuera del Neotrópico (no se muestran)",
  recordID_duplicado = "recordID duplicado"
)

NEOTROPIC_COUNTRIES <- c(
  AR = "Argentina", BO = "Bolivia", BR = "Brasil", BZ = "Belice", CL = "Chile", CO = "Colombia",
  CR = "Costa Rica", CU = "Cuba", DO = "Rep. Dominicana", EC = "Ecuador",
  GF = "Guayana Francesa", GT = "Guatemala", GY = "Guyana", HN = "Honduras", HT = "Haití",
  JM = "Jamaica", MX = "México", NI = "Nicaragua", PA = "Panamá", PE = "Perú",
  PR = "Puerto Rico", PY = "Paraguay", SR = "Surinam", SV = "El Salvador",
  TT = "Trinidad y Tobago", UY = "Uruguay", VE = "Venezuela"
)

#' Nombre en español de un país a partir de su código ISO-2
#'
#' @param code Vector de códigos ISO-2.
#' @return Vector de nombres; si el código no está en el catálogo, se devuelve el mismo código.
country_name <- function(code) {
  out <- unname(NEOTROPIC_COUNTRIES[code])
  ifelse(is.na(out), code, out)
}
