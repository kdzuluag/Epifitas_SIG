# Genera datos SINTÉTICOS de demostración con el esquema real de 20 columnas (Darwin Core).
# Los nombres de especies son inventados: NO usar para ningún análisis taxonómico real.
# Con datos reales, ponga el .xlsx en data/raw/ y omita este script.

set.seed(2026)
n <- 6428L
n_sp <- 2095L

# ---- Taxonomía sintética (géneros reales de epífitas, epítetos inventados) ----
fam <- data.frame(
  family = c(
    "Orchidaceae", "Bromeliaceae", "Araceae", "Polypodiaceae", "Piperaceae",
    "Gesneriaceae", "Cactaceae", "Ericaceae", "Hymenophyllaceae"
  ),
  order = c(
    "Asparagales", "Poales", "Alismatales", "Polypodiales", "Piperales",
    "Lamiales", "Caryophyllales", "Ericales", "Hymenophyllales"
  ),
  w = c(.50, .15, .08, .08, .05, .05, .03, .04, .02),
  stringsAsFactors = FALSE
)
genera <- list(
  Orchidaceae = c(
    "Epidendrum", "Pleurothallis", "Maxillaria", "Lepanthes", "Stelis", "Oncidium",
    "Masdevallia", "Elleanthus", "Dichaea", "Trichosalpinx"
  ),
  Bromeliaceae = c(
    "Tillandsia", "Guzmania", "Vriesea", "Werauhia", "Aechmea", "Racinaea", "Catopsis"
  ),
  Araceae = c("Anthurium", "Philodendron", "Monstera", "Syngonium"),
  Polypodiaceae = c("Pleopeltis", "Campyloneurum", "Serpocaulon", "Niphidium"),
  Piperaceae = "Peperomia",
  Gesneriaceae = c("Columnea", "Drymonia", "Codonanthe"),
  Cactaceae = c("Epiphyllum", "Rhipsalis", "Disocactus"),
  Ericaceae = c("Cavendishia", "Macleania", "Psammisia"),
  Hymenophyllaceae = c("Hymenophyllum", "Trichomanes")
)
stems <- c(
  "alb", "ros", "vir", "lut", "nig", "rub", "aur", "cor", "mont", "silv", "long", "brev",
  "grand", "parv", "hirs", "glab", "mac", "micr", "tenu", "crass", "acut", "obt", "rept",
  "pend", "scand", "fulg", "purp", "flav", "lanc", "ov"
)
suffix <- c(
  "ifolia", "iflora", "oides", "ensis", "ianum", "atum", "inum", "osa", "ella", "aris",
  "ica", "ata", "ulum", "ense", "opsis"
)

sp <- character(0)
sp_fam <- character(0)
while (length(sp) < n_sp) {
  f <- sample(fam$family, 1, prob = fam$w)
  g <- sample(genera[[f]], 1)
  nm <- paste(g, paste0(sample(stems, 1), sample(suffix, 1)))
  if (!nm %in% sp) {
    sp <- c(sp, nm)
    sp_fam <- c(sp_fam, f)
  }
}
w <- 1 / seq_len(n_sp)^0.85
idx <- sample(c(seq_len(n_sp), sample(seq_len(n_sp), n - n_sp, replace = TRUE, prob = w)))
species <- sp[idx]
family <- sp_fam[idx]
genus <- sub(" .*", "", species)
order <- fam$order[match(family, fam$family)]

# ---- Geografía sintética: cajas por país ----
ctry <- data.frame(
  code = c("CO", "EC", "PE", "CR", "PA", "MX", "BR", "BO", "VE", "GT"),
  w = c(.22, .14, .14, .10, .07, .10, .10, .05, .05, .03),
  lat1 = c(-3, -3, -13, 8.2, 7.6, 15.5, -24, -19, 5.5, 14.2),
  lat2 = c(11, 0.8, -3.5, 10.9, 9.3, 21.5, -6, -10.5, 9.5, 16.8),
  lon1 = c(-77.5, -79.5, -77.5, -85, -82, -104, -54, -67.5, -70, -91),
  lon2 = c(-67.5, -76.5, -70.5, -83, -78, -92, -41, -61, -62.5, -89),
  e1 = c(100, 100, 100, 50, 50, 100, 20, 200, 100, 100),
  e2 = c(3600, 3800, 3900, 3100, 2900, 3200, 2400, 3400, 2800, 3000),
  stringsAsFactors = FALSE
)
ci <- sample(seq_len(nrow(ctry)), n, replace = TRUE, prob = ctry$w)
u <- function() runif(n)
lat <- ctry$lat1[ci] + u() * (ctry$lat2[ci] - ctry$lat1[ci])
lon <- ctry$lon1[ci] + u() * (ctry$lon2[ci] - ctry$lon1[ci])
elev <- round(ctry$e1[ci] + u() * (ctry$e2[ci] - ctry$e1[ci]), -1)

# Si está el polígono real del Neotrópico, los puntos que caen fuera de él (mar, otras regiones) se
# vuelven a sortear dentro de la caja de su país, con una semilla aparte (no altera el resto).
morrone <- "data/raw/morrone2014/Lowenberg_Neto_2014.shp"
if (file.exists(morrone)) {
  suppressMessages(suppressWarnings({
    sf::sf_use_s2(FALSE)
    region <- sf::st_union(sf::st_make_valid(sf::st_read(morrone, quiet = TRUE)))
    inside_region <- function(x, y) {
      pts <- sf::st_as_sf(data.frame(x = x, y = y), coords = c("x", "y"), crs = sf::st_crs(region))
      lengths(sf::st_intersects(pts, region)) > 0
    }
    withr::with_seed(2026, {
      for (try in 1:50) {
        out <- which(!inside_region(lon, lat))
        if (!length(out)) break
        k <- ci[out]
        lat[out] <- ctry$lat1[k] + runif(length(out)) * (ctry$lat2[k] - ctry$lat1[k])
        lon[out] <- ctry$lon1[k] + runif(length(out)) * (ctry$lon2[k] - ctry$lon1[k])
      }
    })
  }))
}

has <- runif(n) < 0.109
lat[!has] <- NA
lon[!has] <- NA
# errores de captura intencionales para probar las alertas
bad <- sample(which(has), 6)
lat[bad[1:3]] <- 0
lon[bad[1:3]] <- 0
lon[bad[4:6]] <- abs(lon[bad[4:6]])
lat <- round(lat, 5)
lon <- round(lon, 5)

# ---- Elevación y fecha en formatos heterogéneos ----
fmt_e <- sample(1:4, n, replace = TRUE)
verb <- ifelse(fmt_e == 1, as.character(elev),
  ifelse(fmt_e == 2, paste(elev, "m"),
    ifelse(fmt_e == 3, paste0(elev - 100, "-", elev + 100, " m"), paste("c.", elev, "m s.n.m."))
  )
)
verb[runif(n) < 0.08] <- NA

yr <- pmin(2024L, pmax(1890L, round(2024 - rexp(n, 1 / 25))))
month <- sample(1:12, n, TRUE)
day <- sample(1:28, n, TRUE)
date <- ifelse(runif(n) < .6, sprintf("%d-%02d-%02d", yr, month, day),
  ifelse(runif(n) < .5, sprintf("%d-%02d", yr, month), as.character(yr))
)
date[runif(n) < 0.03] <- NA
date[sample(n, 8)] <- "sin fecha"

# ---- Localidades y colectores ----
place_kind <- c("Reserva", "Parque Nacional", "Finca", "Bosque", "Cerro", "Quebrada", "Estación")
place_name <- c(
  "Alto", "Verde", "Nublado", "Los Cedros", "Río Claro", "San Antonio", "La Selva",
  "El Encanto", "Monteverde", "Piedras Blancas", "Cusuco", "Tinalandia"
)
places <- paste(sample(place_kind, n, TRUE), sample(place_name, n, TRUE))
surnames <- c(
  "Ramírez", "Gómez", "Pérez", "Salazar", "Torres", "Mendoza", "Ortiz", "Vargas", "Castillo",
  "Herrera", "Soto", "Rojas"
)
collectors <- paste0(sample(LETTERS, 60, TRUE), ". ", sample(surnames, 60, TRUE))

raw <- data.frame(
  datasetID = "DEMO_EPIG", recordID = sprintf("DEMO-%05d", seq_len(n)),
  order = order, family = family, genus = genus, speciesName = species,
  scientificName = paste(species, "(DEMO)"),
  countryCode = ctry$code[ci], locality = places, decimalLatitude = lat, decimalLongitude = lon,
  verbatimElevation = verb, eventDate = date,
  basisOfRecord = sample(c("PreservedSpecimen", "HumanObservation"), n, TRUE, prob = c(.9, .1)),
  institutionCode = sample(c("DEMO-HB", "DEMO-UN", "DEMO-JB"), n, TRUE), collectionCode = "EPI",
  catalogNumber = sprintf("C%06d", sample(n)), recordedBy = sample(collectors, n, TRUE),
  recordNumber = sample(1:9999, n, TRUE), identifiedBy = sample(collectors, n, TRUE),
  sensitivityLevel = ifelse(runif(n) < 0.03, 1L, 0L), stringsAsFactors = FALSE
)
# defectos intencionales: género inconsistente, recordID duplicado, taxonomía incompleta
raw$genus[sample(n, 12)] <- sample(unlist(genera), 12)
raw$recordID[c(10, 11)] <- raw$recordID[9]
raw$family[is.na(raw$family) | runif(n) < 0.004] <- NA

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
out <- "data/raw/DEMO_Occurrences.csv"
utils::write.csv(raw, out, row.names = FALSE, na = "", fileEncoding = "UTF-8")
n_geo <- sum(!is.na(raw$decimalLatitude))
cat("OK:", out, "con", nrow(raw), "registros y", n_geo, "con coordenadas.\n")
