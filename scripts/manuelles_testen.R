# Pakete laden
library("zhGebiete")
#install.packages("dplyr")
library("dplyr")


# Test Datensatz erstellen
test_daten <- data.frame(
  "validieren" = c(
    "Hausen am Albis",
    "Hedingen",
    "Kappel am A",
    "Wädenswil",
    "Elgg",
    "Horgen",
    "Illnau-Effretikon",
    "Bauma",
    "Wiesendangen",
    "Bülack",
    "Oetwil",
    "Langnau a. A.",
    "Will",
    "flurlingen",
    "züri",
    "Büla"
  )
)

# Gemeinde Namen Harmonisieren
test_harmonized <- bind_cols(
  "validieren" = test_daten$validieren,
  get_gemeinden(auswahl = TRUE, gemeinde_name = test_daten$validieren)
) |>
  select(validieren, gemeinde_name, gemeinde_code)

View(test_harmonized)

# Harmonisierte Namen und Anreichern
test_enriched <- left_join(
  test_harmonized,
  get_gemeindezuweisungen(),
  by = "gemeinde_code"
) |>
  select(-gemeinde_name.y) |>
  rename(gemeinde_name = gemeinde_name.x)

View(test_enriched)

get_gebiete() # Alle Gebiete abrufen
get_gebietstypen() # Alle Gebietstypen abrufen
get_gemeindemutationen() # Alle Gemeindemutationen abrufen

get_gemeinden() # Alle Gemeinden abrufen
get_bezirke() # Alle Bezirke abrufen
get_raumplanungsregionen() # Alle Raumplanungsregionen abrufen

get_gemeindezuweisungen() # Alle Gemeindezuweisungen abrufen

get_gemeindenhist() # Alle Jahresstände von Gemeinden abrufen

# Filtern nach einem oder mehreren Codes
get_gemeinden(gemeinde_code = 111)
get_bezirke(bezirk_code = c(101, 104, 112))
get_raumplanungsregionen(raumplanungsregion_code = c(104:110))

# Filtern nach einem oder mehreren Namen mit oder ohne Auswahl
get_gemeinden(auswahl = TRUE, gemeinde_name = "am Albis")
get_bezirke(bezirk_name = c("Horgn", "Züri"))

# Filtern nach einem oder mehreren Gemeindecodes
get_gemeindezuweisungen(gemeinde_code = 111)
get_gemeindezuweisungen(gemeinde_code = c(111, 113))

# Filtern nach einem oder mehreren Jahren
get_gemeindenhist(jahr = 2026)
get_gemeindenhist(jahr = c(2021:2026))

# Filtern nach einem oder mehreren Gemeindecodes
get_gemeindenhist(gemeinde_code = 111)
get_gemeindenhist(gemeinde_code = c(111, 113))

# Filtern nach Jahr und Gemeindecode
get_gemeindenhist(jahr = c(2021:2026), gemeinde_code = c(111, 113))

