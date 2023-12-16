# Laden aller verwendeten libraries
library(dplyr)
library(readr)
library(ggplot2)
library(styler)
library(skimr)
library(DataExplorer)
library(lubridate)
library(tidyverse)
library(zoo)

# Importieren aller .csv Dateien im Ordner Data
verkaufs_daten <- read_csv("Data/umsatzdaten_gekuerzt.csv")
wetter_daten <- read_csv("Data/wetter.csv")
kiwo_tage <- read_csv("Data/kiwo.csv")
feiertage <- read_csv("Data/feiertage.csv")
schulferien <- read_csv("Data/schulferien.csv")
holstein_kiel <- read_csv("Data/holstein_kiel.csv")
verkaufsoffener_sonntag <- read_csv("Data/verkaufsoffener_sonntag.csv")
thw_kiel <- read_csv("Data/thw_kiel.csv")
test_ids <- read_csv("Data/test_ids.csv")
inflation <- read_csv("Data/inflation.csv")
verkaufs_preise <- read_csv("Data/retail_prices.csv")

# Hinzufügen von Wochentagen abhängig von dem Datum im Datensatz wetter_daten
wetter_daten$wochentag <- weekdays(wetter_daten$Datum)

# variable_to_impute <- "Temperatur"
#
# impute_missing_linear <- function(x) {
#   zoo::na.approx(x)
# }
#
# gesamte_daten <- gesamte_daten %>%
#   mutate(across(all_of(column_to_impute), impute_missing_linear))

# Hinzufügen von Werten für die Jahreszeit abhängig von dem Datum
wetter_daten <- wetter_daten %>%
  mutate(jahreszeit = case_when(
  between(month(Datum), 3, 4) ~ "Frühling",
  between(month(Datum), 5, 6) ~ "Sommer",
  between(month(Datum), 7, 8) ~ "Hochsommer",
  between(month(Datum), 9, 11) ~ "Herbst"
  ))

wetter_daten <- wetter_daten %>%
  mutate(monat = month.name[month(Datum)])

# Definition von Temperaturgrenzen, um die Temperatur in Kategorien einzuteilen
temperatur_grenzen <- c(-Inf, -8, 4, 15, 21, 28, Inf)

# Definition von Bezeichnungen für die Temperaturkategorien
temperatur_bezeichnung <- c("sehr_niedrig", "niedrig", "gemäßigt", "hoch", "sehr_hoch","heiß")

# Hinzufügen einer neuen Spalte 'temperatur_kategorie' anhand der definierten Grenzen und Bezeichnungen
wetter_daten$temperatur_kategorie <- cut(wetter_daten$Temperatur,
                               breaks = temperatur_grenzen,
                               labels = temperatur_bezeichnung,
                               include.lowest = TRUE)

# Definition von Windgeschwindigkeitsgrenzen, um die Windgeschwindigkeit in Kategorien einzuteilen
wind_grenzen <- c(-Inf, 20, 28, Inf)

# Define the labels for the categories
wind_bezeichnung <- c("kein_wind","normal", "windig")

# Hinzufügen einer neuen Spalte 'wind_kategorie' anhand der definierten Grenzen und Bezeichnungen
wetter_daten$wind_kategorie <- cut(wetter_daten$Windgeschwindigkeit,
                                         breaks = wind_grenzen,
                                         labels = wind_bezeichnung,
                                         include.lowest = TRUE)

# Setzen aller NA Werte für die Spalte 'Wettercodes' auf 0
wetter_daten$Wettercode[is.na(wetter_daten$Wettercode)] <- 0

# Definition von Wettercodesgrenzen, um die Wettercodes in Kategorien einzuteilen
wetter_grenzen <- c(-Inf,0, 99)

# Definition von Bezeichnungen für die Wettercodeskategorien
wetter_bezeichnung <- c("Gutes Wetter", "Schlechtes Wetter")

# Hinzufügen einer neuen Spalte 'wetter_kategorie' anhand der definierten Grenzen und Bezeichnungen
wetter_daten$wetter_kategorie <- cut(wetter_daten$Wettercode,
                                  breaks = wetter_grenzen,
                                  labels = wetter_bezeichnung,
                                  include.lowest = TRUE)


# merge the test ids with the combined data
verkaufs_daten <- full_join(verkaufs_daten, test_ids, by = c("Datum", "Warengruppe"))

# Ändern des Spaltennamens Feiertag zu feiertag
colnames(feiertage)[colnames(feiertage) == "Feiertag"] <- "feiertag"

# Ändern des Wertes in der Spalte 'feiertag' für alle Feiertage auf 1
feiertage <- feiertage %>%
  mutate(feiertag = 1)

# Ändern des Spaltennamens Ferien zu ferien
colnames(schulferien)[colnames(schulferien) == "Ferien"] <- "ferien"

# Ändern des Wertes für keine Ferien auf 0
schulferien <- schulferien %>%
  mutate(ferien = ifelse(ferien == "Keine Ferien", 0, 1)) %>%
  filter(ferien != 0)

# Ändern des Spaltennamens von Spiel zu holstein_spiel
colnames(holstein_kiel)[colnames(holstein_kiel) == "Spiel"] <- "holstein_spiel"

# Setzen aller Heimspiele in der Spalte 'holstein_spiel' auf 1
holstein_kiel <- holstein_kiel %>%
  mutate(holstein_spiel = ifelse(holstein_spiel == "Heimspiel", 1, 0)) %>%
  filter(holstein_spiel == 1)

# Ändern des Datentyps im Datensatz thw_kiel für die Spalte 'Datum' als Datum
thw_kiel$Datum <- dmy(thw_kiel$Datum)

# Reformatieren der Datumsspalte im Datensatz thw_kiel zu YYYY-MM-DD, wie in den anderen Datensätzen
thw_kiel$Datum <- format(thw_kiel$Datum, "%Y-%m-%d")
thw_kiel$Datum <- as.Date(thw_kiel$Datum, format = "%Y-%m-%d")

# Ändern des Spaltennamens von Handballspiel zu thw_spiel
colnames(thw_kiel)[colnames(thw_kiel) == "Handballspiel"] <- "thw_spiel"

# Setzen aller Heimspiele in der Spalte thw_spiel auf 1
thw_kiel <- thw_kiel %>%
  mutate(thw_spiel = ifelse(thw_spiel == "Heimspiel", 1, 0)) %>%
  filter(thw_spiel == 1)

# Ändern des Spaltennamens von Flohmarkt/Verkaufsoffener Sonntag zu flohmarkt
colnames(verkaufsoffener_sonntag)[colnames(verkaufsoffener_sonntag) == "Flohmarkt/Verkaufsoffener Sonntag"] <- "flohmarkt"

# Filtern nach allen verkaufsoffenen Sonntagen und Zuweisung des Wertes 1 in der Spalte 'flohmarkt'
verkaufsoffener_sonntag <- verkaufsoffener_sonntag %>%
  filter(flohmarkt == "offen") %>%
  mutate(flohmarkt = 1)

arbeitslosenquote <- data.frame(
  jahr = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019),
  arbeitslosenquote = c(10.1, 10.2, 10.1, 9.9, 9.7, 9.1, 8.2, 7.6)
)

einwohnerzahl <- data.frame(
  jahr = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019),
  einwohnerzahl = c(239.866, 241.533, 243.488, 246.306, 247.441, 247.943, 247.548, 246.947)
)

# Definition von Wettercodesgrenzen, um die Wettercodes in Kategorien einzuteilen
alzahl_grenzen <- c(-Inf, 9.1, 10.2)

# Definition von Bezeichnungen für die Wettercodeskategorien
alzahl_bezeichnung <- c("Niedrig", "Hoch")

# Hinzufügen einer neuen Spalte 'wetter_kategorie' anhand der definierten Grenzen und Bezeichnungen
arbeitslosenquote$arbeitslosenquote_kategorie <- cut(arbeitslosenquote$arbeitslosenquote,
                                     breaks = alzahl_grenzen,
                                     labels = alzahl_bezeichnung,
                                     include.lowest = TRUE)

sylvester <- data.frame(
  Datum = c("2012-12-31", "2013-12-31", "2014-12-31", "2015-12-31", "2016-12-31", "2017-12-31", "2018-12-31", "2019-12-31"),
  sylvester = c(1,1,1,1,1,1,1,1)
)

tage_vor_ostern <- data.frame(
  Datum = c("2012-04-05", "2013-03-28", "2014-04-15", "2014-04-16","2014-04-17", "2015-03-30", "2015-04-01", "2015-04-02", "2016-03-22", "2016-03-23", "2016-03-24", "2017-04-11", "2017-04-12", "2017-04-13", "2018-03-27", "2018-03-28", "2018-03-29", "2019-04-16", "2019-04-17", "2019-04-18"),
  tage_vor_ostern = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
)

sylvester$Datum <- as.Date(sylvester$Datum, format = "%Y-%m-%d")

tage_vor_ostern$Datum <- as.Date(tage_vor_ostern$Datum, format = "%Y-%m-%d")

# Erstellung eines tibbles mit allen Datensätzen aufgrundlage der gemeinsamen Spalte 'Datum'
gesamte_daten <- full_join(wetter_daten, verkaufs_daten, by = c("Datum"))

gesamte_daten <- gesamte_daten %>%
  full_join(feiertage, join_by(Datum)) %>%
  full_join(kiwo_tage, join_by(Datum)) %>%
  full_join(holstein_kiel, join_by(Datum)) %>%
  full_join(thw_kiel, join_by(Datum)) %>%
  full_join(schulferien, join_by(Datum), relationship = "many-to-many") %>%
  full_join(verkaufsoffener_sonntag, join_by(Datum)) %>%
  full_join(sylvester, join_by(Datum)) %>%
  full_join(tage_vor_ostern, join_by(Datum))

gesamte_daten <- gesamte_daten %>%
  mutate(jahr = year(Datum)) %>%
  full_join(arbeitslosenquote, join_by(jahr)) %>%
  full_join(einwohnerzahl, join_by(jahr)) %>%
  full_join(verkaufs_preise, join_by(jahr, monat)) %>%
  full_join(inflation, join_by(jahr, monat))

# Setzen aller NA Werte auf 0
gesamte_daten$feiertag[is.na(gesamte_daten$feiertag)] <- 0
gesamte_daten$KielerWoche[is.na(gesamte_daten$KielerWoche)] <- 0
gesamte_daten$holstein_spiel[is.na(gesamte_daten$holstein_spiel)] <- 0
gesamte_daten$thw_spiel[is.na(gesamte_daten$thw_spiel)] <- 0
gesamte_daten$ferien[is.na(gesamte_daten$ferien)] <- 0
gesamte_daten$flohmarkt[is.na(gesamte_daten$flohmarkt)] <- 0
gesamte_daten$Umsatz[is.na(gesamte_daten$Umsatz)] <- 0
gesamte_daten$jahreszeit[is.na(gesamte_daten$jahreszeit)] <- 0
gesamte_daten$Bewoelkung[is.na(gesamte_daten$Bewoelkung)] <- 0
gesamte_daten$sylvester[is.na(gesamte_daten$sylvester)] <- 0
gesamte_daten$tage_vor_ostern[is.na(gesamte_daten$tage_vor_ostern)] <- 0

# column_to_clean <- "Temperatur"

# Remove rows with missing values in the specified column
# gesamte_daten <- gesamte_daten[complete.cases(gesamte_daten[, column_to_clean]), ]

# Herausfiltern aller Werte ohne Umsatzdaten, bspw. wenn geschlossen wegen Feiertagen oder wegen fehlenden Daten im Jahr 2012 und 2019
# gefilterte_daten <- gesamte_daten %>%
#   filter(Umsatz == 0 & feiertag == 1 | Umsatz > 0 & feiertag == 0 | Umsatz > 0 & feiertag == 1)

# Filtern nach einzigartigen Werten
gefilterte_daten <- distinct(gesamte_daten)
