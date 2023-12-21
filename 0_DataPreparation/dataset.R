###################################################
### Preparation of the Environment ####

# Clear environment
remove(list = ls())

# Create list with needed libraries
pkgs <- c("readr", "dplyr", "ggplot2", "styler", "skimr", "DataExplorer", "lubridate", "tidyverse", "zoo", "imputeTS")

# Load each listed library and check if it is installed and install if necessary
for (pkg in pkgs) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}


###################################################
### Functions ####
set_na_to_zero <- function(data, columns) {
  for (col in columns) {
    data[[col]][is.na(data[[col]])] <- 0
  }
  return(data)
}

###################################################
### Data Import ####

# # Directory containing CSV files
# directory_path <- "Data/"
#
# # List all CSV files in the directory
# csv_files <- list.files(directory_path, pattern = "\\.csv$", full.names = TRUE)
#
# # Use a for loop to load each file and create variables
# for (file in csv_files) {
#   # Extract the file name without the path and extension
#   file_name <- tools::file_path_sans_ext(basename(file))
#
#   # Load the data into a data frame
#   data <- read_csv(file)
#
#   # Assign the data frame to a variable with the same name as the file
#   assign(file_name, data)
# }

# Reading the data files
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
income <- read_csv("Data/income.csv")
ausgaben <- read_csv("Data/ausgaben.csv")
kreuzfahrtschiffe <- read_csv("Data/kreuzfahrtschiffe.csv")

###################################################
# Hinzufügen von Werten für die Jahreszeit abhängig von dem Datum
wetter_daten <- wetter_daten %>%
  mutate(jahreszeit = case_when(
  between(month(Datum), 3, 4) ~ "Frühling",
  between(month(Datum), 5, 6) ~ "Sommer",
  between(month(Datum), 7, 8) ~ "Hochsommer",
  between(month(Datum), 9, 11) ~ "Herbst",
  between(month(Datum), 12, 2) ~ "Winter"
  ))

# Definition von Temperaturgrenzen, um die Temperatur in Kategorien einzuteilen
temperatur_grenzen <- c(-Inf, -8, 4, 15, 21, 28, Inf)
temperatur_bezeichnung <- c("sehr_niedrig", "niedrig", "gemäßigt", "hoch", "sehr_hoch", "heiß")

# Definition von Windgeschwindigkeitsgrenzen, um die Windgeschwindigkeit in Kategorien einzuteilen
wind_grenzen <- c(-Inf, 20, 28, Inf)
wind_bezeichnung <- c("kein_wind", "normal", "windig")

# Setzen aller NA Werte für die Spalte 'Wettercodes' auf 0
wetter_daten$Wettercode[is.na(wetter_daten$Wettercode)] <- 0

# Definition von Wettercodesgrenzen, um die Wettercodes in Kategorien einzuteilen
wetter_grenzen <- c(-Inf,0, 99)
wetter_bezeichnung <- c("Gutes Wetter", "Schlechtes Wetter")

# merge the test ids with the combined data
verkaufs_daten <- full_join(verkaufs_daten, test_ids, by = c("Datum", "Warengruppe"))

# Ändern des Spaltennamens Feiertag zu feiertag
colnames(feiertage)[colnames(feiertage) == "Feiertag"] <- "feiertag"

# Ändern des Wertes in der Spalte 'feiertag' für alle Feiertage auf 1
feiertage <- feiertage %>%
  mutate(feiertag = 1)

schulferien <- distinct(schulferien)

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

# Einlesen der Datumsspalte des Datensatzes kreuzfahrtschiffe als Datum
kreuzfahrtschiffe$Date <- dmy(kreuzfahrtschiffe$Date)

# Ändern des Spaltennamens von Date zu Datum
colnames(kreuzfahrtschiffe)[colnames(kreuzfahrtschiffe) == "Date"] <- "Datum"

# Ändern des Formats der Datumsspalte im Datensatz kreuzfahrtschiffe zu YYYY-MM-DD, wie in den anderen Datensätzen
kreuzfahrtschiffe$Datum <- as.Date(kreuzfahrtschiffe$Datum, format = "%Y-%m-%d")

# Ändern des Spaltennamens von Flohmarkt/Verkaufsoffener Sonntag zu flohmarkt
colnames(verkaufsoffener_sonntag)[colnames(verkaufsoffener_sonntag) == "Flohmarkt/Verkaufsoffener Sonntag"] <- "flohmarkt"

# Filtern nach allen verkaufsoffenen Sonntagen und Zuweisung des Wertes 1 in der Spalte 'flohmarkt'
verkaufsoffener_sonntag <- verkaufsoffener_sonntag %>%
  filter(flohmarkt == "offen") %>%
  mutate(flohmarkt = 1)


# Dataframe mit jährlicher Arbeitslosenquote von Kiel
arbeitslosenquote <- data.frame(
  jahr = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019),
  arbeitslosenquote = c(10.1, 10.2, 10.1, 9.9, 9.7, 9.1, 8.2, 7.6)
)

# Definition von Arbeitslosenquoten, um die Arbeitslosenquote in Kategorien einzuteilen
alzahl_grenzen <- c(-Inf, 9.1, 10.2)
alzahl_bezeichnung <- c("Niedrig", "Hoch")


# Dataframe mit jährlicher Einwohnerzahl von Kiel
einwohnerzahl <- data.frame(
  jahr = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019),
  einwohnerzahl = c(239.866, 241.533, 243.488, 246.306, 247.441, 247.943, 247.548, 246.947)
)

# Dataframe mit allen Tagen von WM Spielen
wm_spiele <- data.frame(
  Datum = c("2014-06-16", "2014-06-21", "2014-06-26", "2014-06-30", "2014-07-04","2014-07-08", "2014-07-13", "2016-06-12", "2016-06-16", "2016-06-21", "2016-07-02", "2016-07-07", "2018-06-17", "2018-06-23", "2018-06-27"),
  wm_spiele = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
)

wm_spiele$Datum <- as.Date(wm_spiele$Datum, format = "%Y-%m-%d")

# Dataframe mit allen Tagen der Windjammerparade
windjammerparade <- data.frame(
  Datum = c("2012-06-23", "2013-06-29", "2014-06-28", "2015-06-27", "2016-06-25", "2017-06-24", "2018-06-23", "2019-06-29"),
  windjammerparade = c(1, 1, 1, 1, 1, 1, 1, 1)
)

windjammerparade$Datum <- as.Date(windjammerparade$Datum, format = "%Y-%m-%d")

# Dataframe mit allen Tagen von Sylvester
sylvester <- data.frame(
  Datum = c("2012-12-31", "2013-12-31", "2014-12-31", "2015-12-31", "2016-12-31", "2017-12-31", "2018-12-31", "2019-12-31"),
  sylvester = c(1,1,1,1,1,1,1,1)
)

sylvester$Datum <- as.Date(sylvester$Datum, format = "%Y-%m-%d")

# Dataframe mit allen Tagen vor Sylvester
vor_sylvester <- data.frame(
  Datum = c("2012-12-30", "2013-12-30", "2014-12-30", "2015-12-30", "2016-12-30", "2017-12-30", "2018-12-30", "2019-12-30"),
  vor_sylvester = c(1,1,1,1,1,1,1,1)
)

vor_sylvester$Datum <- as.Date(vor_sylvester$Datum, format = "%Y-%m-%d")

# Dataframe mit allen Tagen vor Karfreitag
tage_vor_ostern <- data.frame(
  Datum = c("2012-04-05", "2013-03-28", "2014-04-15", "2014-04-16", "2014-04-17", "2014-04-19", "2015-03-30", "2015-04-01", "2015-04-02", "2015-04-04", "2016-03-22", "2016-03-23", "2016-03-24", "2016-03-26", "2017-04-11", "2017-04-12", "2017-04-13", "2017-04-15", "2018-03-27", "2018-03-28", "2018-03-29", "2018-03-31", "2019-04-16", "2019-04-17", "2019-04-18", "2019-04-20"),
  tage_vor_ostern = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
)

tage_vor_ostern$Datum <- as.Date(tage_vor_ostern$Datum, format = "%Y-%m-%d")

# Dataframe mit allen Ostersamstagen
ostersamstag <- data.frame(
  Datum = c("2014-04-19", "2015-04-04", "2016-03-26", "2017-04-15", "2018-03-31", "2019-04-20"),
  ostersamstag = c(1,1,1,1,1,1)
)

ostersamstag$Datum <- as.Date(ostersamstag$Datum, format = "%Y-%m-%d")

# Dataframe mit Heiligabend
heiligabend <- data.frame(
  Datum = c("2013-12-24", "2014-12-24", "2015-12-24", "2016-12-24", "2017-12-24", "2018-12-24", "2019-12-24"),
  heiligabend = c(1,1,1,1,1,1,1)
)

heiligabend$Datum <- as.Date(heiligabend$Datum, format = "%Y-%m-%d")

# Erstellung eines tibbles mit allen Datensätzen aufgrundlage der gemeinsamen Spalte 'Datum'
gesamte_daten <- full_join(wetter_daten, verkaufs_daten, by = c("Datum"))

# Zusammenfügen aller Datensätze und Erstellung der Spalten 'jahr', 'quartal', 'monat'
gesamte_daten <- gesamte_daten %>%
  full_join(kreuzfahrtschiffe, join_by(Datum)) %>%
  full_join(feiertage, join_by(Datum)) %>%
  full_join(holstein_kiel, join_by(Datum)) %>%
  full_join(thw_kiel, join_by(Datum)) %>%
  full_join(verkaufsoffener_sonntag, join_by(Datum)) %>%
  full_join(kiwo_tage, join_by(Datum)) %>%
  full_join(windjammerparade, join_by(Datum)) %>%
  full_join(sylvester, join_by(Datum)) %>%
  full_join(vor_sylvester, join_by(Datum)) %>%
  full_join(tage_vor_ostern, join_by(Datum)) %>%
  full_join(ostersamstag, join_by(Datum)) %>%
  full_join(heiligabend, join_by(Datum)) %>%
  full_join(wm_spiele, join_by(Datum)) %>%
  mutate(jahr = year(Datum)) %>%
  mutate(quartal = quarter(Datum)) %>%
  mutate(monat = month.name[month(Datum)]) %>%
  full_join(verkaufs_preise, join_by(jahr, monat)) %>%
  full_join(arbeitslosenquote, join_by(jahr)) %>%
  full_join(inflation, join_by(jahr, monat)) %>%
  full_join(income, join_by(jahr, quartal)) %>%
  full_join(einwohnerzahl, join_by(jahr)) %>%
  full_join(ausgaben, join_by(jahr)) %>%
  full_join(schulferien, join_by(Datum))


# Column names to set 0 if is NA
columns_to_set_zero <- c(
  "feiertag", "KielerWoche", "holstein_spiel", "thw_spiel",
  "ferien", "flohmarkt", "Umsatz", "jahreszeit",
  "Bewoelkung", "sylvester", "vor_sylvester",
  "tage_vor_ostern", "ostersamstag", "wm_spiele",
  "Count", "windjammerparade", "heiligabend"
)

gesamte_daten <- set_na_to_zero(gesamte_daten, columns_to_set_zero)

# # Setzen aller NA Werte auf 0, bis auf Wetterangaben
# gesamte_daten$feiertag[is.na(gesamte_daten$feiertag)] <- 0
# gesamte_daten$KielerWoche[is.na(gesamte_daten$KielerWoche)] <- 0
# gesamte_daten$holstein_spiel[is.na(gesamte_daten$holstein_spiel)] <- 0
# gesamte_daten$thw_spiel[is.na(gesamte_daten$thw_spiel)] <- 0
# gesamte_daten$ferien[is.na(gesamte_daten$ferien)] <- 0
# gesamte_daten$flohmarkt[is.na(gesamte_daten$flohmarkt)] <- 0
# gesamte_daten$Umsatz[is.na(gesamte_daten$Umsatz)] <- 0
# gesamte_daten$jahreszeit[is.na(gesamte_daten$jahreszeit)] <- 0
# gesamte_daten$Bewoelkung[is.na(gesamte_daten$Bewoelkung)] <- 0
# gesamte_daten$sylvester[is.na(gesamte_daten$sylvester)] <- 0
# gesamte_daten$vor_sylvester[is.na(gesamte_daten$vor_sylvester)] <- 0
# gesamte_daten$tage_vor_ostern[is.na(gesamte_daten$tage_vor_ostern)] <- 0
# gesamte_daten$ostersamstag[is.na(gesamte_daten$ostersamstag)] <- 0
# gesamte_daten$wm_spiele[is.na(gesamte_daten$wm_spiele)] <- 0
# gesamte_daten$Count[is.na(gesamte_daten$Count)] <- 0
# gesamte_daten$windjammerparade[is.na(gesamte_daten$windjammerparade)] <- 0
# gesamte_daten$heiligabend[is.na(gesamte_daten$heiligabend)] <- 0

# Herausfiltern aller Werte ohne Umsatzdaten, bspw. wenn geschlossen wegen Feiertagen oder wegen fehlenden Daten im Jahr 2012 und 2019
# gefilterte_daten <- gesamte_daten %>%
#   filter(Umsatz == 0 & feiertag == 1 | Umsatz > 0 & feiertag == 0 | Umsatz > 0 & feiertag == 1)

# Filtern nach einzigartigen Werten

# Hinzufügen von Wochentagen abhängig von dem Datum im Datensatz wetter_daten
gesamte_daten$wochentag <- weekdays(gesamte_daten$Datum)

gesamte_daten <- gesamte_daten %>%
  arrange(Datum) %>%
  filter(!is.na(Datum)) %>%
  filter(Datum < as.Date("2019-08-02")) %>%
  mutate(across(c("Bewoelkung", "Temperatur", "Windgeschwindigkeit", "Wettercode"), zoo::na.approx, na.rm = FALSE))

# gesamte_daten <- gesamte_daten %>%
#   arrange(Datum) %>%
#   filter(!is.na(Datum)) %>%
#   filter(Datum < as.Date("2019-08-02")) %>%
#   mutate(across(c("Bewoelkung", "Temperatur", "Windgeschwindigkeit", "Wettercode"), na_interpolation, option = "linear"))

gesamte_daten <- gesamte_daten %>%
  mutate(monatsabschnitt = case_when(
    day(Datum) %in% 1:7 ~ "End",
    day(Datum) %in% 8:22 ~ "Mid",
    day(Datum) %in% 23:31 ~ "End"
  ))

# Hinzufügen einer neuen Spalte 'temperatur_kategorie' anhand der definierten Grenzen und Bezeichnungen
gesamte_daten$temperatur_kategorie <- cut(gesamte_daten$Temperatur,
                                         breaks = temperatur_grenzen,
                                         labels = temperatur_bezeichnung,
                                         include.lowest = TRUE)

# Hinzufügen einer neuen Spalte 'wind_kategorie' anhand der definierten Grenzen und Bezeichnungen
gesamte_daten$wind_kategorie <- cut(gesamte_daten$Windgeschwindigkeit,
                                   breaks = wind_grenzen,
                                   labels = wind_bezeichnung,
                                   include.lowest = TRUE)

# Hinzufügen einer neuen Spalte 'wetter_kategorie' anhand der definierten Grenzen und Bezeichnungen
gesamte_daten$wetter_kategorie <- cut(gesamte_daten$Wettercode,
                                     breaks = wetter_grenzen,
                                     labels = wetter_bezeichnung,
                                     include.lowest = TRUE)

# Hinzufügen einer neuen Spalte 'wetter_kategorie' anhand der definierten Grenzen und Bezeichnungen
gesamte_daten$arbeitslosenquote_kategorie <- cut(gesamte_daten$arbeitslosenquote,
                                                     breaks = alzahl_grenzen,
                                                     labels = alzahl_bezeichnung,
                                                     include.lowest = TRUE)


gefilterte_daten <- distinct(gesamte_daten)



###################################################
### Data Preparation ####

# # Preparation of independent variables ('features') by dummy coding the categorical variables
# features <- as_tibble(model.matrix(Umsatz ~ as.factor(wochentag) + as.factor(Warengruppe) + as.factor(monat) + as.factor(wetter_kategorie) + as.factor(arbeitslosenquote_kategorie) + as.factor(monatsabschnitt) + as.factor(Count), data = gefilterte_daten))
# names(features)
#
# # Construction of prepared data set
# prepared_data <- tibble(label=gefilterte_daten$Umsatz, features) %>%  # inclusion of the dependent variable ('label')
#   filter(complete.cases(.)) # Handling of missing values (here: only keeping rows without missing values)




