# Load libraries
library(dplyr)
library(readr)
library(ggplot2)
library(styler)
library(skimr)
library(DataExplorer)
library(lubridate)

# import csv files
sales_data <- read_csv("Data/umsatzdaten_gekuerzt.csv")
weather_data <- read_csv("Data/wetter.csv")
kiwo_days <- read_csv("Data/kiwo.csv")
holidays <- read_csv("Data/Feiertage.csv")
schulferien <- read_csv("Data/Schulferien_2.csv")
holstein_kiel <- read_csv("Data/HolsteinKiel.csv")
verkaufsoffener_sonntag <- read_csv("Data/VerkaufsoffenerSonntag.csv")
thw_kiel <- read_csv("Data/THWKiel.csv")
test_ids <- read_csv("Data/test_ids.csv")

# turn dates into weekdays
weather_data$Weekday <- weekdays(weather_data$Datum)

# Define the boundaries for the temperature categories
temperature_boundaries <- c(-Inf, -8, 4, 15, 21, 28, Inf)

# Define the labels for the categories
temperature_labels <- c("Very Low", "Low", "Medium", "High", "Very High", "Hot")

# Create a new column 'temperature_category' with the assigned categories
weather_data$temperature_category <- cut(weather_data$Temperatur,
                               breaks = temperature_boundaries,
                               labels = temperature_labels,
                               include.lowest = TRUE)

# Define the boundaries for the temperature categories
wind_boundaries <- c(-Inf, 28, Inf)

# Define the labels for the categories
wind_labels <- c("Normal", "Windy")

# Create a new column 'temperature_category' with the assigned categories
weather_data$wind_category <- cut(weather_data$Windgeschwindigkeit,
                                         breaks = wind_boundaries,
                                         labels = wind_labels,
                                         include.lowest = TRUE)

weather_data$Wettercode[is.na(weather_data$Wettercode)] <- 0

# Define the boundaries for the temperature categories
weather_boundaries <- c(-Inf,0, 99)

# Define the labels for the categories
weather_labels <- c("Gutes Wetter", "Schlechtes Wetter")

# Create a new column 'temperature_category' with the assigned categories
weather_data$weather_category <- cut(weather_data$Wettercode,
                                  breaks = weather_boundaries,
                                  labels = weather_labels,
                                  include.lowest = TRUE)

# filter for Ferien only in schulferien and mutate them to digit 1
holidays <- holidays %>%
  mutate(Feiertag = ifelse(Feiertag == "0", 0, 1)) %>%
  filter(Feiertag == 1)

# filter for Ferien only in schulferien and mutate them to digit 1
schulferien <- schulferien %>%
  mutate(Ferien = ifelse(Ferien == "Keine Ferien", 0, 1)) %>%
  filter(Ferien == 1)

# change colname to Heimspiel
colnames(holstein_kiel)[colnames(holstein_kiel) == "Spiel"] <- "holstein_spiel"

# filter for heimspiele only in holstein_kiel and mutate them to digit 1
holstein_kiel <- holstein_kiel %>%
  filter(holstein_spiel == "Heimspiel") %>%
  mutate(holstein_spiel = ifelse(holstein_spiel == "Heimspiel", 1, holstein_spiel))

# turn Datum into dates to change the format
thw_kiel$Datum <- dmy(thw_kiel$Datum)

# Reformat the date column to "yyyy-mm-dd" to match other datasets
thw_kiel$Datum <- format(thw_kiel$Datum, "%Y-%m-%d")
thw_kiel$Datum <- as.Date(thw_kiel$Datum, format = "%Y-%m-%d")

# change colname to Heimspiel
colnames(thw_kiel)[colnames(thw_kiel) == "Handballspiel"] <- "thw_spiel"

# filter for heimspiele only in thw_kiel and mutate them to digit 1
thw_kiel <- thw_kiel %>%
  filter(thw_spiel == "Heimspiel") %>%
  mutate(thw_spiel = ifelse(thw_spiel == "Heimspiel", 1, thw_spiel))

# change colname to Heimspiel
colnames(verkaufsoffener_sonntag)[colnames(verkaufsoffener_sonntag) == "Flohmarkt/Verkaufsoffener Sonntag"] <- "flohmarkt"

# filter for verkaufsoffene sonntage only in verkaufsoffener_sonntag and mutate them to digit 1
verkaufsoffener_sonntag <- verkaufsoffener_sonntag %>%
  filter(flohmarkt == "offen")

# make all entries in verkaufsoffener_sonntag in the column flohmarkt to 1
verkaufsoffener_sonntag$flohmarkt <- 1

# create a tibble with the sales data, weather data and kiwo days
combined_data <- full_join(weather_data, sales_data, join_by(Datum)) %>%
  full_join(holidays, join_by(Datum)) %>%
  full_join(kiwo_days, join_by(Datum)) %>%
  full_join(holstein_kiel, join_by(Datum)) %>%
  full_join(thw_kiel, join_by(Datum)) %>%
  full_join(schulferien, join_by(Datum)) %>%
  full_join(verkaufsoffener_sonntag, join_by(Datum))

# merge the test ids with the combined data
# combined_data <- merge(combined_data, test_ids, by = c("Datum", "Warengruppe"), all = TRUE)



# make all entries na equal to 0
combined_data$Feiertag[is.na(combined_data$Feiertag)] <- 0
combined_data$KielerWoche[is.na(combined_data$KielerWoche)] <- 0
combined_data$holstein_spiel[is.na(combined_data$holstein_spiel)] <- 0
combined_data$thw_spiel[is.na(combined_data$thw_spiel)] <- 0
combined_data$Ferien[is.na(combined_data$Ferien)] <- 0
combined_data$flohmarkt[is.na(combined_data$flohmarkt)] <- 0

# filter out data without sales data and no feiertag
filtered_data <- combined_data %>%
  filter(!is.na(Umsatz) & Feiertag == 0 | !is.na(Umsatz) & Feiertag == 1 | is.na(Umsatz) & Feiertag == 0)

# show only unique entries
filtered_data <- distinct(filtered_data)
