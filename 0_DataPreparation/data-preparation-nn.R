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
#### Functions ####

load_csv_files <- function(directory_path) {
  # List all CSV files in the directory
  csv_files <- list.files(directory_path, pattern = "\\.csv$", full.names = TRUE)

  # Use a for loop to load each file and create variables
  for (file in csv_files) {
    # Extract the file name without the path and extension
    file_name <- tools::file_path_sans_ext(basename(file))

    # Load the data into a data frame
    data <- read.csv(file)  # You can use other read functions based on your file format

    # Assign the data frame to a variable with the same name as the file
    assign(file_name, data, envir = .GlobalEnv)
  }
}

set_na_to_zero <- function(data, columns) {
  for (col in columns) {
    data[[col]][is.na(data[[col]])] <- 0
  }
  return(data)
}

# one_hot_encode <- function(data, variable) {
#   encoded_data <- model.matrix(~ . - 1, data = data.frame(data[, variable, drop = FALSE])) %>%
#     as.data.frame()
#
#   return(encoded_data)
# }

one_hot_encode <- function(data, variables) {
  encoded_data <- data

  for (variable in variables) {
    encoded_variable <- model.matrix(~ . + 0, data = data.frame(encoded_data[, variable, drop = FALSE])) %>%
      as.data.frame()

    encoded_data <- cbind(encoded_data, encoded_variable) %>%
      select(-{{ variable }})
  }

  return(encoded_data)
}

###################################################
#### Data Import ####

# Directory containing CSV files
directory_path <- "5_Data/"

load_csv_files(directory_path)

###################################################
#### Data Preparation ####

#### Weather Data ####

# Define a vector with the new column names and use colnames to assign the new names to the data frame
weather_column_names <- c("DATE", "CLOUDS", "TEMPERATURE", "WIND_SPEED", "WEATHER_CODE")
colnames(weather_data) <- weather_column_names

weather_data$DATE <- as.Date(weather_data$DATE, format = "%Y-%m-%d")

# Definition of temperature thresholds to categorize temperature
temperature_limits <- c(-Inf, -8, 4, 15, 21, 28, Inf)
temperature_labels <- c("VERY_LOW", "LOW", "MODERATE", "HIGH", "VERY_HIGH", "HOT")

# Definition of wind speed thresholds to categorize wind speed
wind_limits <- c(-Inf, 20, 28, Inf)
wind_labels <- c("NO_WIND", "NORMAL", "WINDY")

# Set all NA values for the 'weather_code' column to 0
set_na_to_zero(weather_data, "WEATHER_CODE")

# Definition of weather code thresholds to categorize weather codes
weather_limits <- c(-Inf, 0, 80, 99)
weather_labels <- c("GOOD_WEATHER", "BAD_WEATHER", "THUNDERSTORM")

#### Sales Data ####

# Define a vector with the new column names and use colnames to assign the new names to the data frame
sales_column_names <- c("DATE", "PRODUCT_GROUP", "REVENUE")
colnames(sales_data) <- sales_column_names
sales_data$DATE <- as.Date(sales_data$DATE, format = "%Y-%m-%d")

# sales_data <- sales_data %>%
#   group_by(PRODUCT_GROUP) %>%
#   arrange(DATE) %>%
#   mutate(REVENUE_7LAG = lag(REVENUE, 365))


# Define a vector with the new column names and use colnames to assign the new names to the data frame
test_id_column_names <- c("ID", "DATE", "PRODUCT_GROUP")
colnames(test_id_data) <- test_id_column_names
test_id_data$DATE <- as.Date(test_id_data$DATE, format = "%Y-%m-%d")

# merge the test ids with the combined data
sales_data <- full_join(sales_data, test_id_data, join_by(DATE, PRODUCT_GROUP))

#### Holidays and Vacations Data ####

# Define a vector with the new column names and use colnames to assign the new names to the data frame
holidays_column_names <- c("DATE", "HOLIDAY")
colnames(holidays_data) <- holidays_column_names

# Mutate all values in the 'HOLIDAY' column to 1
holidays_data <- holidays_data %>%
  mutate(HOLIDAY = 1)

holidays_data$DATE <- as.Date(holidays_data$DATE, format = "%Y-%m-%d")

# Remove duplicates
vacations_data <- distinct(vacations_data)

# Define a vector with the new column names and use colnames to assign the new names to the data frame
vacations_column_names <- c("DATE", "VACATION")
colnames(vacations_data) <- vacations_column_names

# Change the values for vacation to 1 and remove all rows with no vacation
vacations_data <- vacations_data %>%
  mutate(VACATION = ifelse(VACATION == "Keine Ferien", 0, 1)) %>%
  filter(VACATION != 0)

vacations_data$DATE <- as.Date(vacations_data$DATE, format = "%Y-%m-%d")

#### Sports Data ####

# Define a vector with the new column names and use colnames to assign the new names to the data frame
thw_kiel_column_names <- c("DATE", "TK_GAME")
colnames(thw_kiel_data) <- thw_kiel_column_names

# Convert the date column to a date format
thw_kiel_data$DATE <- dmy(thw_kiel_data$DATE)
thw_kiel_data$DATE <- as.Date(thw_kiel_data$DATE, format = "%Y-%m-%d")

# Set all values equal to Heimspiel in the 'GAME' column to 1
thw_kiel_data <- thw_kiel_data %>%
  mutate(TK_GAME = ifelse(TK_GAME == "Heimspiel", 1, 0)) %>%
  filter(TK_GAME == 1)

thw_kiel_data$DATE <- as.Date(thw_kiel_data$DATE, format = "%Y-%m-%d")

# Define a vector with the new column names and use colnames to assign the new names to the data frame
holstein_kiel_column_names <- c("DATE", "HK_GAME")
colnames(holstein_kiel_data) <- holstein_kiel_column_names

# Set all values equal to Heimspiel in the 'GAME' column to 1
holstein_kiel_data <- holstein_kiel_data %>%
  mutate(HK_GAME = ifelse(HK_GAME == "Heimspiel", 1, 0)) %>%
  filter(HK_GAME == 1)

holstein_kiel_data$DATE <- as.Date(holstein_kiel_data$DATE, format = "%Y-%m-%d")

# Dataframe with the dates of the german world cup games
world_cup_games <- data.frame(
  DATE = c("2014-06-16", "2014-06-21", "2014-06-26", "2014-06-30", "2014-07-04","2014-07-08", "2014-07-13", "2016-06-12", "2016-06-16", "2016-06-21", "2016-07-02", "2016-07-07", "2018-06-17", "2018-06-23", "2018-06-27"),
  WC_GAME = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
)

world_cup_games$DATE <- as.Date(world_cup_games$DATE, format = "%Y-%m-%d")

#### Kiel week Data ####

# Define a vector with the new column names and use colnames to assign the new names to the data frame
kiel_week_column_names <- c("DATE", "KIEL_WEEK")
colnames(kiel_week_data) <- kiel_week_column_names

kiel_week_data$DATE <- as.Date(kiel_week_data$DATE, format = "%Y-%m-%d")

# Dataframe with the dates of the windjammerparade
windjammerparade_data <- data.frame(
  DATE = c("2012-06-23", "2013-06-29", "2014-06-28", "2015-06-27", "2016-06-25", "2017-06-24", "2018-06-23", "2019-06-29"),
  WINDJAMMERPARADE = c(1, 1, 1, 1, 1, 1, 1, 1)
)

windjammerparade_data$DATE <- as.Date(windjammerparade_data$DATE, format = "%Y-%m-%d")

#### Cruise Ships Data ####

# Define a vector with the new column names and use colnames to assign the new names to the data frame
cruise_ships_column_names <- c("DATE", "CRUISE_SHIPS")
colnames(cruise_ships_data) <- cruise_ships_column_names

# Convert the date column to a date format
cruise_ships_data$DATE <- dmy(cruise_ships_data$DATE)
cruise_ships_data$DATE <- as.Date(cruise_ships_data$DATE, format = "%Y-%m-%d")

#### Open sundays Data ####

# Define a vector with the new column names and use colnames to assign the new names to the data frame
open_sundays_column_names <- c("DATE", "OPEN_SUNDAY")
colnames(open_sundays_data) <- open_sundays_column_names

# Set all values equal to offen in the 'OPEN_SUNDAY' column to 1
open_sundays_data <- open_sundays_data %>%
  filter(OPEN_SUNDAY == "offen") %>%
  mutate(OPEN_SUNDAY = 1)

open_sundays_data$DATE <- as.Date(open_sundays_data$DATE, format = "%Y-%m-%d")

#### Economic data ####

# Dataframe with yearly unemployment rate of Kiel
unemployment_data <- data.frame(
  YEAR = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019),
  UNEMPLOYMENT_RATE = c(10.1, 10.2, 10.1, 9.9, 9.7, 9.1, 8.2, 7.6)
)

# Definition von Arbeitslosenquoten, um die Arbeitslosenquote in Kategorien einzuteilen
unemployment_limits <- c(-Inf, 9.1, 10.2)
unemployment_labels <- c("LOW", "HIGH")

# Dataframe with yearly population of Kiel
population_data <- data.frame(
  YEAR = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019),
  POPULATION = c(239.866, 241.533, 243.488, 246.306, 247.441, 247.943, 247.548, 246.947)
)

# population_data <- population_data %>%
#   mutate(across(-YEAR, ~./mean(., na.rm = TRUE)))

# Define a vector with the new column names and use colnames to assign the new names to the data frame
expenses_column_names <- c("YEAR", "EXPENSES_1P", "EXPENSES_2P", "EXPENSES_3P", "EXPENSES_4P", "EXPENSES_5P", "EXPENSES_AVG")
colnames(expenses_data) <- expenses_column_names

# Define a vector with the new column names and use colnames to assign the new names to the data frame
income_column_names <- c("YEAR", "QUARTER", "INCOME_REAL", "INCOME_REAL_YEAR", "INCOME_NOMINAL", "INCOME_NOMINAL_YEAR")
colnames(income_data) <- income_column_names

# Define a vector with the new column names and use colnames to assign the new names to the data frame
retail_prices_column_names <- c("YEAR", "MONTH", "RETAIL", "RETAIL_2")
colnames(retail_prices_data) <- retail_prices_column_names

# Define a vector with the new column names and use colnames to assign the new names to the data frame
inflation_column_names <- c("YEAR", "MONTH", "INDEX", "INDEX_YEAR", "INDEX_MONTH")
colnames(inflation_data) <- inflation_column_names

#### Special holidays ####

# Dataframe with sylvester dates
new_years_eve <- data.frame(
  DATE = c("2012-12-31", "2013-12-31", "2014-12-31", "2015-12-31", "2016-12-31", "2017-12-31", "2018-12-31", "2019-12-31"),
  NEW_YEARS_EVE = c(1,1,1,1,1,1,1,1)
)

new_years_eve$DATE <- as.Date(new_years_eve$DATE, format = "%Y-%m-%d")

# Dataframe with the days before sylvester
before_new_years <- data.frame(
  DATE = c("2012-12-30", "2013-12-30", "2014-12-30", "2015-12-30", "2016-12-30", "2017-12-30", "2018-12-30", "2019-12-30"),
  BEFORE_NEW_YEARS = c(1,1,1,1,1,1,1,1)
)

before_new_years$DATE <- as.Date(before_new_years$DATE, format = "%Y-%m-%d")

# Dataframe with the days before sylvester
before_easter <- data.frame(
  DATE = c("2012-04-05", "2013-03-28", "2014-04-15", "2014-04-16", "2014-04-17", "2014-04-19", "2015-03-30", "2015-04-01", "2015-04-02", "2015-04-04", "2016-03-22", "2016-03-23", "2016-03-24", "2016-03-26", "2017-04-11", "2017-04-12", "2017-04-13", "2017-04-15", "2018-03-27", "2018-03-28", "2018-03-29", "2018-03-31", "2019-04-16", "2019-04-17", "2019-04-18", "2019-04-20"),
  BEFORE_EASTER = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
)

before_easter$DATE <- as.Date(before_easter$DATE, format = "%Y-%m-%d")

# Dataframe with saturday during easter
easter_saturday <- data.frame(
  DATE = c("2014-04-19", "2015-04-04", "2016-03-26", "2017-04-15", "2018-03-31", "2019-04-20"),
  EASTER_SATURDAY = c(1,1,1,1,1,1)
)

easter_saturday$DATE <- as.Date(easter_saturday$DATE, format = "%Y-%m-%d")

# Dataframe with christmas eve
christmas_eve <- data.frame(
  DATE = c("2013-12-24", "2014-12-24", "2015-12-24", "2016-12-24", "2017-12-24", "2018-12-24", "2019-12-24"),
  CHRISTMAS_EVE = c(1,1,1,1,1,1,1)
)

christmas_eve$DATE <- as.Date(christmas_eve$DATE, format = "%Y-%m-%d")

# Erstellung eines tibbles mit allen Datensätzen aufgrundlage der gemeinsamen Spalte 'Datum'
complete_data <- full_join(weather_data, sales_data, by = c("DATE"))

head(complete_data)

# Zusammenfügen aller Datensätze und Erstellung der Spalten 'jahr', 'quartal', 'monat'
complete_data <- complete_data %>%
  full_join(cruise_ships_data, join_by(DATE)) %>%
  full_join(holidays_data, join_by(DATE)) %>%
  full_join(holstein_kiel_data, join_by(DATE)) %>%
  full_join(thw_kiel_data, join_by(DATE)) %>%
  full_join(open_sundays_data, join_by(DATE)) %>%
  full_join(kiel_week_data, join_by(DATE)) %>%
  full_join(windjammerparade_data, join_by(DATE)) %>%
  full_join(new_years_eve, join_by(DATE)) %>%
  full_join(before_new_years, join_by(DATE)) %>%
  full_join(before_easter, join_by(DATE)) %>%
  full_join(easter_saturday, join_by(DATE)) %>%
  full_join(christmas_eve, join_by(DATE)) %>%
  full_join(world_cup_games, join_by(DATE)) %>%
  full_join(vacations_data, join_by(DATE)) %>%
  mutate(YEAR = year(DATE)) %>%
  mutate(QUARTER = quarter(DATE)) %>%
  mutate(MONTH = month.name[month(DATE)]) %>%
  full_join(retail_prices_data, join_by(YEAR, MONTH)) %>%
  full_join(unemployment_data, join_by(YEAR)) %>%
  full_join(inflation_data, join_by(YEAR, MONTH)) %>%
  full_join(income_data, join_by(YEAR, QUARTER))
# %>%
#   full_join(population_data, join_by(YEAR))
# %>%
#   full_join(expenses_data, join_by(YEAR))

# Column names to set 0 if is NA
columns_to_set_zero <- c(
  "REVENUE", "PRODUCT_GROUP", "HOLIDAY", "KIEL_WEEK", "HK_GAME", "TK_GAME",
  "VACATION", "OPEN_SUNDAY",
  "CLOUDS", "NEW_YEARS_EVE", "BEFORE_NEW_YEARS",
  "BEFORE_EASTER", "EASTER_SATURDAY", "WC_GAME",
  "CRUISE_SHIPS", "WINDJAMMERPARADE", "CHRISTMAS_EVE","CLOUDS", "TEMPERATURE", "WIND_SPEED", "WEATHER_CODE"
)

complete_data <- set_na_to_zero(complete_data, columns_to_set_zero)

complete_data$WEEKDAY <- weekdays(complete_data$DATE)

complete_data <- complete_data %>%
  arrange(DATE) %>%
  filter(!is.na(DATE)) %>%
  filter(DATE < as.Date("2019-08-02")) # %>%
  #mutate(across(c("CLOUDS", "TEMPERATURE", "WIND_SPEED", "WEATHER_CODE"), zoo::na.approx, na.rm = FALSE))

complete_data <- complete_data %>%
  mutate(MONTH_PERIOD = case_when(
    day(DATE) %in% 1:7 ~ "START",
    day(DATE) %in% 8:22 ~ "MID",
    day(DATE) %in% 23:31 ~ "END"
  ))

# Adding values for the season based on the date and one-hot encoding
complete_data <- complete_data %>%
  mutate(SEASON = case_when(
    between(month(DATE), 1, 2) | between(month(DATE), 12, 12) ~ "WINTER",
    between(month(DATE), 3, 5) ~ "SPRING",
    between(month(DATE), 6, 8) ~ "SUMMER",
    between(month(DATE), 9, 11) ~ "AUTUMN"
  ))

# Hinzufügen einer neuen Spalte 'temperatur_kategorie' anhand der definierten Grenzen und Bezeichnungen
complete_data$TEMPERATURE_CATEGORY <- cut(complete_data$TEMPERATURE,
                                          breaks = temperature_limits,
                                          labels = temperature_labels,
                                          include.lowest = TRUE)

# Hinzufügen einer neuen Spalte 'wind_kategorie' anhand der definierten Grenzen und Bezeichnungen
complete_data$WIND_CATEGORY <- cut(complete_data$WIND_SPEED,
                                    breaks = wind_limits,
                                    labels = wind_labels,
                                    include.lowest = TRUE)

# Hinzufügen einer neuen Spalte 'wetter_kategorie' anhand der definierten Grenzen und Bezeichnungen
complete_data$WEATHER_CATEGORY <- cut(complete_data$WEATHER_CODE,
                                      breaks = weather_limits,
                                      labels = weather_labels,
                                      include.lowest = TRUE)

# Hinzufügen einer neuen Spalte 'wetter_kategorie' anhand der definierten Grenzen und Bezeichnungen
complete_data$UNEMPLOYMENT_CATEGORY <- cut(complete_data$UNEMPLOYMENT_RATE,
                                                 breaks = unemployment_limits,
                                                 labels = unemployment_labels,
                                                 include.lowest = TRUE)

complete_data <- filter(complete_data, !is.na(REVENUE) & (HOLIDAY == 0 | HOLIDAY == 1))

filtered_data <- distinct(complete_data, .keep_all = TRUE)

write.csv(filtered_data, "6_Files/filtered_data.csv", row.names = FALSE)
