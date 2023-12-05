# Load libraries
library(dplyr)
library(readr)
library(ggplot2)
library(styler)
library(skimr)
library(DataExplorer)

# import csv files
sales_data <- read_csv("Data/umsatzdaten_gekuerzt.csv")

# turn dates into weekdays
sales_data$Weekday <- weekdays(sales_data$Datum)

weather_data <- read_csv("Data/wetter.csv")
kiwo_days <- read_csv("Data/kiwo.csv")
holidays <- read_csv("Data/feiertage.csv")
schulferien <- read_csv("Data/Schulferien_2.csv")

# create a tibble with the sales data, weather data and kiwo days
combined_data <- left_join(weather_data, kiwo_days, join_by(Datum)) %>% left_join(holidays, join_by(Datum))  %>% left_join(sales_data, join_by(Datum)) %>% left_join(schulferien, join_by(Datum))

# drop columns in which Feiertage and Umsatz is NA
combined_data <- combined_data %>% filter(!is.na(Feiertag) | !is.na(Umsatz))
