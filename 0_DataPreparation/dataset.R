# Load libraries
library(dplyr)
library(readr)
library(ggplot2)
library(styler)
library(skimr)
library(DataExplorer)

# import csv files
sales_data <- read_csv("Data/umsatzdaten_gekuerzt.csv")
weather_data <- read_csv("Data/wetter.csv")
kiwo_days <- read_csv("Data/kiwo.csv")
holidays <- read_csv("Data/feiertage.csv")

# create a tibble with the sales data, weather data and kiwo days
combined_data <- left_join(weather_data, sales_data) %>%
  left_join(kiwo_days) %>% left_join(holidays)
skim(combined_data)
