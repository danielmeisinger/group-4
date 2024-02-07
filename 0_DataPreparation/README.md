# Data Preparation

The data collected under "5_Data" is prepared for the analysis here. The first file "data_preparation.R" contains the preprocessing for the baseline model whereas the other file "data_preparation.R" prepares the data for analysis via an ML model.

The csv.files from the subdirectory "5_Data" are imported into data frames. The columns are then named informatively and the date variable of every data frame is converted into the YYYY-MM-DD format.

* For the data on weather, contained in "weather_data",  Index variables to classify the weather based on temperatur (very low, low, moderate, high, very high, hot) and wind speed (no wind, normal, windy) as well as the presence of a thunderstorm are created. NA values of the weather code are set to zero.

* The sales data are joined with the ids provided by the kaggle file.

* The data frame "holidays_data" is converted into an index variable for the occurence of a official holiday in Germany. The data on school vacations in Germany, contained in "vacation_data" is processed similiarly.

* The data on sports events occuring in Kiel are collected in the data frames "thw_kiel_data" and "holstein_kiel_data". These data frames are converted into index variables. Additionally, the dates of men's world cup games in Germany in soccer is coded in an additional index variable.

* The data frame "kiel_week_data" contains the dates of all days on which the Kiel Week took place. Additionally, a index variable for all dates where the Windjammerparade as part of the Kiel Wee took place is created.

* The data frame "cruise_ship_data" contains the dates an number of cruise ships arrivals in Kiel.

* The dates of the Open Sundays in Germany in the relevant period are encoded in "open_sundays_data".

* Statistical data on the economy in Germany is contained in the data frames "unemployment_data", "population_data", "inflation_data", "expenses_data" and "income_data". The unemployment is classified (low, high) and converted into an index variable. The population data refers to the population in Kiel and is converted into percentage of the mean over the relevant period. The income data of households of various sizes is similiarly converted.

* Data related to special holidays is contained in "new_years_eve", "before_new_years" (day before new years eve), "before_easter" (dates before easter), "easter_saturday" & "easter_sunday" and "christmas_eve". All these data frames index the dates of the corresponding holidays or derivated dates.

All data frames are then joined into one comprehensive data frame. The NA values created by joining data frames containing every data of the observation period and data frame indexing only individual dates (e.g. special holidays) are set to zero. Entries refering to dates after the 2019-08-02 (resulting from the weather data) are excluded. Additionally, variables encoding the time of the month (start, mid, end) and the season are added.
