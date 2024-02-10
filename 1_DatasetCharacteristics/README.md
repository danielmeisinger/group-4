# Dataset Characteristics

The dataset analyzed in the script "dataset_analysis.r" is stored in the data frame *filtered_data*. It contains 9334 observations of the following 50 variables:

-   *ID*
-   *DATE*
-   *REVENUE*
-   *PRODUCT_GROUP*
-   *CLOUDS*, *TEMPERATURE*, *WIND_SPEED*, *WEATHER_CODE*, *TEMPERATURE_CATEGORY*, *WIND_CATEGORY*, *WEATHER_CATEGORY*
-   *CRUISE_SHIPS*
-   *HOLIDAYS*, *VACATIONS*, *OPEN_SUNDAY*, *BEFORE_EASTER*, *EASTER_SUNDAY*, *CHRISTMAS_EVE*, *BEFORE_NEW_YEARS*, *NEW_YEARS_EVE*
-   *HK_GAME*, *TK_GAME*, *WC_GAME*
-   *KIEL_WEEK*, *WINDJAMMERPARADE*
-   *YEAR*, *QUARTER*, *MONTH*, *WEEKDAY*, *MONTH_PERIOD*, *SEASON*
-   *RETAIL*, *RETAIL_2*
-   *POPULATION*, *UNEMPLOYMENT_RATE*, *UNEMPLOYMENT_CATEGORY*
-   *INCOME_REAL*, *INCOME_REAL_YEAR*, *INCOME_NOMINAL*, *INCOME_NOMINAL_YEAR*
-   *INDEX*, *INDEX_YEAR*, *INDEX_MONTH*
-   *EXPENSES_1P*, *EXPENSES_2P*, *EXPENSES_3P*, *EXPENSES_4P*, *EXPENSES_5P*, *EXPENSES_AVG*

How these variables are calculated is described in the README of 0_DataPreparation.

In this analysis of the dataset characteristics, the mean revenue within groups determined by the following variables is calculated:

-   *NEW_YEARS_EVE*
-   *NEW_YEARS_EVE*\**PRODUCT_GROUP*
-   *CRUISE_SHIPS*
-   *HK_GAME*
-   *HK_GAME*\**PRODUCT_GROUP*
-   *WEATHER_CATEGORY*
-   *UNEMPLOYMENT_CATEGORY*
-   *SEASON*

Also the bivariate distribution of the revenue and the retail prices is analysed via a scatter plot.
