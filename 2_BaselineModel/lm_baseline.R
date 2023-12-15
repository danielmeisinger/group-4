# Need to execute dataset.R from 0_DataPreparation to get variable 'filtered_data'

source("0_DataPreparation/dataset.R")

# Divide the dataset into a prediction dataset and a validation dataset depending on the Datum variable
umsatz_prediction_training_data <- gefilterte_daten %>%
  filter(Datum >= "2013-07-01" & Datum <= "2017-07-31")

umsatz_prediction_validation_data <- gefilterte_daten %>%
  filter(Datum >= "2017-08-01" & Datum <= "2018-07-13")


# Set up a linear model for the training dataset
lm_umsatz_prediction_training_data <- lm(Umsatz ~ Weekday + Temperatur + Wettercode + as.factor(temperatur_kategorie) * as.factor(wind_kategorie) * as.factor(Warengruppe) * as.factor(KielerWoche)  * as.factor(feiertag) * as.factor(ferien) + as.factor(jahreszeit) + as.factor(holstein_spiel) * as.factor(thw_spiel) * as.factor(flohmarkt) * as.factor(temperatur_kategorie) * as.factor(jahreszeit) + as.factor(arbeitslosenquote_kategorie) + einwohnerzahl, data = umsatz_prediction_training_data)

# regression diagnostics
summary(lm_umsatz_prediction_training_data)

# plot(lm_umsatz_prediction_training_data)

# calculate the values for the predicted Umsatz using the validation dataset and compare with the real values using mean squared error
umsatz_prediction_validation_data$Umsatz_prediction <- predict(lm_umsatz_prediction_training_data, umsatz_prediction_validation_data)
mse <- mean((umsatz_prediction_validation_data$Umsatz - umsatz_prediction_validation_data$Umsatz_prediction)^2, na.rm = TRUE)
print(mse)
