# Need to execute dataset.R from 0_DataPreparation
# to get variable 'filtered_data'
source("0_DataPreparation/dataset.R")

# Divide the dataset into a prediction dataset and a validation dataset
# depending on the Datum variable
umsatz_prediction_training_data <- filtered_data %>%
  filter(Datum >= "2013-07-01" & Datum <= "2017-07-31")
umsatz_prediction_validation_data <- filtered_data %>%
  filter(Datum >= "2017-08-01" & Datum <= "2018-07-13")

# Set up a linear model for the training dataset
lm_umsatz_prediction_training_data <- lm(Umsatz ~ Bewoelkung + Temperatur + Windgeschwindigkeit + Wettercode + Warengruppe + as.factor(KielerWoche) + as.factor(holstein_spiel) + as.factor(thw_spiel) + as.factor(Ferien) + as.factor(flohmarkt), data = umsatz_prediction_training_data)

# regression diagnostics
summary(lm_umsatz_prediction_training_data)
plot(lm_umsatz_prediction_training_data)
