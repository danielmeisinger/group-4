# Need to execute dataset.R from 0_DataPreparation to get variable 'filtered_data'

source("0_DataPreparation/dataset.R")

# Divide the dataset into a prediction dataset and a validation dataset depending on the Datum variable
umsatz_prediction_training_data <- gefilterte_daten %>%
  filter(Datum >= "2013-07-01" & Datum <= "2017-07-31")

umsatz_prediction_validation_data <- gefilterte_daten %>%
  filter(Datum >= "2017-08-01" & Datum <= "2018-07-31")

umsatz_prediction_test_data <- gefilterte_daten %>%
  filter(Datum >= "2018-08-01" & Datum <= "2019-07-30")


# Set up a linear model for the training dataset
# lm_umsatz_prediction_training_data <- lm(Umsatz ~ as.factor(wochentag) + Temperatur * Wettercode + Bewoelkung + as.factor(temperatur_kategorie) * as.factor(wind_kategorie) * as.factor(Warengruppe) * as.logical(KielerWoche)  * as.logical(feiertag) * as.logical(ferien) + as.factor(jahreszeit) * wochentag + as.logical(holstein_spiel) * as.logical(thw_spiel) * as.logical(flohmarkt) * as.factor(temperatur_kategorie) + as.factor(jahreszeit) + as.factor(arbeitslosenquote_kategorie) + as.factor(monat) + as.logical(sylvester) * as.factor(Warengruppe) + as.logical(tage_vor_ostern) * as.factor(Warengruppe), data = umsatz_prediction_training_data)

weights <- ifelse(umsatz_prediction_training_data$sylvester == TRUE, 30,
                  ifelse(umsatz_prediction_training_data$vor_sylvester == TRUE, 5,
                         ifelse(umsatz_prediction_training_data$KielerWoche == TRUE, 4,1)))

lm_umsatz_prediction_training_data <- lm(Umsatz ~ as.factor(wochentag) * as.factor(Warengruppe) + as.logical(ferien) * as.factor(Warengruppe) * index_month + as.factor(Warengruppe) * as.logical(KielerWoche) + as.logical(sylvester) * as.factor(Warengruppe) * index_month + as.logical(vor_sylvester) * as.factor(Warengruppe) * index_month + as.factor(monat) * as.factor(Warengruppe) + Temperatur * Wettercode * as.factor(Warengruppe) + Bewoelkung + as.factor(wetter_kategorie) * as.factor(Warengruppe) + as.logical(tage_vor_ostern) * as.factor(Warengruppe) + as.logical(holstein_spiel) * as.factor(Warengruppe) + as.factor(arbeitslosenquote_kategorie) * index_year_month + log(retail1) * I(index_year_month) * as.factor(Warengruppe) + as.factor(monatsabschnitt) * as.factor(Warengruppe) * index_month + as.factor(Count) * Bewoelkung, data = umsatz_prediction_training_data, weights = weights)

# regression diagnostics
summary(lm_umsatz_prediction_training_data)

# plot(lm_umsatz_prediction_training_data)

# calculate the values for the predicted Umsatz using the validation dataset and compare with the real values using mean squared error
umsatz_prediction_validation_data$umsatz_prediction <- predict(lm_umsatz_prediction_training_data, umsatz_prediction_validation_data)
mse <- mean((umsatz_prediction_validation_data$Umsatz - umsatz_prediction_validation_data$umsatz_prediction)^2, na.rm = TRUE)
print(mse)
mape <- mean(abs((umsatz_prediction_validation_data$Umsatz - umsatz_prediction_validation_data$umsatz_prediction)) / umsatz_prediction_validation_data$Umsatz, na.rm = TRUE) * 100
print(mape)

umsatz_prediction_validation_data <- umsatz_prediction_validation_data %>%
  mutate(Difference = abs(Umsatz - umsatz_prediction))

# Find the row with the biggest difference
max_difference_row <- umsatz_prediction_validation_data[which.max(umsatz_prediction_validation_data$Difference), ]

# Print the date with the biggest difference
print(max_difference_row$Datum)

umsatz_prediction_test_data$umsatz_prediction <- predict(lm_umsatz_prediction_training_data, umsatz_prediction_test_data)

kaggle_submission <- umsatz_prediction_test_data %>%
  select(id, umsatz_prediction) %>%
  rename(Umsatz = umsatz_prediction)

kaggle_submission <-  kaggle_submission[complete.cases(kaggle_submission$id), ]

kaggle_submission <- kaggle_submission %>%
  mutate_all(~ifelse(. < 0, 0.1, .)) %>%
  mutate(Umsatz = coalesce(Umsatz, 0))

write.csv(kaggle_submission, "kaggle_submission.csv", row.names = FALSE)
