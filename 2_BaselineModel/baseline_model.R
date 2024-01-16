# Need to execute data-preparation.R from 0_DataPreparation to get variable 'filtered_data'

source("0_DataPreparation/data_preparation.R")

# Divide the dataset into a prediction dataset and a validation dataset depending on the Datum variable
revenue_prediction_training_data <- filtered_data %>%
  filter(DATE >= "2013-07-01" & DATE <= "2017-07-31")

revenue_prediction_validation_data <- filtered_data %>%
  filter(DATE >= "2017-08-01" & DATE <= "2018-07-31")

revenue_prediction_test_data <- filtered_data %>%
  filter(DATE >= "2018-08-01" & DATE <= "2019-07-30")


# Set up a linear model for the training dataset
weights <- ifelse(revenue_prediction_training_data$NEW_YEARS_EVE == TRUE, 30,
                  ifelse(revenue_prediction_training_data$BEFORE_NEW_YEARS == TRUE, 25,
                         ifelse(revenue_prediction_training_data$KIEL_WEEK == TRUE, 4,
                                ifelse(revenue_prediction_training_data$CRUISE_SHIPS > 2, 2,
                                       ifelse(revenue_prediction_training_data$WEATHER_CATEGORY == "THUNDERSTORM", 2, 1)))))

lm_revenue_prediction_training_data <- lm(REVENUE ~ as.factor(WEEKDAY) * as.factor(PRODUCT_GROUP) +
                                            as.logical(VACATION) * as.factor(PRODUCT_GROUP) * INDEX_MONTH +
                                            as.logical(KIEL_WEEK) * as.factor(PRODUCT_GROUP) +
                                            as.logical(NEW_YEARS_EVE) * as.factor(PRODUCT_GROUP) * INDEX_MONTH +
                                            as.logical(BEFORE_NEW_YEARS) * as.factor(PRODUCT_GROUP) * INDEX_MONTH +
                                            as.logical(BEFORE_EASTER) * as.factor(PRODUCT_GROUP) +
                                            as.logical(WINDJAMMERPARADE) * as.factor(PRODUCT_GROUP) * INDEX_MONTH +
                                            as.logical(CHRISTMAS_EVE) * as.factor(PRODUCT_GROUP) +
                                            as.factor(MONTH) * as.factor(PRODUCT_GROUP) +
                                            as.logical(HK_GAME) * as.factor(PRODUCT_GROUP) +
                                            log(RETAIL) * I(INDEX_YEAR) * as.factor(PRODUCT_GROUP) +
                                            as.factor(MONTH_PERIOD) * as.factor(PRODUCT_GROUP) * INDEX_MONTH +
                                            as.factor(CRUISE_SHIPS) +
                                            as.logical(SEASONAL_BRED) * as.factor(PRODUCT_GROUP) +
                                            CRUISE_SHIPS * as.factor(PRODUCT_GROUP) * as.factor(WEEKDAY) +
                                            POPULATION +
                                            log(EXPENSES_AVG) +
                                            as.logical(HK_GAME) * I(TEMPERATURE) * as.factor(PRODUCT_GROUP) +
                                            TEMPERATURE * WEATHER_CODE * as.factor(PRODUCT_GROUP) +
                                            CLOUDS +
                                            as.factor(WEATHER_CATEGORY) +
                                            as.logical(HK_GAME) * TEMPERATURE * CLOUDS * as.factor(PRODUCT_GROUP),
                                          data = revenue_prediction_training_data, weights = weights)

# summary of the linear model
# Assuming lm_revenue_prediction_training_data is your linear regression model
model_summary <- summary(lm_revenue_prediction_training_data)
print(model_summary)


# calculate the values for the predicted Umsatz using the validation dataset and compare with the real values using mean squared error
revenue_prediction_training_data$REVENUE_PREDICTION <- predict(lm_revenue_prediction_training_data, revenue_prediction_training_data)
mse_train <- mean((revenue_prediction_training_data$REVENUE - revenue_prediction_training_data$REVENUE_PREDICTION)^2, na.rm = TRUE)
print(mse_train)

mape_train <- mean(abs((revenue_prediction_training_data$REVENUE - revenue_prediction_training_data$REVENUE_PREDICTION)) / revenue_prediction_training_data$REVENUE, na.rm = TRUE)
print(mape_train)

revenue_prediction_validation_data$REVENUE_PREDICTION <- predict(lm_revenue_prediction_training_data, revenue_prediction_validation_data)
mse <- mean((revenue_prediction_validation_data$REVENUE - revenue_prediction_validation_data$REVENUE_PREDICTION)^2, na.rm = TRUE)
print(mse)
mape <- mean(abs((revenue_prediction_validation_data$REVENUE - revenue_prediction_validation_data$REVENUE_PREDICTION)) / revenue_prediction_validation_data$REVENUE, na.rm = TRUE)
print(mape)

revenue_prediction_validation_data <- revenue_prediction_validation_data %>%
  mutate(DIFFERENCE = abs(REVENUE - REVENUE_PREDICTION))

# Find the row with the biggest difference
date_max_difference <- revenue_prediction_validation_data[which.max(revenue_prediction_validation_data$DIFFERENCE), ]

# Print the date with the biggest difference
print(date_max_difference$DATE)

revenue_prediction_test_data$REVENUE_PREDICTION <- predict(lm_revenue_prediction_training_data, revenue_prediction_test_data)

kaggle_submission_data <- revenue_prediction_test_data %>%
  select(ID, REVENUE_PREDICTION) %>%
  rename(REVENUE = REVENUE_PREDICTION)

kaggle_submission_data <-  kaggle_submission_data[complete.cases(kaggle_submission_data$ID), ]

kaggle_submission_data <- kaggle_submission_data %>%
  mutate_all(~ifelse(. < 0, 5, .)) %>%
  mutate(REVENUE = coalesce(REVENUE, 0))

# Define a vector with the new column names and use colnames to assign the new names to the data frame
kaggle_submission_column_names <- c("id", "Umsatz")
colnames(kaggle_submission_data) <- kaggle_submission_column_names

# Current date and time
current_datetime <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

# Zusammenfügen der aktuellen Zeit mit dem Dateinamen
file_name <- paste0(current_datetime, "_kaggle_submission.csv")

validation_data <- revenue_prediction_validation_data %>%
  select(DATE, REVENUE)

val_file_name <- paste0(current_datetime, "_validation_data.csv")

# csv Datei mit der aktuellen Zeit
write.csv(validation_data, val_file_name, row.names = FALSE)

# csv Datei mit der aktuellen Zeit
write.csv(kaggle_submission_data, file_name, row.names = FALSE)
