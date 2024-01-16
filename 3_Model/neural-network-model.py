# Import needed Python libraries and functions
import numpy as np
import tensorflow as tf
import pandas as pd
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import InputLayer, Dense, BatchNormalization, Dropout
from tensorflow.keras.optimizers import Adam

# Import your training and validation data
data_path = '/Users/daniel/Documents/10-projects/data-science-project/group-4/6_Files'
training_features = pd.read_csv(f'{data_path}/revenue_prediction_training_data_features_2.csv')
training_labels = pd.read_csv(f'{data_path}/revenue_prediction_training_data_label_2.csv')
validation_features = pd.read_csv(f'{data_path}/revenue_prediction_validation_data_features_2.csv')
validation_labels = pd.read_csv(f'{data_path}/revenue_prediction_validation_data_label_2.csv')
test_features = pd.read_csv(f'{data_path}/revenue_prediction_test_data_features_2.csv')

# The argument "input_shape" for the definition of the input layer must include the number input variables (features) used for the model. To automatically calculate this number, we use the  function `r.training_features.keys()`, which returns the list of variable names of the dataframe `training_features`. Then, the function `len()` returns the length of this list of variable names (i.e. the number of variables in the input).

model = Sequential([
    InputLayer(input_shape=(training_features.shape[1], )),
  BatchNormalization(),
  Dense(64, activation='relu'),
  Dropout(.2),
  Dense(64, activation='relu'),
  Dense(1)
])

# Ausgabe einer Zusammenfassung zur Form des Modells, das geschaetzt wird (nicht notwendig)
model.summary()

model.compile(loss="mse", optimizer=Adam(learning_rate=0.01))

history = model.fit(training_features, training_labels, epochs=200,
                    validation_data=(validation_features, validation_labels))

# Extract IDs and prepare test features
test_ids = test_features['ID']
test_features_for_prediction = test_features.drop(columns=['ID'])

# Make predictions
test_predictions = model.predict(test_features_for_prediction)

# Create a DataFrame for submission
predictions_df = pd.DataFrame({
    'id': test_ids,
    'Umsatz': test_predictions.flatten()
})

# Export to CSV
print(predictions_df)

predictions_df.to_csv('kaggle_submission.csv', index=False)