import pandas as pd
import numpy as np
from sklearn.preprocessing import StandardScaler, OneHotEncoder
from sklearn.compose import ColumnTransformer
import tensorflow as tf
from tensorflow.keras import layers, callbacks, optimizers, Sequential
from tensorflow.keras.layers import Dense, Dropout
from tensorflow.keras.callbacks import ModelCheckpoint
from tensorflow.keras.regularizers import l2

# Load your data
data_path = '/Users/daniel/Documents/10-projects/data-science-project/group-4/6_Files/filtered_data.csv'  # Replace with the actual path to your CSV file
data = pd.read_csv(data_path)
# Sort data by date
data['DATE'] = pd.to_datetime(data['DATE'])
data = data.sort_values(by='DATE')

# Define date ranges for training, validation, and test sets
train_start_date = pd.to_datetime('2013-07-01')
train_end_date = pd.to_datetime('2017-07-31')
val_start_date = pd.to_datetime('2017-08-01')
val_end_date = pd.to_datetime('2018-07-31')
test_start_date = pd.to_datetime('2018-08-01')
test_end_date = pd.to_datetime('2019-07-30')

# Split data into training, validation, and test sets
train_data = data[(data['DATE'] >= train_start_date) & (data['DATE'] <= train_end_date)]
val_data = data[(data['DATE'] >= val_start_date) & (data['DATE'] <= val_end_date)]
test_data = data[(data['DATE'] >= test_start_date) & (data['DATE'] <= test_end_date)]

# Separate features and target variable
X_train = train_data.drop(['REVENUE', 'ID','YEAR','DATE'], axis=1)
y_train = train_data['REVENUE']

X_val = val_data.drop(['REVENUE', 'ID','YEAR','DATE'], axis=1)
y_val = val_data['REVENUE']

X_test = test_data.drop(['REVENUE', 'ID','YEAR','DATE'], axis=1)

# Define columns to be one-hot encoded (replace 'categorical_column' with your actual column name)
categorical_columns = ['PRODUCT_GROUP','QUARTER','WEEKDAY','MONTH','MONTH_PERIOD','SEASON','TEMPERATURE_CATEGORY','WIND_CATEGORY','UNEMPLOYMENT_CATEGORY','WEATHER_CATEGORY']

# Define columns to be standardized (exclude categorical columns)
numeric_columns = [col for col in X_train.columns if col not in categorical_columns]

# Create preprocessor with a ColumnTransformer
preprocessor = ColumnTransformer(
    transformers=[
        ('numeric', StandardScaler(), numeric_columns),
        ('categorical', OneHotEncoder(), categorical_columns)
    ],
    remainder='passthrough'
)

# Apply preprocessing to the training, validation, and test sets
X_train_preprocessed = preprocessor.fit_transform(X_train)
X_val_preprocessed = preprocessor.transform(X_val)
X_test_preprocessed = preprocessor.transform(X_test)

# Check for NaN values in input data
nan_check = np.isnan(X_train_preprocessed).sum()
if nan_check > 0:
    raise ValueError("NaN values found in the input data.")

# Check for NaN values in target variable
nan_check_y = np.isnan(y_train).sum()
if nan_check_y > 0:
    raise ValueError("NaN values found in the target variable.")

checkpoint_path = "best_model.keras"
checkpoint = tf.keras.callbacks.ModelCheckpoint(
    filepath=checkpoint_path,
    monitor='val_loss',  # Monitor the sum of train_loss and val_loss
    save_best_only=True,
    mode='min',  # Minimize the sum of train_loss and val_loss
    verbose=1,
)

# # Build the neural network model
# model = Sequential([
#     Dense(128, activation='relu', input_shape=(X_train_preprocessed.shape[1],)),
#     Dropout(0.1),
#     Dense(128, activation='relu'),
#     Dropout(0.1),
#     Dense(256, activation='relu',kernel_regularizer=l2(0.001)),
#     layers.BatchNormalization(),
#     Dense(128, activation='relu',kernel_regularizer=l2(0.01)),
#     Dropout(0.05),
#     Dense(1)
# ])

# Build the neural network model
model = Sequential([
    Dense(64, activation='relu', input_shape=(X_train_preprocessed.shape[1],)),
    Dropout(0.1),
    Dense(128, activation='relu'),
    Dropout(0.1),
    Dense(128, activation='tanh',kernel_regularizer=l2(0.01)),
    Dense(64, activation='relu',kernel_regularizer=l2(0.01)),
    #Dense(48, activation='relu',kernel_regularizer=l2(0.01)),
    Dropout(0.1),
    Dense(1)
])

# model = keras.Sequential([
#     layers.Dense(128, activation='relu', input_shape=(X_train_preprocessed.shape[1],)),
#     layers.Dense(64, activation='relu', kernel_regularizer=l2(0.05)),
#     # layers.Dropout(0.2),
#     # layers.Dense(128, activation='relu', kernel_regularizer=l2(0.05)),
#     # layers.Dense(32, activation='relu'),
#     layers.Dropout(0.2),
#     # layers.Dense(64, activation = 'relu', kernel_regularizer=l2(0.1)),
#     # layers.Dense(32, activation = 'relu'),
#     # layers.Dropout(0.1),
#     layers.Dense(32, activation='relu'),
#     layers.Dense(1)
# ])

# model = keras.Sequential([
#     layers.InputLayer(input_shape=(X_train_preprocessed.shape[1],)),
#     layers.Dense(12, activation='relu'),
#     layers.Dropout(0.2),
#     layers.Dense(4, activation='relu'),
#     layers.Dense(1)
# ])

custom_optimizer = optimizers.legacy.Adam(learning_rate=0.00075)  # Adjust the learning rate as needed
model.compile(optimizer=custom_optimizer, loss='mse')

# Create EarlyStopping callback
# early_stopping = callbacks.EarlyStopping(monitor='val_loss', patience=100, restore_best_weights=True)

# Train the model
history = model.fit(
    X_train_preprocessed, y_train,
    epochs=200,
    batch_size=48,
    validation_data=(X_val_preprocessed, y_val),
    callbacks=[checkpoint]
)

# Load the best model
best_model = tf.keras.models.load_model(checkpoint_path)

# Make predictions on the test set
test_predictions = best_model.predict(X_test_preprocessed)

# Combine predictions with the ID column
predictions_df = pd.DataFrame({
    'ID': test_data['ID'],
    'Predicted_REVENUE': test_predictions.flatten()
})

print(predictions_df)

predictions_df.to_csv('/Users/daniel/Documents/10-projects/data-science-project/group-4/6_Files/kaggle_submission_61.csv')