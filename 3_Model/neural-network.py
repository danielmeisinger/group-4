import os
import csv
import pandas as pd
import numpy as np
from sklearn.preprocessing import StandardScaler, OneHotEncoder
from sklearn.compose import ColumnTransformer
import tensorflow as tf
from tensorflow.keras import layers, callbacks, optimizers, Sequential
from tensorflow.keras.layers import Dense, Dropout
from tensorflow.keras.callbacks import ModelCheckpoint
from tensorflow.keras.regularizers import l2

def mape(y_true, y_pred):
    y_true, y_pred = np.array(y_true), np.array(y_pred)
    non_zero_mask = y_true != 0
    return np.mean(np.abs((y_true[non_zero_mask] - y_pred[non_zero_mask]) / y_true[non_zero_mask])) * 100

# def zscore_normalize_columns(df, columns_to_normalize):
#     for column in columns_to_normalize:
#         # Calculate mean and standard deviation
#         mean_val = df[column].mean()
#         std_dev = df[column].std()
        
#         # Create a new column with normalized values
#         new_column_name = f'{column}_normalized'
#         df[new_column_name] = (df[column] - mean_val) / std_dev
        
#         # Drop the original column
#         df.drop(column, axis=1, inplace=True)
    
#     return df

# Get the current script's directory
script_dir = os.path.dirname(os.path.abspath(__file__))

# Construct the relative path to the data file
data_path = os.path.join(script_dir, '../6_Files/filtered_data.csv')
data = pd.read_csv(data_path)
# Sort data by date
data['DATE'] = pd.to_datetime(data['DATE'])
data = data.sort_values(by='DATE')

# columns_to_normalize = ['UNEMPLOYMENT_RATE','INDEX','CLOUDS','TEMPERATURE','WIND_SPEED','RETAIL','RETAIL_2','INCOME_REAL','INCOME_NOMINAL']

# data = zscore_normalize_columns(df=data,columns_to_normalize=columns_to_normalize)

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
X_train = train_data.drop(['REVENUE', 'ID','YEAR','DATE','WEATHER_CODE'], axis=1)
y_train = train_data['REVENUE']

X_val = val_data.drop(['REVENUE', 'ID','YEAR','DATE','WEATHER_CODE'], axis=1)
y_val = val_data['REVENUE']

X_test = test_data.drop(['REVENUE', 'ID','YEAR','DATE','WEATHER_CODE'], axis=1)

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
model = Sequential([
    layers.InputLayer(input_shape=(X_train_preprocessed.shape[1],)),
    layers.BatchNormalization(),
    Dense(128, activation='relu'),
    # Dropout(0.1),
    Dense(128, activation='relu'),
    Dense(96, activation='relu'),
    # Dropout(0.1),
    Dense(128, activation='relu',kernel_regularizer=l2(0.01)),
    # Dense(256, activation='relu',kernel_regularizer=l2(0.01)),
    # Dense(128, activation='relu',kernel_regularizer=l2(0.01)),
    # Dense(96, activation='relu'),
    Dropout(0.1),
    Dense(1)
])

# model = Sequential([
#     Dense(64, activation='relu', input_shape=(X_train_preprocessed.shape[1],)),
#     layers.BatchNormalization(),
#     Dense(64, activation='relu'),
#     Dense(128, activation='relu'),
#     Dense(128, activation='relu'),
#     Dropout(0.1),
#     Dense(1)
# ])


# model = Sequential([
#     layers.Dense(128, activation='relu', input_shape=(X_train_preprocessed.shape[1],)),
#     layers.BatchNormalization(),
#     layers.Dense(64, activation='relu'),
#     # layers.Dropout(0.1),
#     layers.Dense(128, activation='relu'),
#     # layers.Dense(32, activation='relu'),
#     layers.Dense(64, activation = 'relu'),
#     # layers.Dense(32, activation = 'relu'),
#     # layers.Dense(32, activation='relu'),
#     layers.Dropout(0.1),
#     layers.Dense(1)
# ])

# model = Sequential([
#     layers.InputLayer(input_shape=(X_train_preprocessed.shape[1],)),
#     layers.BatchNormalization(),
#     layers.Dense(20, activation='relu'),
#     layers.Dense(4, activation='relu'),
#     layers.Dense(16, activation='relu'),
#     layers.Dense(1)
# ])

custom_optimizer = optimizers.legacy.Adam(learning_rate=0.0005)  # Adjust the learning rate as needed
model.compile(optimizer=custom_optimizer, loss='mse')

model.summary()

# Create EarlyStopping callback
# early_stopping = callbacks.EarlyStopping(monitor='val_loss', patience=100, restore_best_weights=True)

# Train the model
history = model.fit(
    X_train_preprocessed, y_train,
    epochs=250,
    batch_size=76,
    validation_data=(X_val_preprocessed, y_val),
    callbacks=[checkpoint]
)

# Load the best model
best_model = tf.keras.models.load_model(checkpoint_path)


# Make predictions on the test set

val_prediction = best_model.predict(X_val_preprocessed)

val_data['REVENUE_PREDICTION'] = val_prediction

val_predictions_path = os.path.join(script_dir, '../5_Data/validation_prediction.csv')

val_data.to_csv(val_predictions_path, index=False)

test_predictions = best_model.predict(X_test_preprocessed)
test_predictions_model = model.predict(X_test_preprocessed)


# Combine predictions with the ID column
predictions_df = pd.DataFrame({
    'id': test_data['ID'],
    'Umsatz': test_predictions.flatten()
})

# Remove entries with empty 'id'
predictions_df = predictions_df[predictions_df['id'].notna()]

predictions_df_model = pd.DataFrame({
    'id': test_data['ID'],
    'Umsatz': test_predictions_model.flatten()
})

# Remove entries with empty 'id'
predictions_df_model = predictions_df_model[predictions_df_model['id'].notna()]

predictions_path = os.path.join(script_dir, '../6_Files/kaggle_submission_29.csv')
predictions_model_path = os.path.join(script_dir, '../6_Files/kaggle_submission_29_model.csv')

predictions_df.to_csv(predictions_path, index=False)
predictions_df_model.to_csv(predictions_model_path, index=False)

# Assuming you have a 'history' object with loss and val_loss data
loss_data = history.history['loss']
val_loss_data = history.history['val_loss']

csv_file = os.path.join(script_dir, '../5_Data/loss_data.csv')

# Create lists for 'Epoch', 'Training Loss', and 'Validation Loss'
epochs = list(range(1, len(loss_data) + 1))
training_loss = loss_data
validation_loss = val_loss_data




# Write the data to a CSV file
with open(csv_file, 'w', newline='') as file:
    writer = csv.writer(file)
    writer.writerow(['Epoch', 'Training Loss', 'Validation Loss'])  # Header
    writer.writerows(zip(epochs, training_loss, validation_loss))