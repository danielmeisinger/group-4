import pandas as pd
import numpy as np
from scipy.stats import uniform, randint
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler, OneHotEncoder
from sklearn.compose import ColumnTransformer
from sklearn.model_selection import RandomizedSearchCV
from scikeras.wrappers import KerasRegressor
import tensorflow as tf
from tensorflow import keras
from tensorflow.keras import layers, callbacks, optimizers
from tensorflow.keras.layers import Dense, Dropout
from tensorflow.keras.callbacks import ModelCheckpoint


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

def create_model(optimizer='adam', loss='mean_squared_error'):
    model = keras.Sequential([
    layers.InputLayer(input_shape=(X_train_preprocessed.shape[1],)),
    layers.Dense(128, activation='relu'),
    layers.Dropout(0.2),
    layers.Dense(64, activation='relu'),
    layers.Dropout(0.2),
    layers.Dense(32, activation='relu'),
    layers.Dense(1)
    ])
    model.compile(optimizer=optimizer, loss=loss)
    return model

# Create the KerasRegressor object with the build function
model = KerasRegressor(model=create_model, epochs=10, batch_size=32, verbose=0)

# Define your optimizer
custom_optimizer = optimizers.legacy.Adam(learning_rate=0.001)

# Compile the model within the KerasRegressor
model.set_params(optimizer=custom_optimizer, loss='mean_squared_error')

param_dist = {
    'batch_size': randint(16, 128),
    'epochs': randint(50, 150),
#    'learning_rate': [0.001, 0.01, 0.1],
#    'dropout_rate': [0.01, 0.05, 0.1, 0.2, 0.3]
}

# Perform RandomizedSearchCV
random_search = RandomizedSearchCV(estimator=model, param_distributions=param_dist, n_iter=10, scoring='neg_mean_squared_error', cv=3)
random_result = random_search.fit(X_train_preprocessed, y_train)

print(f"Best Parameters: {random_result.best_params_}")
print(f"Best MSE: {random_result.best_score_}")