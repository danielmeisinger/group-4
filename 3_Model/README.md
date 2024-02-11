# Model Definition and Evaluation

A neural network was established to predict the price of bakery goods, similar to the baseline model.
The prediction used two hidden layers with 4 and 16 neurons respectively. The learning rate and batch size were kept at 0.001 and 32. A custom checkpoint was used to save the neural network model with the lowest validation loss, as well as the neural network model after the completion of 200 epochs.

**Results Summary**
  Best Model: 2024-01-19_0192-best_model.keras
  Evaluation Metric: MAPE
  Result by Category (Identifier):
    Bread (1): 18.87%
    Rolls (2): 31.98%
    Croissant (3): 22.13%
    Confectionery (4): 22.52%
    Cake (5): 30.74%
    Seasonal Bread (6): 15.88%
