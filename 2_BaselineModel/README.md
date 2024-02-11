# Baseline Model

In this section we establish a simple baseline for the results of the more complex neural net model.

For the baseline and the neural net model the dataset is split into a training set (all data from 2013-07-01 until 2017-07-31), a validation set (all data from 2017-08-01 until 2018-07-31) and a test set (all data from 2018-08-01 until 2019-07-30). Then, a weighted ols model is fitted to the data and some metrics on the performance of this baseline model are datermined. Eventually, the results of the baseline model are formatted and saved into a csv file for the upload to kaggle.

For baseline model we fitted an weighted ols model with simple and interaction effects (up to the order of 3). In particular, the observations on the dates of new years eve, the day before new years, the Kiel Week, more than 2 cruise ships and the occurence of a thunderstorm are weighted by 30, 25, 4, 2 and 2 respectively. The resulting model is significant (p \< 2.2e-16) and has an explained variation (adjusted R squared) of 0.9693. In terms of prediction accuracy the baseline model has a mape of 0.1676 on the training dataset and a mape of 0.1807 on the validation set. Thus, in total the baseline model performs better on the validation set than on the training set.

The prediction accuracy (measured in mape) of the baseline model in each product category is

-   **Bread** (1): 18.7%
-   **Rolls** (2): 10.4%
-   **Croissant** (3): 18.8%
-   **Confectionery** (4): 26.5%
-   **Cake** (5): 13.4%
-   **Seasonal Bread** (6): 34.6%
