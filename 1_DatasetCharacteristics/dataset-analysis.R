# Need to execute data-preparation.R from 0_DataPreparation to get variable 'filtered_data'

source("0_DataPreparation/data_preparation.R")

# Filter data for holidays and non-holidays
holiday <- filtered_data %>% filter(HOLIDAY == TRUE)
non_holiday <- filtered_data %>% filter(HOLIDAY == FALSE)

# Calculate confidence intervals (adjust alpha as needed)
holiday_ci <- t.test(holiday$REVENUE)$conf.int
non_holiday_ci <- t.test(non_holiday$REVENUE)$conf.int

# Calculates mean, sd, se and IC
new_years_analysis <- filtered_data %>%
  group_by(NEW_YEARS_EVE, PRODUCT_GROUP) %>%
  summarise(
    n = n(),
    mean = mean(REVENUE),
    sd = sd(REVENUE)
  ) %>%
  mutate(se = sd / sqrt(n)) %>%
  mutate(ic = ifelse(n > 1, se * qt((1 - 0.05) / 2 + 0.5, n - 1), NA))

# Make a bar plots of sales_data divided by weekday, separated bars for each Warengruppe
ggplot(new_years_analysis, aes(x = factor(NEW_YEARS_EVE), y = mean)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~PRODUCT_GROUP, ncol = 3) +
  labs(title = "Mean Revenue Comparison on New Year's Eve",
       x = "New Year's Eve",
       y = "Mean Revenue") +
  theme_minimal()

# Calculates mean, sd, se and IC
month_analysis <- filtered_data %>%
  group_by(MONTH, PRODUCT_GROUP) %>%
  summarise(
    n = n(),
    mean = mean(REVENUE),
    sd = sd(REVENUE)
  ) %>%
  mutate(se = sd / sqrt(n)) %>%
  mutate(ic = se * qt((1 - 0.05) / 2 + 0.5, n - 1))

# Make a bar plots of sales_data divided by weekday, separated bars for each Warengruppe
ggplot(month_analysis, aes(x = factor(MONTH), y = mean)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~PRODUCT_GROUP, ncol = 3) +
  labs(title = "Mean Revenue Comparison on New Year's Eve",
       x = "New Year's Eve",
       y = "Mean Revenue") +
  theme_minimal()

# Calculates mean, sd, se and IC
weather_analysis <- filtered_data %>%
  group_by(WEATHER_CATEGORY,PRODUCT_GROUP) %>%
  summarise(
    n = n(),
    mean = mean(REVENUE),
    sd = sd(REVENUE)
  ) %>%
  mutate(se = sd / sqrt(n)) %>%
  mutate(ic = se * qt((1 - 0.05) / 2 + 0.5, n - 1))

# Make a bar plots of sales_data divided by weekday, separated bars for each Warengruppe
ggplot(weather_analysis, aes(x = factor(WEATHER_CATEGORY), y = mean)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~PRODUCT_GROUP, ncol = 3) +
  labs(title = "Mean Revenue Comparison on New Year's Eve",
       x = "New Year's Eve",
       y = "Mean Revenue") +
  theme_minimal()


