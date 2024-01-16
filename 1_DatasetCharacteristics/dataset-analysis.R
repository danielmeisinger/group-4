# Need to execute data-preparation.R from 0_DataPreparation to get variable 'filtered_data'

source("0_DataPreparation/data_preparation.R")

# Filter out rows with missing values in REVENUE
filtered_data <- filtered_data %>%
  filter(!is.na(REVENUE))

# Calculate mean revenue for each group
mean_revenue <- filtered_data %>%
  group_by(NEW_YEARS_EVE, PRODUCT_GROUP) %>%
  summarize(mean_revenue = mean(REVENUE, na.rm = TRUE))

# Define custom colors for PRODUCT_GROUP and NEW_YEARS_EVE
product_group_colors <- c("1" = "#BC5215", "2" = "#66800B", "3" = "#205EA6", "4" = "#AD8301", "5" = "#5E409D", "6" = "#A02F6F")

# Define custom labels for NEW_YEARS_EVE
new_years_labels <- c("0" = "Non-New Year's Eve", "1" = "New Year's Eve")
product_group_labels <- c("1" = "Bread", "2" = "Rolls", "3" = "Croissant", "4" = "Confectionery", "5" = "Cake", "6" = "Seasonal Bread")

# Create the ggplot with mean bars
plot_mean_revenue <- ggplot(mean_revenue, aes(x = factor(NEW_YEARS_EVE), y = mean_revenue, fill = factor(PRODUCT_GROUP))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Revenue on New Year's Eve by Product Group",
       x = NULL,
       y = "Mean Revenue") +
  scale_fill_manual(values = product_group_colors, name = "Product Group", labels = product_group_labels) +
  scale_x_discrete(labels = new_years_labels) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#1C1B1A", color = "transparent"),  # Set border color to transparent
    panel.background = element_rect(fill = "#1C1B1A", color = "transparent"),
    text = element_text(color = "#CECDC3"),
    axis.text.x = element_text(color = "#CECDC3", angle = 45, hjust = 1),
    axis.text.y = element_text(color = "#CECDC3"),
    axis.title = element_text(color = "#CECDC3"),
    legend.text = element_text(color = "#CECDC3"),
    legend.title = element_text(color = "#CECDC3"),
    legend.background = element_rect(fill = "#1C1B1A", color = "transparent"), # Set border color to transparent
    plot.title = element_text(hjust = 0.5),
  )

# Display the plot
print(plot_mean_revenue)



# Filter out rows with missing values in REVENUE
filtered_data <- filtered_data %>%
  filter(!is.na(REVENUE))

# Calculate mean revenue for each group
mean_revenue <- filtered_data %>%
  group_by(NEW_YEARS_EVE) %>%
  summarize(mean_revenue = mean(REVENUE, na.rm = TRUE))

# Define custom colors for PRODUCT_GROUP and NEW_YEARS_EVE
# product_group_colors <- c("1" = "#BC5215", "2" = "#66800B", "3" = "#205EA6", "4" = "#AD8301", "5" = "#5E409D", "6" = "#A02F6F")
new_years_colors <- c("0" = "#BC5215", "1" = "#205EA6")

# Define custom labels for NEW_YEARS_EVE
new_years_labels <- c("0" = "Non-New Year's Eve", "1" = "New Year's Eve")
# product_group_labels <- c("1" = "Bread", "2" = "Rolls", "3" = "Croissant", "4" = "Confectionery", "5" = "Cake", "6" = "Seasonal Bread")

# Create the ggplot with mean bars
plot_mean_revenue <- ggplot(mean_revenue, aes(x = factor(NEW_YEARS_EVE), y = mean_revenue, fill = factor(NEW_YEARS_EVE))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Revenue on New Year's Eve by Product Group",
       x = NULL,
       y = "Mean Revenue") +
  scale_fill_manual(values = new_years_colors, name = "New Year's Eve", labels = new_years_labels) +
  scale_x_discrete(labels = new_years_labels) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#1C1B1A", color = "transparent"),  # Set border color to transparent
    panel.background = element_rect(fill = "#1C1B1A", color = "transparent"),
    text = element_text(color = "#CECDC3"),
    axis.text.x = element_text(color = "#CECDC3"),
    axis.text.y = element_text(color = "#CECDC3"),
    axis.title = element_text(color = "#CECDC3"),
    legend.text = element_text(color = "#CECDC3"),
    legend.title = element_text(color = "#CECDC3"),
    legend.background = element_rect(fill = "#1C1B1A", color = "transparent"), # Set border color to transparent
    plot.title = element_text(hjust = 0.5)
  )

# Display the plot
print(plot_mean_revenue)


# Filter out rows with missing values in REVENUE
filtered_data <- filtered_data %>%
  filter(!is.na(REVENUE))

# Calculate mean revenue for each group
mean_revenue <- filtered_data %>%
  group_by(CRUISE_SHIPS) %>%
  summarize(mean_revenue = mean(REVENUE, na.rm = TRUE))

# Define custom colors for PRODUCT_GROUP and NEW_YEARS_EVE
# product_group_colors <- c("1" = "#BC5215", "2" = "#66800B", "3" = "#205EA6", "4" = "#AD8301", "5" = "#5E409D", "6" = "#A02F6F")
cruise_ships_colors <- c("0" = "#66800B", "1" = "#24837B", "2" = "#205EA6", "3" = "#AD8301", "4" = "#BC5215", "5" = "#AF3029")

# Define custom labels for NEW_YEARS_EVE
# new_years_labels <- c("0" = "Non-New Year's Eve", "1" = "New Year's Eve")
# product_group_labels <- c("1" = "Bread", "2" = "Rolls", "3" = "Croissant", "4" = "Confectionery", "5" = "Cake", "6" = "Seasonal Bread")

# Create the ggplot with mean bars
plot_mean_revenue <- ggplot(mean_revenue, aes(x = factor(CRUISE_SHIPS), y = mean_revenue, fill = factor(CRUISE_SHIPS))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Revenue on New Year's Eve by Product Group",
       x = NULL,
       y = "Mean Revenue") +
  scale_fill_manual(values = cruise_ships_colors, name = "Cruise Ships") +
  # scale_x_discrete(labels = new_years_labels) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#1C1B1A", color = "transparent"),  # Set border color to transparent
    panel.background = element_rect(fill = "#1C1B1A", color = "transparent"),
    text = element_text(color = "#CECDC3"),
    axis.text.x = element_text(color = "#CECDC3"),
    axis.text.y = element_text(color = "#CECDC3"),
    axis.title = element_text(color = "#CECDC3"),
    legend.text = element_text(color = "#CECDC3"),
    legend.title = element_text(color = "#CECDC3"),
    legend.background = element_rect(fill = "#1C1B1A", color = "transparent"), # Set border color to transparent
    plot.title = element_text(hjust = 0.5)
  )

# Display the plot
print(plot_mean_revenue)


# Filter out rows with missing values in REVENUE
filtered_data <- filtered_data %>%
  filter(!is.na(REVENUE))

# Calculate mean revenue for each group
mean_revenue <- filtered_data %>%
  group_by(HK_GAME) %>%
  summarize(mean_revenue = mean(REVENUE, na.rm = TRUE))

# Define custom colors for PRODUCT_GROUP and NEW_YEARS_EVE
# product_group_colors <- c("1" = "#BC5215", "2" = "#66800B", "3" = "#205EA6", "4" = "#AD8301", "5" = "#5E409D", "6" = "#A02F6F")
hk_games_colors <- c("0" = "#BC5215", "1" = "#205EA6")

# Define custom labels for NEW_YEARS_EVE
hk_games_labels <- c("0" = "No KSV Game", "1" = "KSV Game")
# product_group_labels <- c("1" = "Bread", "2" = "Rolls", "3" = "Croissant", "4" = "Confectionery", "5" = "Cake", "6" = "Seasonal Bread")

# Create the ggplot with mean bars
plot_mean_revenue <- ggplot(mean_revenue, aes(x = factor(HK_GAME), y = mean_revenue, fill = factor(HK_GAME))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Revenue on New Year's Eve by Product Group",
       x = NULL,
       y = "Mean Revenue") +
  scale_fill_manual(values = hk_games_colors, name = "Holstein Kiel", labels = hk_games_labels) +
  scale_x_discrete(labels = hk_games_labels) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#1C1B1A", color = "transparent"),  # Set border color to transparent
    panel.background = element_rect(fill = "#1C1B1A", color = "transparent"),
    text = element_text(color = "#CECDC3"),
    axis.text.x = element_text(color = "#CECDC3"),
    axis.text.y = element_text(color = "#CECDC3"),
    axis.title = element_text(color = "#CECDC3"),
    legend.text = element_text(color = "#CECDC3"),
    legend.title = element_text(color = "#CECDC3"),
    legend.background = element_rect(fill = "#1C1B1A", color = "transparent"), # Set border color to transparent
    plot.title = element_text(hjust = 0.5)
  )

# Display the plot
print(plot_mean_revenue)







# Filter out rows with missing values in REVENUE
filtered_data <- filtered_data %>%
  filter(!is.na(REVENUE))

# Calculate mean revenue for each group
mean_revenue <- filtered_data %>%
  group_by(HK_GAME, PRODUCT_GROUP) %>%
  summarize(mean_revenue = mean(REVENUE, na.rm = TRUE))

# Define custom colors for PRODUCT_GROUP and NEW_YEARS_EVE
product_group_colors <- c("1" = "#BC5215", "2" = "#66800B", "3" = "#205EA6", "4" = "#AD8301", "5" = "#5E409D", "6" = "#A02F6F")

# Define custom labels for NEW_YEARS_EVE
new_years_labels <- c("0" = "Non-New Year's Eve", "1" = "New Year's Eve")
product_group_labels <- c("1" = "Bread", "2" = "Rolls", "3" = "Croissant", "4" = "Confectionery", "5" = "Cake", "6" = "Seasonal Bread")

# Create the ggplot with mean bars
plot_mean_revenue <- ggplot(mean_revenue, aes(x = factor(HK_GAME), y = mean_revenue, fill = factor(PRODUCT_GROUP))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Revenue on New Year's Eve by Product Group",
       x = NULL,
       y = "Mean Revenue") +
  scale_fill_manual(values = product_group_colors, name = "Product Group", labels = product_group_labels) +
  scale_x_discrete(labels = new_years_labels) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#1C1B1A", color = "transparent"),  # Set border color to transparent
    panel.background = element_rect(fill = "#1C1B1A", color = "transparent"),
    text = element_text(color = "#CECDC3"),
    axis.text.x = element_text(color = "#CECDC3"),
    axis.text.y = element_text(color = "#CECDC3"),
    axis.title = element_text(color = "#CECDC3"),
    legend.text = element_text(color = "#CECDC3"),
    legend.title = element_text(color = "#CECDC3"),
    legend.background = element_rect(fill = "#1C1B1A", color = "transparent"), # Set border color to transparent
    plot.title = element_text(hjust = 0.5)
  )

# Display the plot
print(plot_mean_revenue)


filtered_data <- filtered_data %>%
  filter(PRODUCT_GROUP == 2)

# Scatter plot
plot_mean_revenue <- ggplot(filtered_data, aes(x = TEMPERATURE, y = REVENUE)) +
  geom_point() +
  labs(title = "Scatter Plot of Retail Prices vs. Revenue",
       x = "Retail Prices",
       y = "Revenue") +
  theme_minimal()

# Display the plot
print(plot_mean_revenue)






# Filter out rows with missing values in REVENUE
filtered_data <- filtered_data %>%
  filter(!is.na(REVENUE))

# Calculate mean revenue for each group
mean_revenue <- filtered_data %>%
  group_by(WEATHER_CATEGORY) %>%
  summarize(mean_revenue = mean(REVENUE, na.rm = TRUE))

# Define custom colors for PRODUCT_GROUP and NEW_YEARS_EVE
# product_group_colors <- c("1" = "#BC5215", "2" = "#66800B", "3" = "#205EA6", "4" = "#AD8301", "5" = "#5E409D", "6" = "#A02F6F")
weather_colors <- c("GOOD_WEATHER" = "#BC5215", "THUNDERSTORM" = "#205EA6")

# Define custom labels for NEW_YEARS_EVE
weather_labels <- c("GOOD_WEATHER" = "No Thunderstorm", "THUNDERSTORM" = "Thunderstorm")
# product_group_labels <- c("1" = "Bread", "2" = "Rolls", "3" = "Croissant", "4" = "Confectionery", "5" = "Cake", "6" = "Seasonal Bread")

# Create the ggplot with mean bars
plot_mean_revenue <- ggplot(mean_revenue, aes(x = factor(WEATHER_CATEGORY), y = mean_revenue, fill = factor(WEATHER_CATEGORY))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Revenue on Thunderstorm by",
       x = NULL,
       y = "Mean Revenue") +
  scale_fill_manual(values = weather_colors, name = "Weather", labels = weather_labels) +
  scale_x_discrete(labels = weather_labels) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#1C1B1A", color = "transparent"),  # Set border color to transparent
    panel.background = element_rect(fill = "#1C1B1A", color = "transparent"),
    text = element_text(color = "#CECDC3"),
    axis.text.x = element_text(color = "#CECDC3"),
    axis.text.y = element_text(color = "#CECDC3"),
    axis.title = element_text(color = "#CECDC3"),
    legend.text = element_text(color = "#CECDC3"),
    legend.title = element_text(color = "#CECDC3"),
    legend.background = element_rect(fill = "#1C1B1A", color = "transparent"), # Set border color to transparent
    plot.title = element_text(hjust = 0.5)
  )

# Display the plot
print(plot_mean_revenue)




# Filter out rows with missing values in REVENUE
filtered_data <- filtered_data %>%
  filter(!is.na(REVENUE))

# Calculate mean revenue for each group
mean_revenue <- filtered_data %>%
  group_by(UNEMPLOYMENT_CATEGORY) %>%
  summarize(mean_revenue = mean(REVENUE, na.rm = TRUE))

# Define custom colors for PRODUCT_GROUP and NEW_YEARS_EVE
# product_group_colors <- c("1" = "#BC5215", "2" = "#66800B", "3" = "#205EA6", "4" = "#AD8301", "5" = "#5E409D", "6" = "#A02F6F")
weather_colors <- c("LOW" = "#BC5215", "HIGH" = "#205EA6")

# Define custom labels for NEW_YEARS_EVE
weather_labels <- c("LOW" = "No Thunderstorm", "HIGH" = "Thunderstorm")
# product_group_labels <- c("1" = "Bread", "2" = "Rolls", "3" = "Croissant", "4" = "Confectionery", "5" = "Cake", "6" = "Seasonal Bread")

# Create the ggplot with mean bars
plot_mean_revenue <- ggplot(mean_revenue, aes(x = factor(UNEMPLOYMENT_CATEGORY), y = mean_revenue, fill = factor(UNEMPLOYMENT_CATEGORY))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Revenue on Thunderstorm by",
       x = NULL,
       y = "Mean Revenue") +
  scale_fill_manual(values = weather_colors, name = "Weather", labels = weather_labels) +
  scale_x_discrete(labels = weather_labels) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#1C1B1A", color = "transparent"),  # Set border color to transparent
    panel.background = element_rect(fill = "#1C1B1A", color = "transparent"),
    text = element_text(color = "#CECDC3"),
    axis.text.x = element_text(color = "#CECDC3"),
    axis.text.y = element_text(color = "#CECDC3"),
    axis.title = element_text(color = "#CECDC3"),
    legend.text = element_text(color = "#CECDC3"),
    legend.title = element_text(color = "#CECDC3"),
    legend.background = element_rect(fill = "#1C1B1A", color = "transparent"), # Set border color to transparent
    plot.title = element_text(hjust = 0.5)
  )

# Display the plot
print(plot_mean_revenue)




# Filter out rows with missing values in REVENUE
filtered_data <- filtered_data %>%
  filter(!is.na(REVENUE))

# Calculate mean revenue for each group
mean_revenue <- filtered_data %>%
  group_by(SEASON) %>%
  summarize(mean_revenue = mean(REVENUE, na.rm = TRUE))

season_order <- c("WINTER", "SPRING", "SUMMER", "AUTUMN")

# Define custom colors for PRODUCT_GROUP and NEW_YEARS_EVE
# product_group_colors <- c("1" = "#BC5215", "2" = "#66800B", "3" = "#205EA6", "4" = "#AD8301", "5" = "#5E409D", "6" = "#A02F6F")
season_colors <- c("SPRING" = "#66800B", "WINTER" = "#205EA6", "AUTUMN" = "#AD8301", "SUMMER" = "#BC5215")

# Define custom labels for NEW_YEARS_EVE
season_labels <- c("SPRING" = "Spring", "AUTUMN" = "Autumn", "WINTER" = "Winter", "SUMMER" = "Summer")
#_labels <- c("SPRING" = "Spring", "AUTUMN" = "Autumn", "WINTER" = "Winter", "SUMMER" = "Summer")


# Create the ggplot with mean bars
plot_mean_revenue <- ggplot(mean_revenue, aes(x = factor(SEASON), y = mean_revenue, fill = factor(SEASON))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Revenue by Season",
       x = NULL,
       y = "Mean Revenue") +
  scale_fill_manual(values = season_colors, name = "Season", labels = season_labels) +
  scale_x_discrete(labels = season_labels, limits = season_order) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#1C1B1A", color = "transparent"),  # Set border color to transparent
    panel.background = element_rect(fill = "#1C1B1A", color = "transparent"),
    text = element_text(color = "#CECDC3"),
    axis.text.x = element_text(color = "#CECDC3"),
    axis.text.y = element_text(color = "#CECDC3"),
    axis.title = element_text(color = "#CECDC3"),
    legend.text = element_text(color = "#CECDC3"),
    legend.title = element_text(color = "#CECDC3"),
    legend.background = element_rect(fill = "#1C1B1A", color = "transparent"), # Set border color to transparent
    plot.title = element_text(hjust = 0.5)
  )

# Display the plot
print(plot_mean_revenue)
