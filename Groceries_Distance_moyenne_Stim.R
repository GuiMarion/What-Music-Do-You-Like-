library(tidyverse)

# Load the data
df <- read.csv("full_questionnaire_data.csv")

# List of stim columns
stim_vars <- grep("^stim_", names(df), value = TRUE)

# Remove rows with too many missing stim values
df <- df %>% filter(rowSums(is.na(select(., all_of(stim_vars)))) < length(stim_vars))

# Convert numeric Groceries to labeled factor
df$GroceriesLabel <- factor(df$Groceries,
                            levels = 1:6,
                            labels = c("≤10 min", "10–20 min", "20–30 min",
                                       "30–45 min", "45–60 min", "≥60 min"))

# Compute global mean across stims
stim_mean <- colMeans(df[, stim_vars], na.rm = TRUE)

# Compute L2 distance per participant
df$L2_distance_stim <- apply(df[, stim_vars], 1, function(x) {
  sqrt(sum((x - stim_mean)^2, na.rm = TRUE))
})

# Plot
ggplot(df, aes(x = GroceriesLabel, y = L2_distance_stim, fill = GroceriesLabel)) +
  geom_boxplot(show.legend = FALSE, outlier.alpha = 0.2) +
  theme_minimal(base_size = 14) +
  labs(x = "Time Spent Grocery Shopping (per week)",
       y = "L2 Distance from Average Stim Preferences",
       title = "Variation in Stim Preferences by Grocery Shopping Time") +
  scale_x_discrete(limits = c("≤10 min", "10–20 min", "20–30 min",
                              "30–45 min", "45–60 min", "≥60 min"))
