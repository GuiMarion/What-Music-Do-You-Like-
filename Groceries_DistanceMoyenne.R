library(tidyverse)

# Load the data
df <- read.csv("full_questionnaire_data.csv")

# List of genre columns
genre_vars <- paste0("genre_", 1:19)

# Remove rows with too many missing genre values
df <- df %>% filter(rowSums(is.na(select(., all_of(genre_vars)))) < length(genre_vars))

# Convert numeric Groceries to labeled factor
df$GroceriesLabel <- factor(df$Groceries,
                            levels = 1:6,
                            labels = c("≤10 min", "10–20 min", "20–30 min",
                                       "30–45 min", "45–60 min", "≥60 min"))

# Compute global mean across genres
genre_mean <- colMeans(df[, genre_vars], na.rm = TRUE)

# Compute L2 distance per participant
df$L2_distance <- apply(df[, genre_vars], 1, function(x) {
  sqrt(sum((x - genre_mean)^2, na.rm = TRUE))
})

# Plot
ggplot(df, aes(x = GroceriesLabel, y = L2_distance, fill = GroceriesLabel)) +
  geom_boxplot(show.legend = FALSE, outlier.alpha = 0.2) +
  theme_minimal(base_size = 14) +
  labs(x = "Time Spent Grocery Shopping (per week)",
       y = "L2 Distance from Average Genre Preferences",
       title = "Variation in Music Preferences by Grocery Shopping Time") +
  scale_x_discrete(limits = c("≤10 min", "10–20 min", "20–30 min",
                              "30–45 min", "45–60 min", "≥60 min"))
