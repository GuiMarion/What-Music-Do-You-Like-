# Load necessary packages
library(tidyverse)

# Load the data
#data <- read.csv("full_questionnaire_data.csv")

# Select genre and stim columns
genre_cols <- grep("^genre_", names(data), value = TRUE)
stim_cols <- grep("^stim_", names(data), value = TRUE)

# Initialize R² containers
r2_genre_to_stim <- numeric(length(stim_cols))
r2_stim_to_genre <- numeric(length(genre_cols))

# 1. Predict each stim using all genres
for (i in seq_along(stim_cols)) {
  model <- lm(as.formula(paste(stim_cols[i], "~", paste(genre_cols, collapse = "+"))), data = data)
  r2_genre_to_stim[i] <- summary(model)$r.squared
}

# 2. Predict each genre using all stims
for (i in seq_along(genre_cols)) {
  model <- lm(as.formula(paste(genre_cols[i], "~", paste(stim_cols, collapse = "+"))), data = data)
  r2_stim_to_genre[i] <- summary(model)$r.squared
}

# Compute means
summary_df <- tibble(
  Direction = c("Genres → Stims", "Stims → Genres"),
  R2 = c(mean(r2_genre_to_stim), mean(r2_stim_to_genre))
)

# Plot
ggplot(summary_df, aes(x = Direction, y = R2, fill = Direction)) +
  geom_bar(stat = "identity", width = 0.6, show.legend = FALSE) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14) +
  ylab("Mean R²") +
  xlab("") +
  ggtitle("Variance Explained Between Genres and Stims")
