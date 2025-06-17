library(tidyverse)
library(ggpubr)
library(patchwork)

# Charger les données
df <- read.csv("full_questionnaire_data.csv")

# Recode Chiesa en "Church" et les autres en "Other"
df$MusicGroup <- ifelse(df$MusicPlaces == "Chiesa", "Church", "Other")

# === GENRE ANALYSE ===
genre_vars <- grep("^genre_", names(df), value = TRUE)
df_genre <- df %>%
  filter(rowSums(is.na(select(., all_of(genre_vars)))) < length(genre_vars))

global_mean_genre <- colMeans(df_genre[, genre_vars], na.rm = TRUE)
df_genre$L2_genre <- apply(df_genre[, genre_vars], 1, function(x) {
  sqrt(sum((x - global_mean_genre)^2, na.rm = TRUE))
})
df_genre$L2_genre_z <- scale(df_genre$L2_genre)

# === STIM ANALYSE ===
stim_vars <- grep("^stim_", names(df), value = TRUE)
df_stim <- df %>%
  filter(rowSums(is.na(select(., all_of(stim_vars)))) < length(stim_vars))

global_mean_stim <- colMeans(df_stim[, stim_vars], na.rm = TRUE)
df_stim$L2_stim <- apply(df_stim[, stim_vars], 1, function(x) {
  sqrt(sum((x - global_mean_stim)^2, na.rm = TRUE))
})
df_stim$L2_stim_z <- scale(df_stim$L2_stim)

# === BOXPLOTS ===
plot_genre <- ggplot(df_genre, aes(x = factor(MusicGroup, levels = c("Church", "Other")), y = L2_genre_z)) +
  geom_boxplot(fill = "#86C5D8", alpha = 0.7) +
  geom_vline(xintercept = 1.5, linetype = "dashed") +
  labs(title = "Genre Distance", x = "Listening Location", y = "Z-scored L2 distance") +
  theme_minimal(base_size = 14)

plot_stim <- ggplot(df_stim, aes(x = factor(MusicGroup, levels = c("Church", "Other")), y = L2_stim_z)) +
  geom_boxplot(fill = "#F4A582", alpha = 0.7) +
  geom_vline(xintercept = 1.5, linetype = "dashed") +
  labs(title = "Stimulus Distance", x = "Listening Location", y = "Z-scored L2 distance") +
  theme_minimal(base_size = 14)

# === DIFFERENCE DISTRIBUTIONS ===
church_genre <- df_genre$L2_genre_z[df_genre$MusicGroup == "Church"]
other_genre <- df_genre$L2_genre_z[df_genre$MusicGroup == "Other"]
church_stim <- df_stim$L2_stim_z[df_stim$MusicGroup == "Church"]
other_stim <- df_stim$L2_stim_z[df_stim$MusicGroup == "Other"]

# Toutes les combinaisons
diffs_genre <- as.vector(outer(church_genre, other_genre, "-"))
diffs_stim <- as.vector(outer(church_stim, other_stim, "-"))

# Créer data frame pour plot
diff_df <- tibble(
  difference = c(diffs_genre, diffs_stim),
  type = rep(c("Genre", "Stim"), times = c(length(diffs_genre), length(diffs_stim)))
)

# Wilcoxon test
wilcox_result <- wilcox.test(diffs_genre, diffs_stim)
p_val <- format.pval(wilcox_result$p.value, digits = 3)

# Plot des distributions
plot_diff <- ggplot(diff_df, aes(x = difference, fill = type)) +
  geom_density(alpha = 0.6) +
  labs(title = paste0("Distribution of Differences (Church - Other)\nWilcoxon p = ", p_val),
       x = "Z-scored Distance Difference", y = "Density") +
  theme_minimal(base_size = 14)

# === COMBINE ===
(plot_genre | plot_stim) / plot_diff
