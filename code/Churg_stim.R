library(tidyverse)

# Charger les données
df <- read.csv("full_questionnaire_data.csv")

# Identifiants des colonnes
stim_vars <- paste0("stim_", 1:80)
stim_genre_labels <- c(7, 6, 19, 7, 17, 3, 19, 12, 2, 12, 18, 19, 15, 8, 19, 19,
                       7, 9, 12, 13, 17, 19, 10, 19, 12, 14, 14, 19, 1, 4, 18, 3,
                       7, 3, 19, 19, 11, 7, 12, 18, 19, 16, 3, 5, 7, 13, 19, 19,
                       4, 18, 12, 7, 18, 9, 9, 18, 12, 16, 17, 7, 8, 3, 7, 11,
                       14, 1, 12, 15, 16, 12, 17, 15, 3, 9, 3, 5, 19, 19, 11, 7)

genre_labels <- c(
  "Classical music", "Opera", "Jazz", "Blues", "Reggae", "Funk", "Pop", "Gospel", "Rock", "Soul",
  "Metal", "Electronic music", "Hip-hop/Rap", "Indie/Alternative rock",
  "Dance music", "Latin", "Country", "Traditional Italian music", "World music"
)

# Nettoyage : retirer les lignes incomplètes
df <- df %>% filter(rowSums(is.na(select(., all_of(stim_vars)))) == 0)

# Créer une colonne "Church_vs_Other"
df$MusicGroup <- ifelse(df$MusicPlaces == "Chiesa", "Church", "Other")

# Créer une matrice des préférences par genre à partir des stims
genre_from_stims <- matrix(NA, nrow = nrow(df), ncol = 19)
for (g in 1:19) {
  stim_indices <- which(stim_genre_labels == g)
  if (length(stim_indices) > 1) {
    genre_from_stims[, g] <- rowMeans(df[, stim_vars[stim_indices]], na.rm = TRUE)
  } else {
    genre_from_stims[, g] <- df[, stim_vars[stim_indices]]
  }
}

# Ajouter à df
colnames(genre_from_stims) <- paste0("Gstim_", 1:19)
df <- bind_cols(df, as.data.frame(genre_from_stims))

# Calculer les moyennes Church vs Other
means_by_group <- df %>%
  group_by(MusicGroup) %>%
  summarise(across(starts_with("Gstim_"), mean, na.rm = TRUE), .groups = "drop")

# Différences
diff_means <- as.numeric(means_by_group[means_by_group$MusicGroup == "Church", -1] -
                           means_by_group[means_by_group$MusicGroup == "Other", -1])

# Dataframe pour plot
diff_df <- data.frame(
  Genre = genre_labels,
  Difference = diff_means
)

# Plot
ggplot(diff_df, aes(x = reorder(Genre, Difference), y = Difference)) +
  geom_col(fill = "darkred") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(x = NULL, y = "Church – Other (Mean Stim-Based Rating Difference)",
       title = "Genres More Appreciated in Church Based on Stimuli") +
  theme(axis.title.y = element_blank())
