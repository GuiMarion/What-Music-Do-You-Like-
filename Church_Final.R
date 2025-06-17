library(tidyverse)
library(ggpubr)

# Charger les données
df <- read.csv("full_questionnaire_data.csv")

# Identifiants des colonnes
genre_vars <- paste0("genre_", 1:19)
stim_vars <- paste0("stim_", 1:80)
stim_genre_labels <- c(7, 6, 19, 7, 17, 3, 19, 12, 2, 12, 18, 19, 15, 8, 19, 19,
                       7, 9, 12, 13, 17, 19, 10, 19, 12, 14, 14, 19, 1, 4, 18, 3,
                       7, 3, 19, 19, 11, 7, 12, 18, 19, 16, 3, 5, 7, 13, 19, 19,
                       4, 18, 12, 7, 18, 9, 9, 18, 12, 16, 17, 7, 8, 3, 7, 11,
                       14, 1, 12, 15, 16, 12, 17, 15, 3, 9, 3, 5, 19, 19, 11, 7)

# Traduction des labels des genres
genre_labels <- c(
  "Classical music", "Opera", "Jazz", "Blues", "Reggae", "Funk", "Pop", "Gospel", "Rock", "Soul",
  "Metal", "Electronic music", "Hip-hop/Rap", "Indie/Alternative rock",
  "Dance music", "Latin", "Country", "Traditional Italian music", "World music"
)

# Nettoyage des données
df <- df %>% filter(rowSums(is.na(select(., all_of(c(genre_vars, stim_vars))))) == 0)

# Ajouter groupe église/autre
df <- df %>% mutate(MusicGroup = ifelse(MusicPlaces == "Chiesa", "Church", "Other"))

# --- Différences selon genre explicite ---
means_genre <- df %>%
  group_by(MusicGroup) %>%
  summarise(across(all_of(genre_vars), mean, na.rm = TRUE), .groups = "drop")

diff_genre <- as.numeric(means_genre[means_genre$MusicGroup == "Church", -1] -
                           means_genre[means_genre$MusicGroup == "Other", -1])

# --- Différences selon moyenne des stims associés ---
genre_from_stims <- matrix(NA, nrow = nrow(df), ncol = 19)
for (g in 1:19) {
  stim_indices <- which(stim_genre_labels == g)
  if (length(stim_indices) > 1) {
    genre_from_stims[, g] <- rowMeans(df[, stim_vars[stim_indices]], na.rm = TRUE)
  } else {
    genre_from_stims[, g] <- df[, stim_vars[stim_indices]]
  }
}
colnames(genre_from_stims) <- paste0("Gstim_", 1:19)
df <- bind_cols(df, as.data.frame(genre_from_stims))

means_stim <- df %>%
  group_by(MusicGroup) %>%
  summarise(across(starts_with("Gstim_"), mean, na.rm = TRUE), .groups = "drop")

diff_stim <- as.numeric(means_stim[means_stim$MusicGroup == "Church", -1] -
                          means_stim[means_stim$MusicGroup == "Other", -1])

# --- Combiner les deux sources ---
diff_df <- data.frame(
  Genre = genre_labels,
  Diff_Genre = diff_genre,
  Diff_Stim = diff_stim
)

# Réordonner selon la moyenne
diff_df <- diff_df %>%
  mutate(Genre = factor(Genre, levels = Genre[order(Diff_Stim)]))

# --- Plot combiné ---
ggplot(diff_df, aes(x = Genre)) +
  geom_col(aes(y = Diff_Genre, fill = "Questionnaire"), alpha = 0.5) +
  geom_col(aes(y = Diff_Stim, fill = "Stimuli"), alpha = 0.5) +
  coord_flip() +
  scale_fill_manual(values = c("Questionnaire" = "steelblue", "Stimuli" = "darkred")) +
  labs(x = NULL, y = "Church – Other (Mean Difference)",
       fill = "Source",
       title = "Church vs. Other: Genre Appreciation (Questionnaire vs. Stimuli)") +
  theme_minimal(base_size = 14)
