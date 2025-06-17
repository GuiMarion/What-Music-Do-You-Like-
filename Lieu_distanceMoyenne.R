library(tidyverse)

# Charger les données
df <- read.csv("full_questionnaire_data.csv")

# Colonnes des genres
genre_vars <- paste0("genre_", 1:19)

# Supprimer les lignes avec tous les genres manquants
df <- df %>% filter(rowSums(is.na(select(., all_of(genre_vars)))) < length(genre_vars))

# Calculer la moyenne globale pour chaque genre
global_means <- df %>% select(all_of(genre_vars)) %>% summarise(across(everything(), ~mean(., na.rm = TRUE)))

# Calculer la distance L2 pour chaque participant
df$L2_distance <- apply(df[, genre_vars], 1, function(x) {
  sqrt(sum((x - global_means)^2, na.rm = TRUE))
})

# Plot des distances par lieu d'écoute
ggplot(df, aes(x = MusicPlaces, y = L2_distance)) +
  geom_boxplot() +
  theme_minimal(base_size = 14) +
  labs(x = "Lieu d'écoute de musique", y = "Distance L2 à la moyenne",
       title = "Distance au profil moyen de goûts musicaux selon le lieu d'écoute") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
