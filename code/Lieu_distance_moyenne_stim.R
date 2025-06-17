library(tidyverse)

# Charger les données
df <- read.csv("full_questionnaire_data.csv")

# Colonnes des stims
stim_vars <- paste0("stim_", 1:80)

# Supprimer les lignes avec tous les stims manquants
df <- df %>% filter(rowSums(is.na(select(., all_of(stim_vars)))) < length(stim_vars))

# Calculer la moyenne globale pour chaque stim
global_means <- df %>% select(all_of(stim_vars)) %>% summarise(across(everything(), ~mean(., na.rm = TRUE)))

# Calculer la distance L2 pour chaque participant
df$L2_distance_stims <- apply(df[, stim_vars], 1, function(x) {
  sqrt(sum((x - global_means)^2, na.rm = TRUE))
})

# Plot des distances par lieu d'écoute
ggplot(df, aes(x = MusicPlaces, y = L2_distance_stims)) +
  geom_boxplot() +
  theme_minimal(base_size = 14) +
  labs(x = "Music Listening Place", y = "L2 Distance from Stimulus Profile Mean",
       title = "Distance from Average Stimulus Preference by Listening Place") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
