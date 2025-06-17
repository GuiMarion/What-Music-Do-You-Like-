library(tidyverse)
library(ggpubr)

# Charger les données
df <- read.csv("full_questionnaire_data.csv")

# Sélectionner les colonnes des genres
genre_vars <- grep("^genre_", names(df), value = TRUE)

# Supprimer les lignes avec tous les genres manquants
df <- df %>%
  filter(rowSums(is.na(select(., all_of(genre_vars)))) < length(genre_vars))

# Calculer la moyenne globale pour chaque genre
global_means <- df %>%
  select(all_of(genre_vars)) %>%
  summarise(across(everything(), ~mean(., na.rm = TRUE)))

# Calculer la distance L2 pour chaque participant
df$L2_distance <- apply(df[, genre_vars], 1, function(x) {
  sqrt(sum((x - global_means)^2, na.rm = TRUE))
})

# S'assurer que la variable est factor et trier les niveaux
df$ListeningPerWeek <- factor(df$ListeningPerWeek, levels = sort(unique(df$ListeningPerWeek)))

# Garder uniquement le premier et le dernier bin
extremes_df <- df %>% filter(ListeningPerWeek %in% c(levels(ListeningPerWeek)[1], levels(ListeningPerWeek)[length(levels(ListeningPerWeek))]))

# Comparaison statistique entre les deux extrêmes
ggplot(extremes_df, aes(x = ListeningPerWeek, y = L2_distance)) +
  geom_boxplot(fill = "#69b3a2", alpha = 0.7) +
  stat_compare_means(method = "wilcox.test", label = "p.format") +
  theme_minimal(base_size = 14) +
  labs(x = "Weekly Music Listening Time (lowest vs. highest)",
       y = "L2 Distance from Global Genre Profile",
       title = "Genre Preference Deviation by Listening Time (Extremes Only)")
