library(tidyverse)
library(ggpubr)  # Pour stat_compare_means()

# Charger les données
df <- read.csv("full_questionnaire_data.csv")

# Sélectionner les colonnes de genres
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

# Nettoyer la variable Foreign
df$Foreign <- as.factor(df$Foreign)

# Vérifier le nombre de niveaux
levels(df$Foreign)

# Plot avec test statistique
ggplot(df, aes(x = Foreign, y = L2_distance)) +
  geom_boxplot(fill = "#69b3a2", alpha = 0.7) +
  stat_compare_means(method = ifelse(nlevels(df$Foreign) == 2, "wilcox.test", "kruskal.test"),
                     label = "p.format") +
  theme_minimal(base_size = 14) +
  labs(x = "Parents Foreign", y = "L2 Distance from Global Genre Profile",
       title = "Genre Preference Deviation by Parental Foreign Background")
