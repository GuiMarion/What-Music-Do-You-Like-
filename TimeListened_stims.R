library(tidyverse)
library(ggpubr)

# Charger les données
df <- read.csv("full_questionnaire_data.csv")

# Sélectionner les colonnes des stims
stim_vars <- grep("^stim_", names(df), value = TRUE)

# Supprimer les lignes avec tous les stims manquants
df <- df %>%
  filter(rowSums(is.na(select(., all_of(stim_vars)))) < length(stim_vars))

# Calculer la moyenne globale pour chaque stim
global_means <- df %>%
  select(all_of(stim_vars)) %>%
  summarise(across(everything(), ~mean(., na.rm = TRUE)))

# Calculer la distance L2 pour chaque participant
df$L2_distance <- apply(df[, stim_vars], 1, function(x) {
  sqrt(sum((x - global_means)^2, na.rm = TRUE))
})

# S'assurer que la variable est factor et trier les niveaux
df$ListeningPerWeek <- factor(df$ListeningPerWeek, levels = sort(unique(df$ListeningPerWeek)))

# Garder uniquement le premier et le dernier bin
extremes_df <- df %>% filter(ListeningPerWeek %in% c(levels(ListeningPerWeek)[1], levels(ListeningPerWeek)[length(levels(ListeningPerWeek))]))

# Comparaison statistique entre les deux extrêmes
ggplot(extremes_df, aes(x = ListeningPerWeek, y = L2_distance)) +
  geom_boxplot(fill = "#FF6B6B", alpha = 0.7) +
  stat_compare_means(method = "wilcox.test", label = "p.format") +
  theme_minimal(base_size = 14) +
  labs(x = "Weekly Music Listening Time (lowest vs. highest)",
       y = "L2 Distance from Global Stimulus Profile",
       title = "Stimulus Preference Deviation by Listening Time (Extremes Only)")
