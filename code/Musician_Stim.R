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

# Nettoyer la variable Musician
df$Musician <- as.factor(df$Musician)

# Plot avec test statistique
ggplot(df, aes(x = Musician, y = L2_distance)) +
  geom_boxplot(fill = "#FFD166", alpha = 0.7) +
  stat_compare_means(method = ifelse(nlevels(df$Musician) == 2, "wilcox.test", "kruskal.test"),
                     label = "p.format") +
  theme_minimal(base_size = 14) +
  labs(x = "Musician", y = "L2 Distance from Global Stimulus Profile",
       title = "Stimulus Preference Deviation by Musical Practice")
