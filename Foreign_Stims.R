library(tidyverse)
library(ggpubr)  # Pour stat_compare_means()

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

# Calculer la distance L2 pour chaque participant (stims)
df$L2_distance <- apply(df[, stim_vars], 1, function(x) {
  sqrt(sum((x - global_means)^2, na.rm = TRUE))
})

# Nettoyer la variable Foreign
df$Foreign <- as.factor(df$Foreign)

# Plot avec test statistique
ggplot(df, aes(x = Foreign, y = L2_distance)) +
  geom_boxplot(fill = "#FF9999", alpha = 0.7) +
  stat_compare_means(method = ifelse(nlevels(df$Foreign) == 2, "wilcox.test", "kruskal.test"),
                     label = "p.format") +
  theme_minimal(base_size = 14) +
  labs(x = "Parents Foreign", y = "L2 Distance from Global Stimulus Profile",
       title = "Stimulus Preference Deviation by Parental Foreign Background")
  