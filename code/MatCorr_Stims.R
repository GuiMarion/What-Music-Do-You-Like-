library(tidyverse)
library(corrplot)

# Charger les données
df <- read.csv("full_questionnaire_data.csv")

# Identifier les colonnes des stimuli
stim_vars <- paste0("stim_", 1:80)

# Nettoyer : enlever les lignes incomplètes
df_clean <- df %>% filter(rowSums(is.na(select(., all_of(stim_vars)))) == 0)

# Extraire la matrice des stimuli
stim_matrix <- df_clean[, stim_vars]

# Calculer la matrice de corrélation
stim_cor <- cor(stim_matrix, use = "complete.obs")

# Renommer les lignes/colonnes avec des numéros
colnames(stim_cor) <- as.character(1:80)
rownames(stim_cor) <- as.character(1:80)

# Afficher la matrice de corrélation
corrplot(stim_cor,
         method = "color",
         type = "full",
         tl.cex = 0.5,
         col = colorRampPalette(c("blue", "white", "red"))(200),
         title = "Stimuli Correlation Matrix",
         mar = c(1,1,3,1))
