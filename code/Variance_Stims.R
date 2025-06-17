library(tidyverse)

# Charger les données
df <- read.csv("full_questionnaire_data.csv")

# Liste des variables dépendantes : les stims
stim_vars <- paste0("stim_", 1:80)

# Fixe les trois premiers prédicteurs
fixed_predictors <- c("Age", "Gender", "CatSocioParents")
other_predictors <- c("City", "Incomes", "MusicPlaces")

# Nombre d'itérations
n_iter <- 100

# Fonction pour calculer les adjusted R² exclusifs dans un ordre donné
compute_adj_r2_decomp <- function(df, stim, predictors_order) {
  r2_values <- numeric(length(predictors_order))
  formula_base <- paste0(stim, " ~ ")
  
  for (i in seq_along(predictors_order)) {
    current_predictors <- predictors_order[1:i]
    formula <- as.formula(paste0(formula_base, paste(current_predictors, collapse = " + ")))
    model <- lm(formula, data = df)
    r2_values[i] <- summary(model)$adj.r.squared
  }
  
  exclusive_r2 <- c(r2_values[1], diff(r2_values))
  exclusive_r2 <- pmax(exclusive_r2, 0)  # Clip négatifs à zéro
  names(exclusive_r2) <- predictors_order
  return(exclusive_r2)
}

# Répéter pour chaque stim et chaque permutation
all_results <- list()
for (iter in 1:n_iter) {
  permuted_rest <- sample(other_predictors)
  predictors_perm <- c(fixed_predictors, permuted_rest)
  
  for (stim in stim_vars) {
    r2_explained <- compute_adj_r2_decomp(df, stim, predictors_perm)
    temp_df <- data.frame(Predictor = names(r2_explained),
                          R2 = r2_explained,
                          Stim = stim,
                          Iter = iter)
    all_results[[length(all_results) + 1]] <- temp_df
    
    # R² total du modèle complet
    full_model <- lm(as.formula(paste0(stim, " ~ ", paste(predictors_perm, collapse = " + "))), data = df)
    full_r2 <- summary(full_model)$adj.r.squared
    all_results[[length(all_results) + 1]] <- data.frame(Predictor = "TOTAL",
                                                         R2 = full_r2,
                                                         Stim = stim,
                                                         Iter = iter)
  }
}

# Combiner et résumer
results_df <- bind_rows(all_results)

summary_df <- results_df %>%
  group_by(Predictor) %>%
  summarise(mean_R2 = mean(R2, na.rm = TRUE)) %>%
  arrange(desc(mean_R2))

# Afficher le R² total moyen (toutes variables combinées)
mean_total_r2 <- summary_df %>% filter(Predictor == "TOTAL") %>% pull(mean_R2)
cat("Variance moyenne totale expliquée (R² ajusté moyen complet):", round(mean_total_r2, 3), "\n")

# Afficher la somme des R² exclusifs moyens par prédicteur
explained_variance_sum <- summary_df %>%
  filter(Predictor != "TOTAL") %>%
  summarise(total = sum(mean_R2)) %>%
  pull(total)

cat("Somme des variances moyennes expliquées par les prédicteurs (R² exclusifs):", round(explained_variance_sum, 3), "\n")

# Plot
ggplot(summary_df, aes(x = reorder(Predictor, -mean_R2), y = mean_R2, fill = Predictor)) +
  geom_col(show.legend = FALSE) +
  theme_minimal(base_size = 14) +
  labs(x = "Predictor", y = "Mean Adjusted R² (clipped at 0)",
       title = "Average Exclusive Adjusted R² per Predictor (across stims)") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))
