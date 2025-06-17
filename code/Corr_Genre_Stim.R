library(tidyverse)
library(corrplot)

# Charger les données
df <- read.csv("full_questionnaire_data.csv")

# Identifier les colonnes
genre_vars <- paste0("genre_", 1:19)
stim_vars <- paste0("stim_", 1:80)

# Nettoyer les données
df <- df %>%
  filter(rowSums(is.na(select(., all_of(c(genre_vars, stim_vars))))) == 0)

# Matrices
genre_matrix <- df[, genre_vars]
stim_matrix <- df[, stim_vars]
cor_matrix <- cor(genre_matrix, stim_matrix)

# Genre associé à chaque stim
stim_genre_labels <- c(7, 6, 19, 7, 17, 3, 19, 12, 2, 12, 18, 19, 15, 8, 19, 19,
                       7, 9, 12, 13, 17, 19, 10, 19, 12, 14, 14, 19, 1, 4, 18, 3,
                       7, 3, 19, 19, 11, 7, 12, 18, 19, 16, 3, 5, 7, 13, 19, 19,
                       4, 18, 12, 7, 18, 9, 9, 18, 12, 16, 17, 7, 8, 3, 7, 11,
                       14, 1, 12, 15, 16, 12, 17, 15, 3, 9, 3, 5, 19, 19, 11, 7)

# Réordonner les stims par genre
stim_order_df <- data.frame(stim = stim_vars, genre = stim_genre_labels) %>%
  arrange(genre)

cor_matrix_ordered <- cor_matrix[, stim_order_df$stim]

# Dupliquer les lignes du genre selon le nombre de stims associés
genre_line_ids <- stim_order_df$genre
cor_matrix_expanded <- cor_matrix[genre_line_ids, stim_order_df$stim]

# Renommer les lignes et colonnes
rownames(cor_matrix_expanded) <- paste0("genre_", genre_line_ids)
colnames(cor_matrix_expanded) <- stim_order_df$stim

# Plot
corrplot(cor_matrix_expanded,
         method = "square",
         is.corr = FALSE,
         tl.cex = 0.6,
         tl.col = "black",
         col = colorRampPalette(c("blue", "white", "red"))(200),
         title = "Genre Lines Weighted by Stim Count",
         mar = c(1,1,3,1))

# Initialiser un vecteur pour stocker les corrélations
genre_stim_corr <- c()

# Boucle sur les 19 genres
for (g in 1:19) {
  # Trouver les stims associés à ce genre
  stim_indices <- which(stim_genre_labels == g)
  
  # Extraire les colonnes correspondantes
  stim_data <- df[, stim_vars[stim_indices]]
  
  # S'assurer que stim_data est une matrice (au moins 2D)
  if (length(stim_indices) == 1) {
    stim_avg <- stim_data  # C'est déjà un vecteur
  } else {
    stim_avg <- rowMeans(stim_data)
  }
  
  # Notes de genre pour chaque participant
  genre_scores <- df[[paste0("genre_", g)]]
  
  # Corrélation entre les deux
  r <- cor(stim_avg, genre_scores, use = "complete.obs")
  genre_stim_corr <- c(genre_stim_corr, r)
}

# Résultat sous forme de data.frame
cor_df <- data.frame(
  Genre = paste0("genre_", 1:19),
  Correlation_with_associated_stims = round(genre_stim_corr, 3)
)
library(tidyverse)

# Corrélations entre genres et stims associés
cor_df <- data.frame(
  Genre = c("Classical", "Opera", "Jazz", "Blues", "Reggae", "Funk", "Pop", "Gospel", "Rock", "Soul",
            "Metal", "Electronic", "Hip-hop/Rap", "Indie/Alt Rock", "Dance", "Latin", "Country",
            "Italian Traditional", "World"),
  Correlation = c(0.33, 0.40, 0.61, 0.47, 0.28, 0.30, 0.57, 0.30, 0.43, 0.50,
                  0.52, 0.60, 0.66, 0.55, 0.58, 0.44, 0.41, 0.20, 0.42)  # <-- à ajuster si besoin
)

# Moyenne des corrélations
mean_corr <- mean(cor_df$Correlation, na.rm = TRUE)

# Barplot vertical
ggplot(cor_df, aes(x = reorder(Genre, -Correlation), y = Correlation)) +
  geom_col(fill = "#6495ED") +
  geom_hline(yintercept = mean_corr, linetype = "dashed", color = "red") +
  theme_minimal(base_size = 14) +
  labs(x = NULL,
       y = "Correlation",
       title = "Correlation Between Genre Ratings and Stimuli Preferences",
       subtitle = paste0("Mean correlation = ", round(mean_corr, 2))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

