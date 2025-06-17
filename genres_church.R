library(tidyverse)
library(ggpubr)

# Charger les données
df <- read.csv("full_questionnaire_data.csv")

# Extraire les colonnes des genres
genre_vars <- grep("^genre_", names(df), value = TRUE)

# Traduction des labels des genres
genre_labels <- c(
  "Classical music", "Opera", "Jazz", "Blues", "Reggae", "Funk", "Pop", "Gospel", "Rock", "Soul",
  "Metal", "Electronic music", "Hip-hop/Rap", "Indie/Alternative rock",
  "Dance music", "Latin", "Country", "Traditional Italian music", "World music"
)

# Garder seulement les lignes valides
df <- df %>% filter(rowSums(is.na(select(., all_of(genre_vars)))) < length(genre_vars))

# Créer une colonne "Church_vs_Other"
df <- df %>%
  mutate(MusicGroup = ifelse(MusicPlaces == "Chiesa", "Church", "Other"))

# Calculer les moyennes par groupe pour chaque genre
means_by_group <- df %>%
  group_by(MusicGroup) %>%
  summarise(across(all_of(genre_vars), mean, na.rm = TRUE), .groups = "drop")

# Calculer la différence Church - Other
diff_means <- as.numeric(means_by_group[means_by_group$MusicGroup == "Church", -1] -
                           means_by_group[means_by_group$MusicGroup == "Other", -1])

# Créer un data.frame long pour plotting
diff_df <- data.frame(
  Genre = genre_labels,
  Difference = diff_means
)

# Plot sans "Genre" affiché à gauche
ggplot(diff_df, aes(x = reorder(Genre, Difference), y = Difference)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(x = NULL, y = "Church – Other (Mean Rating Difference)",
       title = "Genres More Appreciated in Church Listening Context") +
  theme(axis.title.y = element_blank())
