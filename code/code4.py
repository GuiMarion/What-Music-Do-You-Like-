import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

# Charger les données
df = pd.read_csv("/mnt/data/full_questionnaire_data.csv")

# Sous-ensemble des variables d'intérêt
columns_of_interest = ['Age', 'Gender', 'City', 'Incomes', 'CatSocioParents', 'MusicPlaces']

# Supprimer les lignes avec des NaN dans les colonnes d'intérêt
subset_df = df[columns_of_interest].dropna()

# Encodage des variables catégorielles
encoded_df = pd.get_dummies(subset_df, drop_first=True)

# Matrice de corrélation
corr_matrix = encoded_df.corr()

# Affichage graphique
plt.figure(figsize=(10, 8))
sns.heatmap(corr_matrix, annot=True, cmap='coolwarm', fmt=".2f", square=True, cbar_kws={'shrink': .8})
plt.title("Correlation Matrix of Predictors")
plt.tight_layout()
plt.show()

