# Rechargement et recalcul de la matrice de corrélation
import pandas as pd

# Chargement du fichier
file_path = "/mnt/data/full_questionnaire_data.csv"
df = pd.read_csv(file_path)

# Sélection des colonnes des genres musicaux
genre_columns = [col for col in df.columns if col.startswith("genre_")]

# Création de la matrice de corrélation
corr_matrix = df[genre_columns].corr()

# Création de la figure heatmap sans les annotations
import matplotlib.pyplot as plt
import seaborn as sns

plt.figure(figsize=(12, 10))
sns.heatmap(corr_matrix, cmap="coolwarm", square=True, cbar=True,
            xticklabels=corr_matrix.columns,
            yticklabels=corr_matrix.columns,
            annot=False, linewidths=0.5)

plt.title("Corrélation entre les genres musicaux", fontsize=16, pad=20)
plt.xticks(rotation=45, ha='right')
plt.yticks(rotation=0)
plt.tight_layout()
plt.show()

