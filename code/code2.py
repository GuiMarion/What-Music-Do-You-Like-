import pandas as pd
from sklearn.linear_model import LinearRegression
from sklearn.metrics import r2_score
import matplotlib.pyplot as plt

# Load the full data
df = pd.read_csv("/mnt/data/full_questionnaire_data.csv")

# Identify genre and stim columns
genre_cols = [col for col in df.columns if col.startswith("genre_")]
stim_cols = [col for col in df.columns if col.startswith("stim_")]

# Drop rows with missing data in relevant columns
df_clean = df.dropna(subset=genre_cols + stim_cols)

# Regression: genres -> each stim
r2_genres_to_stims = []
X_genres = df_clean[genre_cols].values
for stim in stim_cols:
    y_stim = df_clean[stim].values
    model = LinearRegression().fit(X_genres, y_stim)
    y_pred = model.predict(X_genres)
    r2 = r2_score(y_stim, y_pred)
    r2_genres_to_stims.append(r2)

# Regression: stims -> each genre
r2_stims_to_genres = []
X_stims = df_clean[stim_cols].values
for genre in genre_cols:
    y_genre = df_clean[genre].values
    model = LinearRegression().fit(X_stims, y_genre)
    y_pred = model.predict(X_stims)
    r2 = r2_score(y_genre, y_pred)
    r2_stims_to_genres.append(r2)

# Plotting
plt.figure(figsize=(6, 4))
plt.bar(["Genres → Stims", "Stims → Genres"], 
        [sum(r2_genres_to_stims)/len(r2_genres_to_stims), 
         sum(r2_stims_to_genres)/len(r2_stims_to_genres)])
plt.ylabel("Mean $R^2$")
plt.title("Explained Variance Between Genres and Stimuli")
plt.ylim(0, 1)
plt.grid(axis="y", linestyle="--", alpha=0.6)
plt.tight_layout()
plt.show()

