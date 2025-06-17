# Recharger les données sans remplacer les NaN
df = pd.read_csv("/mnt/data/full_questionnaire_data.csv")

# Séparer genres et stims
genre_cols = [col for col in df.columns if col.startswith("genre_")]
stim_cols = [col for col in df.columns if col.startswith("stim_")]

X_stims = df[stim_cols].values
Y_genres = df[genre_cols].values

# Supprimer les lignes contenant des NaN dans les stims ou les genres
valid_rows = ~np.isnan(X_stims).any(axis=1) & ~np.isnan(Y_genres).any(axis=1)
X_stims_clean = X_stims[valid_rows]
Y_genres_clean = Y_genres[valid_rows]

selected_stims = []
remaining_stims = list(range(X_stims_clean.shape[1]))
r2_by_subset = []

# Sélection incrémentale avec R² multi-output (sans NaN)
for _ in range(len(remaining_stims)):
    best_r2 = -np.inf
    best_idx = None

    for idx in remaining_stims:
        candidate = selected_stims + [idx]
        X_candidate = X_stims_clean[:, candidate]
        model = LinearRegression().fit(X_candidate, Y_genres_clean)
        y_pred = model.predict(X_candidate)
        r2 = r2_score(Y_genres_clean, y_pred, multioutput='uniform_average')
        if r2 > best_r2:
            best_r2 = r2
            best_idx = idx

    if best_idx is not None:
        selected_stims.append(best_idx)
        remaining_stims.remove(best_idx)
        r2_by_subset.append(best_r2)

    if best_r2 >= 0.95:
        break

# Plot the result again
plt.figure(figsize=(10, 6))
plt.plot(range(1, len(r2_by_subset) + 1), r2_by_subset, marker='o')
plt.axhline(y=0.95, color='r', linestyle='--', label='Seuil 95%')
plt.xlabel("Nombre de stims sélectionnés")
plt.ylabel("Variance expliquée des genres (R² global)")
plt.title("Stims sélectionnés pour prédire les genres (avec NaN conservés)")
plt.grid(True)
plt.legend()
plt.tight_layout()
plt.show()

