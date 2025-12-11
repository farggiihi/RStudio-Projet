# -------------------------------
# 1) Identification variables
# -------------------------------
# Variables qualitatives : Code_departement, Code_region, Taille_Departement


# -------------------------------
# 2) Fréquence des régions
# -------------------------------

tab <- table(df$Code_region)

barplot(
  tab,
  main = "Fréquence des régions",
  xlab = "Code INSEE des régions (effectif)",
  ylab = "Effectif",
  names.arg = names(tab),
  las = 2
)


# -------------------------------
# 3) Répartition des départements par taille
# -------------------------------

# 1. Tableau des effectifs
tab_taille <- table(df$Taille_Departement)

# 2. Afficher les effectifs
print(tab_taille)

# 3. Pourcentages
pourcentages <- prop.table(tab_taille) * 100
print(round(pourcentages, 1))


# -------------------------------
# Barplot de la taille des départements
# -------------------------------

barplot(
  tab_taille,
  main = "Répartition des Départements par Taille",
  xlab = "Catégorie de taille",
  ylab = "Nombre de départements",
  col = "steelblue",
  border = NA,
  las = 1
)


# -------------------------------
# Diagramme circulaire (Pie chart)
# -------------------------------

labels_pie <- paste(names(tab_taille), "\n", round(pourcentages, 1), "%")

pie(
  tab_taille,
  labels = labels_pie,
  main = "Répartition par Taille de Département",
  col = c("#66c2a5", "#fc8d62", "#8da0cb")
)
