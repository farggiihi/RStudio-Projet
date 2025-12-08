# ==============================================================================
# SCRIPT DE PRÉPARATION DES DONNÉES - PROJET DATA EXPLORATION
# ==============================================================================

# 1. Chargement des librairies nécessaires
# Si tu ne les as pas, installe-les via : install.packages(c("dplyr", "tidyr"))
library(dplyr)
library(tidyr)

# 2. Chargement du fichier brut
# Assure-toi que le fichier est bien dans ton dossier de travail
nom_fichier <- "donnee-dep-data.gouv-2024-geographie2025-produit-le2025-06-04.csv"
df_raw <- read.csv(nom_fichier, sep = ";", stringsAsFactors = FALSE)

# 3. Définition des variables à conserver
vars_selection <- c(
  "Homicides",
  "Violences physiques hors cadre familial",
  "Vols avec armes",
  "Vols de véhicule",
  "Cambriolages de logement",
  "Trafic de stupéfiants"
)

# 4. Traitement des données
df_clean <- df_raw %>%
  # A. Filtrage : On ne garde que 2024 et les indicateurs choisis
  filter(annee == 2024, indicateur %in% vars_selection) %>%
  
  # B. Nettoyage du taux : Remplacer la virgule par un point pour en faire un nombre
  mutate(taux_pour_mille = as.numeric(gsub(",", ".", taux_pour_mille))) %>%
  
  # C. Sélection des colonnes utiles (On garde l'année temporairement pour vérifier)
  select(Code_departement, Code_region, annee, insee_pop, indicateur, taux_pour_mille) %>%
  
  # D. Pivot : Transformation des lignes en colonnes (Format Large)
  pivot_wider(
    names_from = indicateur, 
    values_from = taux_pour_mille,
    values_fill = 0 # Sécurité : met 0 si une donnée manque
  ) %>%
  
  # E. Agrégation : On regroupe par département pour avoir une ligne par dept
  # (Cela permet de récupérer la population unique et les taux pivotés)
  group_by(Code_departement, Code_region, annee) %>%
  summarise(
    Population = mean(insee_pop, na.rm = TRUE),
    across(all_of(vars_selection), sum, na.rm = TRUE),
    .groups = "drop"
  )

# 5. Ajout de la variable qualitative "Taille_Departement"
# Seuils : < 300k (Rural/Petit), 300k-800k (Moyen), > 800k (Grand/Urbain)
df_clean$Taille_Departement <- cut(
  df_clean$Population,
  breaks = c(0, 300000, 800000, 10000000),
  labels = c("Petit (<300k)", "Moyen", "Grand (>800k)"),
  include.lowest = TRUE
)

# 6. Suppression de la colonne année (comme demandé)
df_clean$annee <- NULL

# 7. Vérification finale
print("Aperçu des données finales :")
print(head(df_clean))
print("Structure des données :")
str(df_clean)

# 8. Exportation en CSV propre
write.csv(df_clean, "donnees_finales_projet_2024.csv", row.names = FALSE)

print("✅ Fichier 'donnees_finales_projet_2024.csv' généré avec succès !")
