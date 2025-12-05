# Étape6 - AFC
# Analyse Factorielle des Correspondances
# Fichier de départ : delinquance_dep_clean_2024.csv

library(readr)      # import des données
library(dplyr)      # manipulation
library(FactoMineR) # AFC (CA)
library(factoextra) # visualisation des résultats de CA

# 1. Import du fichier clean (communi à tout le groupe)
delinq <- read_csv("delinquance_dep_clean_2024.csv")

# Vérifications rapides
dim(delinq)
names(delinq)
head(delinq)

# 2. Construction d'un indicateur global de délinquance par département
#    On somme les taux d'infractions (toutes les colonnes numériques sauf annee)
delinq <- delinq %>%
  mutate(
    delinq_global = rowSums(
      select(., -Code_departement, -Code_region, -annee),
      na.rm = TRUE
    )
  )

summary(delinq$delinq_global)

# 3. Création d'une variable qualitative : niveau de délinquance
#    On découpe delinq_global en 3 classes : Faible / Moyen / Élevé
delinq <- delinq %>%
  mutate(
    niveau_delinquance = cut(
      delinq_global,
      breaks = 3,
      labels = c("Faible", "Moyen", "Élevé")
    )
  )

# Contrôle : distribution des niveaux par région
table(delinq$niveau_delinquance)
table(delinq$Code_region, delinq$niveau_delinquance)

# 4. Construction du tableau de contingence (Région x Niveau de délinquance)
tab_afc <- table(delinq$Code_region, delinq$niveau_delinquance)
tab_afc

# 5. AFC avec FactoMineR
res_afc <- CA(tab_afc, graph = FALSE)

# Valeurs propres et inerties (pour le rapport)
res_afc$eig

# 6. Graphiques principaux

# 6.1. Biplot (lignes + colonnes)
fviz_ca_biplot(res_afc, repel = TRUE,
               title = "AFC - Régions et niveaux de délinquance (2024)")

# 6.2. Carte des régions (lignes)
fviz_ca_row(res_afc, repel = TRUE,
            title = "AFC - Régions (lignes)")

# 6.3. Carte des niveaux de délinquance (colonnes)
fviz_ca_col(res_afc, repel = TRUE,
            title = "AFC - Niveaux de délinquance (colonnes)")

# 7. Contributions (utile pour le texte du rapport)
res_afc$row$contrib    # contributions des régions aux axes
res_afc$col$contrib    # contributions des niveaux aux axes
