# Projet Data Exploration - Etape 1 - Préparation des données de délinquance départementale (2024)

library(readr)   # import de données
library(dplyr)   # manipulation de données 
library(tidyr)   # passage long -> large

# 1. Import du fichier départemental (format CSV séparé par ';')
data_dep <- read_csv2("donnee-dep-data.gouv-2024-geographie2025-produit-le2025-06-04.csv")

# TESTS
dim(data_dep)      # nombre de lignes / colonnes
names(data_dep)    # noms des variables
table(data_dep$annee)  # années disponibles dans le fichier

# 2. Filtre pour garder uniquement 2024
delinq_2024 <- data_dep %>%
  filter(annee == 2024)

#TEST
head(delinq_2024)  

# 3. Filtre pour garder uniquement les Infractions
delinq_2024_inf <- delinq_2024 %>%
  filter(unite_de_compte == "Infraction") %>%
  select(Code_departement, Code_region, annee, indicateur, taux_pour_mille)

#TEST
head(delinq_2024_inf)  

# 4. Passage au format large : une colonne par type d’infraction (taux pour 1000 hab.)
delinq_2024_wide <- delinq_2024_inf %>%
  pivot_wider(
    names_from  = indicateur,      # chaque type d’infraction devient une variable
    values_from = taux_pour_mille  # valeur = taux correspondant
  )

# Vérification du tableau final
names(delinq_2024_wide)
head(delinq_2024_wide)

# 5. Export du fichier propre pour le reste du projet
write_csv(delinq_2024_wide, "delinquance_dep_clean_2024.csv")
