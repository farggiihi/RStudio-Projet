library(readr)
library(dplyr)

data_dep <- read_csv2("donnee-dep-data.gouv-2024-geographie2025-produit-le2025-06-04.csv")

# Juste pour vérifier qu'il est bien chargé :
dim(data_dep)      # nb de lignes / colonnes
names(data_dep)   # liste des colonnes

which(names(data_dep) == "annee")

table(data_dep$annee)



# Si une colonne ANNEE existe, on filtre 2024
if ("annee" %in% names(data_dep)) {
  data_2024 <- data_dep %>% filter(annee == 2024)
} else {
  # Sinon, on suppose que le fichier correspond déjà à la dernière année dispo
  data_2024 <- data_dep
}

# On garde ce nom pour la suite :
delinq_2024 <- data_2024

#test
head(delinq_2024)


# 1) On garde uniquement les infractions
delinq_2024_inf <- delinq_2024 %>%
  filter(unite_de_compte == "Infraction")

# 2) On garde les colonnes dont on a besoin pour la suite
delinq_2024_inf <- delinq_2024_inf %>%
  select(Code_departement, Code_region, annee, indicateur, taux_pour_mille)

#test
unique(delinq_2024_inf$unite_de_compte)  # normalement n'existe plus -> on l'a enlevée
head(delinq_2024_inf)

library(tidyr)

delinq_2024_wide <- delinq_2024_inf %>%
  pivot_wider(
    names_from = indicateur,        # chaque type d’infraction devient une colonne
    values_from = taux_pour_mille   # et la valeur dans la colonne = le taux
  )

#test
names(delinq_2024_wide)
head(delinq_2024_wide)

write_csv(delinq_2024_wide, "delinquance_dep_clean_2024.csv")








