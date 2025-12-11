##############################################
# GEO GRAND BANDITISME 2024
# Croisement GB_cat x DO_cat (qualitatif x qualitatif)
##############################################

## 0) IMPORT DES DONNÉES ---------------------

df <- read.csv("donnees_finales_projet_2024.csv",
               check.names = FALSE)

names(df)  # juste pour contrôle visuel


## 1) VARIABLES SYNTHÉTIQUES -----------------
# Grand banditisme = Homicides + Trafic de stupéfiants + Vols avec armes
df$Grand_banditisme <-
  df$Homicides +
  df$`Trafic de stupéfiants` +
  df$`Vols avec armes`

# Délinquance ordinaire = violences + vols véhicule + cambriolages
df$Delinquance_ordinaire <-
  df$`Violences physiques hors cadre familial` +
  df$`Vols de véhicule` +
  df$`Cambriolages de logement`

# Vérifier que c'est bien créé
summary(df$Grand_banditisme)
summary(df$Delinquance_ordinaire)


## 2) CLASSEMENT EN 3 NIVEAUX ----------------
# Terciles : Faible / Moyen / Fort

df$GB_cat <- cut(
  df$Grand_banditisme,
  breaks = quantile(df$Grand_banditisme,
                    probs = c(0, 1/3, 2/3, 1),
                    na.rm = TRUE),
  include.lowest = TRUE,
  labels = c("Faible", "Moyen", "Fort")
)

df$DO_cat <- cut(
  df$Delinquance_ordinaire,
  breaks = quantile(df$Delinquance_ordinaire,
                    probs = c(0, 1/3, 2/3, 1),
                    na.rm = TRUE),
  include.lowest = TRUE,
  labels = c("Faible", "Moyen", "Fort")
)


## 3) TABLEAU DE CONTINGENCE -----------------

tab <- table(df$GB_cat, df$DO_cat)
tab
addmargins(tab)  # avec totaux


## 4) PROFILS LIGNE / COLONNE ----------------

# Profils ligne : répartition de DO_cat dans chaque niveau de GB_cat
prof_ligne <- prop.table(tab, 1)
prof_ligne

# Profils colonne : répartition de GB_cat dans chaque niveau de DO_cat
prof_col <- prop.table(tab, 2)
prof_col


## 5) TEST DU CHI-DEUX -----------------------

test_chi <- chisq.test(tab)
test_chi          # stat, ddl, p-value
test_chi$expected # effectifs théoriques


## 6) TABLEAU OBSERVÉ / THÉORIQUE ------------

result <- data.frame(
  GB         = rep(rownames(tab), times = ncol(tab)),
  DO         = rep(colnames(tab), each  = nrow(tab)),
  Observes   = as.vector(tab),
  Theoriques = as.vector(test_chi$expected)
)

result
##############################################













##le graph
# Nuage de points
plot(df$Grand_banditisme, df$Delinquance_ordinaire,
     pch = 19,
     col = "blue",
     xlab = "Grand banditisme",
     ylab = "Délinquance ordinaire",
     main = "Relation entre grand banditisme et délinquance ordinaire")

# Courbe de tendance (LOWESS)
lines(lowess(df$Grand_banditisme, df$Delinquance_ordinaire),
      col = "red", lwd = 3)

# Régression linéaire
abline(lm(Delinquance_ordinaire ~ Grand_banditisme, data = df),
       col = "darkgreen", lwd = 2)

# Légende
legend("topleft",
       legend = c("Départements", "Tendance LOWESS", "Régression linéaire"),
       col = c("blue", "red", "darkgreen"),
       pch = c(19, NA, NA),
       lty = c(NA, 1, 1),
       lwd = c(NA, 3, 2),
       bty = "n")

