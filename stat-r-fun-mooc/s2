# Introduction à la statistique avec R
# Semaine 2 ~ Intervalles de confiance

# Set working directory 
setwd("~/OneDrive/Documents/1. After-doctorate/R_projects/Introduction a la statistique avec R/working_directory")

# Liste des fichiers du directory
list.files()

# Importer un fichier *.csv
smp <- read.csv2("smp1.csv")

# Structure de la base de données
str(smp)

# Installer et utiliser un package
# Ce packet est pour les graphiques
install.packages("gplots", dep=TRUE)

# Faire appel aux packets:
library(gplots)

# IL y a une facon plus "jolie" de le faire les tableaux
# installer le packet puis la fonction describe
install.packages("prettyR")
library("prettyR")

# ---- Calculs d'un intervalle de confiance ----
describe(smp$age)

# Sous réserve que la distribution suit une loi normale
# Intervalle de confiance a 95 % d'un parametre i.e. moyenne mean + 1.96 *sd/sqrt(valid.n)
38.9-1.96*13.28/sqrt(797)       # Borne inferieure
38.9+1.96*13.28/sqrt(797)       # Borne superieure

# Intervalle à 95% d'un pourcentage 
install.packages("binom")
library(binom)

# Trois personnes déprimée tirée au sort d'une vaste population (3, 10 = échantillon):
help("binom.confint")
binom.confint(3, 10, methods = "all")
# Il existe plusieurs moyen d'éstimer un interval de confiance. En général, on utilise la méthode
# method = "exact"...
# Quand la taille de l'échantillon est grande, plusieurs méthodes convergent vers le meme
# Résultat.

binom.confint(300, 1000, method = "all")

# ---- Coefficient de corrélation ----
help("cor")
# Force de la liaison (liaison =/= causalité)
# Le coef de corrélation de Pearson
# Coefficient de correlation (jitter dans le graphs, c'est pour delimiter
# les points qui ont les meme coordonnees# )

plot(jitter(smp$age), jitter(smp$n.enfant))
cor(smp$age, smp$n.enfant, use = "complete.obs")

# ---- Risque relatif et ODDS-Ratio ----
# Ajout d'une variable (ed.d) qui est la variable "eb" recodée:
smp$ed.b <- ifelse(smp$ed > 2, 1, 0)
str(smp)

# Vérification
table(smp$ed.b, smp$ed, deparse.level = 2, useNA = "always")

# Calcul de l'ODD-Ratio
install.packages("Epi")
library(Epi)

twoby2(1 - smp$ed.b, 1 - smp$dep.cons) # 1-la variable (on transforme les variables)
