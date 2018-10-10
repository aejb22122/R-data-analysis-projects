# Introduction à la statistique avec R
# Semaine 1 ~ Définitions

# Set working directory 
setwd("~/OneDrive/Documents/1. Research analyst ressources R/Online courses/0. Introduction a la statistique avec R")

# Liste des fichiers du directory
list.files()

# Importer un fichier *.csv
smp <- read.csv2("smp1.csv")

# Structure de la base de données
str(smp)

# Installer et utiliser un package
# Ce packet est pour les graphiques
install.packages("gplots", dep=TRUE)

# Faire appel au packet
library(gplots)

## Représentations graphiques ~ Variable aléatoire qualitative
barplot(table(smp$prof))

# ou un camembert ~ pie chart
pie(table(smp$prof))

#### Variables aléatoires quantitative continue

## Représentations graphiques 
hist(smp$age)
### On peut changer les fonctionnalités du graphique
hist(smp$age, xlab = "age", ylab = "Nombres de détenus", main = "Histogramme test", col = "blue")

## Boxplot ~ 
boxplot(smp$age, xlab = "age")

## Boxplot en sous-groupe, ici les sous-groupes en fonction du niverau de rs recherché
boxplot(smp$age ~ smp$rs, xlab = "age")

# Diagramme cartesien, nuages de points plot(x,y)
plot(smp$age, smp$n.enfant)

# Pour détailler les points qui ont les memes valeurs de x et de y nous donnent
# Avec la fonction "jitter"
plot(jitter(smp$age), jitter(smp$n.enfant))

# Représentation graphique d'évolution d'une variable dans le temps (il faut le faire avec gplots)
# On peut le faire pour les études longitunales (comme dans la thèse)
# L'évolution moyenne :
plotmeans(repdate$HDRS ~ repdate$VISIT, gap = 0, barcol = "blue")


# Mesures de dispersions (Moyennes, écart-types, minimums, maximums.quantils etc)
summary(smp)

# Summary par variables 
summary(smp$n.enfant)

# IL y a une facon plus "jolie" de le faire, installer le packet puis la fonction describe
install.packages("prettyR")
library("prettyR")
describe(smp)


