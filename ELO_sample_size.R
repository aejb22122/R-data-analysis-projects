# ---- Preliminaries ----

library(readxl)
df <- read_excel("ELO_Sample_Size.xlsx",
col_types = c("text", "numeric", "numeric"))
View(df)

# ---- Population parameter calculations ----

# ---- Sample size ----
# we are assuming that the population follows a normal distribution
# Sampling Size of Population Proportion

"La taille d'échantillon se calcule avec la formule suivante : 
n = t² × p × (1-p) / m²
n : Taille d'échantillon minimale pour l'obtention de résultats significatifs pour un événement et un niveau de risque fixé
t : Niveau de confiance (la valeur type du niveau de confiance de 95 % sera 1,96)
p : proportion estimée de la population qui présente la caractéristique
m : Marge d'erreur (généralement fixée à 5 %)
Ainsi, pour un événement ayant une probabilité de réalisation de 40 %, en prenant un niveau de confiance de 95 % et une marge d'erreur de 5 %, la taille d'échantillon devra être de 
n = 1,96² × 0,4 × 0,6 / 0,05² = 368,79
"

sample_size = function(confidence_level, margin = 0.5, margin_of_error = 0.05, population) { # Les variables de la fonction
        z.val = qnorm(0.5 + confidence_level/200)
        ss = (z.val^2 * margin * (1-margin))/margin_of_error^2
        p.ss = round((ss/(1 + ((ss-1)/population))), digits = 0)
        METHOD = paste("La taille de l'echantillon pour une population de",
                       population, "avec un intervalle", confidence_level,
                       "% ", sep = "")
        structure(list(Population = population,
                       "Confidence level" = confidence_level,
                       "Margin of error" = margin_of_error,
                       "Response distribution" = margin,
                       "Recommended sample size" = p.ss,
                       method = METHOD),
                  class = "power.htest")
}

# Let's apply the function to the desired population size and the confidence interval:
sample.size(95, population= 5386)
sample.size(95, population= 125)


sample.size(95, population = sample95$x[1])
sample.size(95, population = sample95$x[2])
sample.size(95, population = sample95$x[3])
sample.size(95, population = sample95$x[4])
sample.size(95, population = sample95$x[5])
sample.size(95, population = sample95$x[6])
sample.size(95, population = sample95$x[7])
sample.size(95, population = sample95$x[8])
sample.size(95, population = sample95$x[9])
sample.size(95, population = sample95$x[10])
sample.size(95, population = sample95$x[11])

sample.size(90, population = sample95$x[1])
