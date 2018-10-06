# Map Nigeria
# Proposed Schedule for the MCV2 Introduction
# World Health Organisation 
# Accelerated Disease Control - ADC

# Installing the required packages
install.packages("maptools", dependencies = TRUE)
install.packages("raster")

# Loading the packages
library(maptools)
library(raster)

# ---- Map of the proposed Schedule for the MCV2 Introduction in Nigeria ----

setwd("~/OneDrive/Documents/1. After-doctorate/R_projects")
# Dowloading files directly from https://gadm.org/data.html
# Remember to use the "R (sp)" format for the R file

# file_url <- "https://biogeo.ucdavis.edu/data/gadm3.6/Rsp/gadm36_NGA_2_sp.rds"
# destfile <- "~/OneDrive/Documents/1. After-doctorate/R_projects/nigeria.rds"

#download.file(file_url, destfile = destfile)
#date_of_download <- date()      # So we can remember when the file was downloaded; and know when the analysis was done.

# Loading the *.rds file :
nigeria_adm <- readRDS("~/OneDrive/Documents/1. After-doctorate/R_projects/gadm36_NGA_1_sp.rds")

# Plotting Nigerai ADM
plot(nigeria_adm)

# Shadding different areas :
# This gives you the data frame by opening the data slot of the "nigeria_adm" object
# It will give the different states :
nigeria_adm@data

# Names of the states
states <- nigeria_adm$NAME_1


# 1) create a vector of colours for all states in the dataset (37 = the number of states)
myColours <- rep("white", 37)
myColours[c(1, 3, 4, 6, 9, 11, 14, 17, 33)] <- "palegreen"         # Phase I
myColours[c(10, 12, 13, 31, 25, 28, 29, 30)] <- "orange"       # Phase II
myColours[c(2, 5, 8, 16, 23, 24, 27, 35, 36)] <- "papayawhip"   # Phase III
myColours[c(7, 15, 18, 19, 20, 21, 22, 26, 32, 34, 37)] <- "lavender" # Phase IV

# 2) The selected areas should be in green
plot(nigeria_adm, col = myColours, border = 'black')


# Names in the map
u <- unique(nigeria_adm$NAME_1)
u
m <- match(nigeria_adm$NAME_1, u)
plot(nigeria_adm, col = myColours[m], border = 'black')
text(nigeria_adm, "NAME_1", halo = FALSE)

# ---- Phase II B ----
remove(list = ls())

# Loading the *.rds file :
nigeria_adm <- readRDS("~/OneDrive/Documents/1. After-doctorate/R_projects/gadm36_NGA_1_sp.rds")

# Plotting Nigerai ADM
plot(nigeria_adm)

# Shadding different areas :
# This gives you the data frame by opening the data slot of the "nigeria_adm" object
# It will give the different states :
nigeria_adm@data

# Names of the states
states <- nigeria_adm$NAME_1


# 1) create a vector of colours for all states in the dataset (37 = the number of states)
myColours <- rep("ghostwhite", 37)
myColours[c(22, 34, 27, 32, 8, 15)] <- "orange"

# 2) The selected areas should be in green
plot(nigeria_adm, col = myColours, border = 'black')


# Names in the map
u <- unique(nigeria_adm$NAME_1)
u
m <- match(nigeria_adm$NAME_1, u)
plot(nigeria_adm, col = myColours[m], border = 'black')
text(nigeria_adm, "NAME_1", halo = FALSE)

# ----- MNT Campaign ----
remove(list = ls())

# Loading the *.rds file :
nigeria_adm <- readRDS("~/OneDrive/Documents/1. After-doctorate/R_projects/gadm36_NGA_1_sp.rds")

# Plotting Nigerai ADM
plot(nigeria_adm)

# Shadding different areas :
# This gives you the data frame by opening the data slot of the "nigeria_adm" object
# It will give the different states :
nigeria_adm@data

# Names of the states
states <- nigeria_adm$NAME_1


# 1) create a vector of colours for all states in the dataset (37 = the number of states)
myColours <- rep("ghostwhite", 37)
myColours[c(6, 9, 3, 33, 10)] <- "darkorchid3"

# 2) The selected areas should be in green
plot(nigeria_adm, col = myColours, border = 'black')


# Adding the names in the map
u <- unique(nigeria_adm$NAME_1)
u
m <- match(nigeria_adm$NAME_1, u)
plot(nigeria_adm, col = myColours[m], border = 'black')
text(nigeria_adm, "NAME_1", halo = FALSE)
