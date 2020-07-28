# Annick Eudes Jean Baptiste
# Mapping of ELOs
# Tuesday june 29, 2020

# ---- Preliminaries ----

# Load the relevant libraries - do this every time
library(lubridate)
library(ggplot2)
library(ggmap)
library(dplyr)
library(data.table)
library(ggrepel)
library(sf)
library(readxl)
# Create some color variables for graphing later
col1 = "#011f4b"
col2 = "#6497b1"
col3 = "#b3cde0"
col4 = "#CC0000"

# Google requires API keys now :
register_google(key='-----------------------')


# ---- Loading the data sets ----
# Load Data File and Assign Variables
# Read in the Elo data for 2019 only
library(readxl)
data_elo_2018_2019 <- read_excel("equity_map/2018_2019_only_elo_mapping.xlsx",
col_types = c("text", "text", "text",
"text", "text", "text", "numeric",
"text", "numeric", "numeric", "numeric",
"numeric", "numeric", "numeric",
"numeric", "numeric", "text", "text",
"text", "numeric", "numeric"))

View(data_elo_2018_2019)
str(data_elo_2018_2019)

names(data_elo_2018_2019)[20] <- "ZIP_Code"

csc_indicators <- read_excel("equity_map/CSC Zip-Code-Report-Data-Tables-Updated-March-2020.xlsx",
col_types = c("numeric", "text", "numeric",
"numeric", "numeric", "numeric",
"numeric", "numeric", "numeric",
"numeric", "text", "text", "numeric",
"numeric"))
View(csc_indicators)

#as.numeric(csc_indicators$`Community Needs Index, 2020`)

# Geocoded data (latitudes and longitudes)
geo_coded <- read_excel("equity_map/2020_05_08_geocode-output.xlsx",
col_types = c("text", "text", "text",
"numeric", "numeric"))

View(geo_coded)



# Merging the datasets 
# Let's merge the ELO data with the CSC indicators
df3 = inner_join(data_elo_2018_2019, csc_indicators, by = 'ZIP_Code')
str(df3)

names(df3)[1]<- "OST_Program_ID"


data_2018_2019 = inner_join(df3, geo_coded, by = 'OST_Program_ID')
str(data_2018_2019)
#data_2019$`Community Needs Index, 2020.x` <- as.numeric(data_2019$`Community Needs Index, 2020.x`)



# 'OST_Program_ID ', 'Organization', 'ZIP_Code', 'Community Needs Index, 2020.x', 'latitude', 'longitude'
# "ZIP_Code" "Community Needs Index, 2020.x" "latitude" "longitude"
subdf <- data_2018_2019[c(20,21,37,38)]
str(subdf)

# ---- Preparing data for plotting ----
# by zipcodes
elo_by_zipcode_2018_2019 <- 
        subdf %>%
        group_by(ZIP_Code) %>%
        summarize(long = mean(longitude),
                  lat = mean(latitude),
                  n = n()) %>%
        ggplot(aes(long, lat, size = n)) +
        geom_point() +
        theme_bw() 

elo_by_zipcode_2018_2019 + ggtitle('Concentration of ELOs (Oct 2018 - Sept 2019)')

# Service by zipcode 
# This shows the number of time an ELO was present per zipCode
concentration_by_zipcode_table_2018_2019 <- 
        subdf %>%
        group_by(ZIP_Code) %>%
        summarize(long = mean(longitude),
                  lat = mean(latitude),
                  n = n())

plot_data_2018_2019 <- as.data.frame(concentration_by_zipcode_table_2018_2019)


# Services by community needs index
service_by_needs_2018_2019 <- 
        subdf %>%
        group_by(`Community Needs Index, 2020.x`) %>%
        summarize(long = mean(longitude),
                  lat = mean(latitude),
                  n = n())

plot_data2_2018_2019 <- as.data.frame(service_by_needs_2018_2019)
# Table of community needs index
View(plot_data2_2018_2019)
 
# # "% of Teen Births, 2016-2018 avg."
# teen_births <- 
#         df %>% 
#         group_by(`% of Teen Births, 2016-2018 avg.`) %>%
#         summarize(long = mean(longitude),
#                   lat = mean(latitude),
#                   n = n())
# 
# plot_data3 <- as.data.frame(teen_births)

# ---- Creating maps ----

# Looking for a spacial trend
# get_map(location = c(lon = -80.0717282, lat = 26.55178))

# Plotting the latitudes and longitutes location of the ELOs 2018-2019
subdf %>% ggplot(aes(longitude, latitude)) + geom_point() + 
        labs(title = 'Programs that received Expanded Learning Opportunities \nOct 2018 - Sept 2019')



#map <- get_map('Boynton Beach, florida', maptype = 'toner-lite')
map <- get_map('Palm Beach County', maptype = "toner-lite")
map_canevas <- ggmap(map)
map_canevas

# First maps
map_canevas +
        geom_point(data = subdf, mapping = aes(x = `longitude`, y = `latitude`), color = 'blue') +
        labs(color = 'ELO in Programs',
             title = 'Programs that received ELOs (Oct 2018 - Sept 2019)')

map_canevas +
        geom_point(data = plot_data_2018_2019, mapping = aes(x= long, y = lat,
                                                   col = ZIP_Code, size = n)) +
        scale_color_gradient(low="blue", high="red") +
        #scale_color_distiller(palette = "OrRd", direction = 1) +
        labs(color = 'Concentration of \nELO service per Zipcode',
             size = '# of ELOs',
             title = 'Concentration of ELOs (Oct 2018 - Sept 2019)')


map_canevas +
        geom_point(data = plot_data2_2018_2019, 
                   mapping = aes(x= long, y = lat,
                                 col = `Community Needs Index, 2020.x`, size = n)) +
        scale_color_distiller(palette = "Reds", direction = 1) +
        labs(color = 'Concentration of \n ELO per area of need \n(Community Needs Index)',
             size = '# of ELOs',
             title = 'Concentration of ELOs in areas of needs \n(Oct 2018 - Sept 2019)')


# map_canevas +
#         geom_point(data = plot_data3, mapping = aes(x= long, y = lat,
#                                                     col = `% of Teen Births, 2016-2018 avg.`, size = n)) +
#         scale_color_distiller(palette = "Blues", direction = 1) +
#         labs(color = 'Concentration of \n ELO per area of need\n(teen births) ',
#              size = '# of ELOs',
#              title = 'Concentration of Expanded Learning Opportunities in areas of needs \n(teen births)\n Oct 2019 - March 2020')
# 
# 
# 
# 



# 
# boynton_beach <-  get_map("Boynton Beach", maptype = "roadmap") 
# 
# ggmap(boynton_beach) +
#         geom_point(data = subdf, aes(x = longitude, y = latitude), color = "navy", size = 1)
# 
# 
# # Plotting the latitudes and longitutes location of the ELOs
# ggmap(boynton_beach) %>% ggplot(aes(longitude, latitude)) + geom_point()
# 
