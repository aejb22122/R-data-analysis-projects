# Annick Eudes Jean Baptiste
# Mapping of ELOs
# Tuesday june 9, 2020

# ---- Preliminaries ----
# Install the relevant libraries - do this one time
# install.packages("lubridate")
# install.packages("ggplot2")
# install.packages("ggmap")
# install.packages("data.table")
# install.packages("ggrepel")
# install.packages("dplyr")
# install.packages('sf') # This installs shapefiles

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
# col1 = "#011f4b"
# col2 = "#6497b1"
# col3 = "#b3cde0"
# col4 = "#CC0000"

# Google requires API keys now :
register_google(key='AIzaSyCMh5ZKF40xIe_7sol_HL-zXsrMCnwG9lE')

# Use this website to geocode addresses to lat and long
# https://www.findlatitudeandlongitude.com/batch-geocode/#.U7IcAI1dWmI
# Get Geocoding API from Google 
# https://console.cloud.google.com/google/maps-apis/apis/geocoding-backend.googleapis.com/credentials?project=centered-oasis-275616


# ---- Loading the data sets ----
# Load Data File and Assign Variables
# Read in the Elo data
data <- read_excel("equity_map/2020_only_elo_mapping.xlsx")
View(data)

csc_indicators <- read_excel("equity_map/CSC Zip-Code-Report-Data-Tables-Updated-March-2020.xlsx")
View(csc_indicators)


# Geocoded data (latitudes and longitudes)
geo_coded <- read_excel("equity_map/2020_05_08_geocode-output.xlsx")
View(geo_coded)

# Merging the datasets 
# Let's merge the ELO data with the CSC indicators
df2 = inner_join(data, csc_indicators, by = 'ZIP_Code')
str(df2)

df = inner_join(df2, geo_coded, by = 'OST_Program_ID')
str(df)

# ---- Preparing data for plotting ----
# by zipcodes
elo_by_zipcode <- 
        df %>%
        group_by(ZIP_Code) %>%
        summarize(long = mean(longitude),
                  lat = mean(latitude),
                  n = n()) %>%
        ggplot(aes(long, lat, size = n)) +
        geom_point() +
        theme_bw()

elo_by_zipcode + ggtitle('Concentration of ELOs (Oct 2019 - March 2020)')

# Service by zipcode 
# This shows the number of time an ELO was present per zipCode
concentration_by_zipcode_table <- 
        df %>%
        group_by(ZIP_Code) %>%
        summarize(long = mean(longitude),
                  lat = mean(latitude),
                  n = n())

plot_data <- as.data.frame(concentration_by_zipcode_table)


# Services by community needs index
service_by_needs <- 
        df %>%
        group_by(`Community Needs Index, 2020.x`) %>%
        summarize(long = mean(longitude),
                  lat = mean(latitude),
                  n = n())

plot_data2 <- as.data.frame(service_by_needs)


# "% of Teen Births, 2016-2018 avg."
teen_births <- 
        df %>% 
        group_by(`% of Teen Births, 2016-2018 avg.`) %>%
        summarize(long = mean(longitude),
                  lat = mean(latitude),
                  n = n())

plot_data3 <- as.data.frame(teen_births)

# ---- Creating maps ----

# Looking for a spacial trend
# get_map(location = c(lon = -80.0717282, lat = 26.55178))

# Plotting the latitudes and longitutes location of the ELOs
df %>% ggplot(aes(longitude, latitude)) + geom_point() +        
        labs(title = 'Programs that received Expanded Learning Opportunities \nOct 2019 - March 2020')


#map <- get_map('Boynton Beach, florida', maptype = 'toner-lite')
map <- get_map('Palm Beach County', maptype = 'toner-lite')
map_canevas <- ggmap(map)
map_canevas

# First maps
map_canevas +
        geom_point(data = df, mapping = aes(x = `longitude`, y = `latitude`), color = 'blue') +
        labs(color = 'ELO in Programs',
             title = 'Programs that received ELOs (Oct 2019 - March 2020)')

map_canevas +
        geom_point(data = plot_data, mapping = aes(x= long, y = lat,
                                                   col = ZIP_Code, size = n)) +
        scale_color_gradient(low="blue", high="red") +
        #scale_color_distiller(palette = "OrRd", direction = 1) +
        labs(color = 'Concentration of \nELO service per Zipcode',
             size = '# of ELOs',
             title = 'Concentration of Expanded Learning Opportunities \n Oct 2019 - March 2020')


map_canevas +
        geom_point(data = plot_data2, mapping = aes(x= long, y = lat,
                                                   col = `Community Needs Index, 2020.x`, size = n)) +
        scale_color_distiller(palette = "Reds", direction = 1) +
        labs(color = 'Concentration of \n ELO per area of need\n(Community Needs Index)',
             size = '# of ELOs',
             title = 'Concentration of Expanded Learning Opportunities in areas of needs \nOct 2019 - March 2020')


map_canevas +
        geom_point(data = plot_data3, mapping = aes(x= long, y = lat,
                                                    col = `% of Teen Births, 2016-2018 avg.`, size = n)) +
        scale_color_distiller(palette = "Blues", direction = 1) +
        labs(color = 'Concentration of \n ELO per area of need\n(teen births) ',
             size = '# of ELOs',
             title = 'Concentration of Expanded Learning Opportunities in areas of needs \n(teen births)\n Oct 2019 - March 2020')
















# Stanford example
# ggmap(map) +
#         geom_point(data = df, mapping = aes(x = longitude, y = latitude, 
#                                             col = median_price, size = transactions)) +
#         scale_color_distiller(palette = "YlOrRd", direction = 1)


# DONT DELETE
# map_canevas <- ggmap(get_googlemap(center = c(lon = -80.0717282, lat = 26.55178), scale = 0.7,
#                                    zoom = 8,
#                          maptype ='roadmap',
#                          color = 'blue'))

# map_canevas + geom_point(aes(x = longitude, y = latitude), data = df, size = 2.5) + 
#         theme(legend.position="bottom")


# DONT DELETE
# # Let's try adding the zipcodes
# #pbc_zipcode <- read_sf('C:/Users/annic/OneDrive/Documents/data_science/my_analysis_projects/elo_map_pbc/equity_map/ZIPCODE/ZIPCODE.shp')
# pbc_zipcode <- read_sf('C:/Users/annic/OneDrive/Documents/data_science/my_analysis_projects/elo_map_pbc/equity_map/ZIPCODE/ZIPCODE.shx')
# 
# ggplot(elo_by_zipcode_table) +
#         geom_sf(data = pbc_zipcode) +
#         geom_point(aes(long, lat, size = n)) +
#         theme_bw()
#         
# 
# elo_by_zipcode %>%
#         ggmap(get_googlemap(center = c(lon = -80.0717282, lat = 26.55178), scale = 0.7,
#                             zoom = 8,
#                             maptype ='roadmap',
#                             color = 'blue'))+
#         geom_point(aes(longitude, latitude, color = runs_from))

