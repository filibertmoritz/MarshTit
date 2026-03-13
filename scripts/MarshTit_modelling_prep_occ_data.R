##### Marsh tit distribution modelling, script 4 to clean occurence data  ####
##### Script written by Filibert Heim, filibert.heim@posteo.de, in March 2026 ####


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. Preps ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# load packages 
library(CoordinateCleaner)
library(sf)
library(tidyverse)
library(CoordinateCleaner)
select <- dplyr::select; filter <- dplyr::filter; rename <- dplyr::rename

# load observation data 
raw.data <- read.csv(file = "data/MarshTit_gbif_download.csv", header = T, sep = "\t") 

# load study area 
skane <- st_read(dsn = 'data/vect.data/Skane_vect.shp') %>% st_transform(crs = 3006) #  from geodata::gadm(country = "Sweden", level = 0) and 1
study.area <- skane
study.area.buffer <- st_buffer(study.area, dist = 1000)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Data cleaning and manipulation ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# this completely relies on the functionalities of the package CoordinareCleaner
clean.data <- clean_coordinates(raw.data, countries = "countryCode", value = "clean")

# data cleaning
pres.data <- clean.data %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) %>% 
  st_transform(crs = 3006) %>%
  mutate(date = make_date(year, month, day)) %>%
  drop_na(date) %>%
  filter(year(date) >= 2016) %>% 
  # filter(coordinateUncertaintyInMeters <= 1000) %>% # low uncertainty filter skews data to more recent observations
  select(stateProvince, date, collectionCode, coordinateUncertaintyInMeters, individualCount)

# filter for study area
pres.data <- pres.data %>% st_filter(study.area)

# create presence-absence column 
pres.data <- pres.data %>% 
  mutate(occ = 1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3. Creation of pseudo-absences  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# sample pseudo-absences
set.seed(123)
abs.data <- st_sample(study.area, size = 2*dim(pres.data)[1], type = "random") %>% 
  st_sf() %>% 
  mutate(occ = 0)

# join to presences 
occ.data <- bind_rows(pres.data, abs.data)

plot(st_geometry(study.area), col = "lightblue")
plot(st_geometry(occ.data), add = TRUE, col = ifelse(occ.data$occ == 1, "red", "black"), size = 0.1)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4. Export data ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

saveRDS(occ.data, "data/occ.data.rds")



