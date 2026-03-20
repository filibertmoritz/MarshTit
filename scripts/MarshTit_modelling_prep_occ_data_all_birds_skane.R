##### Marsh tit distribution modelling, script 4 to clean occurence data  ####
##### Script written by Filibert Heim, filibert.heim@posteo.de, in March 2026 ####


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. Preps ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# load packages 
library(CoordinateCleaner)
library(sf)
library(tidyverse)
library(data.table)
select <- dplyr::select; filter <- dplyr::filter; rename <- dplyr::rename

# load observation data 
raw.all.data <- fread(file = "data/Birds_skane_gbif_download.csv") 

# load basic variables 
load("data/basic.variables.RData")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Data cleaning and manipulation ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# data cleaning
pres.all.data.raw <- raw.all.data %>% #clean.data %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326, remove = F) %>% 
  st_transform(crs = 3006) %>%
  mutate(date = make_date(year, month, day)) %>%
  drop_na(date) %>%
  filter(year(date) %in% study.period) %>% # filter for study period
  filter(month(date) %in% breeding.period)  # filter for breeding period
  # filter(coordinateUncertaintyInMeters <= 1000) %>% # low uncertainty filter skews data to more recent observations
rm(raw.all.data) # free memory

# filter for study area
pres.all.data.raw <- pres.all.data.raw %>% st_filter(study.area)

# perform data cleaning afterwards!

# this completely relies on the functionalities of the package CoordinareCleaner
pres.all.data <- clean_coordinates(pres.all.data.raw %>% st_drop_geometry(), countries = "countryCode", value = "clean", 
                                   tests = c("centroids","equal", "gbif", "institutions","seas", "zeros")) # this prevents a few tests that crash my laptop

# remove unneeded columns 
pres.all.data <- pres.all.data %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326, remove = F) %>% 
  st_transform(crs = 3006) %>% 
  select(stateProvince, date, collectionCode, coordinateUncertaintyInMeters, individualCount, species)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4. Export data and remove unneeded data ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

saveRDS(pres.all.data, "data/occ.data.all.skane.rds")
rm(list = ls())
gc()
