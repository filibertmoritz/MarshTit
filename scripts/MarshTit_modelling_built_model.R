##### Marsh tit distribution modelling, script xx to built model(s) ####
##### Script written by Filibert Heim, filibert.heim@posteo.de, in March 2026 ####


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. Preps ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# load packages 
library(tidyverse)
library(sf)
select <- dplyr::select; filter <- dplyr::filter; rename <- dplyr::rename

# load skane and create study area 
skane <- st_read(dsn = 'data/vect.data/Skane_vect.shp') %>% st_transform(crs = 3006) #  from geodata::gadm(country = "Sweden", level = 0) and 1
study.area <- skane
study.area.buffer <- st_buffer(study.area, dist = 1000)

# load prepared presence-absence data and envCovs
occ.data <- readRDS("data/modelling.data.rds")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Built model ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~









