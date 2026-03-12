##### Marsh tit distribution modelling, script 1 to get data ####
##### Script written by Filibert Heim, filibert.heim@posteo.de, in March 2026 ####


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. Preps ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# load packages 
library(rgbif)
library(sf)
library(tidyverse)
select <- dplyr::select; filter <- dplyr::filter; rename <- dplyr::rename

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.1 Get occ data from GBIF ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# https://rpubs.com/jonesor/gettinggbifdata
# https://www.r-bloggers.com/2021/03/downloading-and-cleaning-gbif-data-with-r/
# https://docs.ropensci.org/rgbif/articles/getting_occurrence_data.html

# set species name 
speciesKey <- name_backbone(name = "Poecile palustris")$speciesKey

# this creates a request to prepare data from GBIF for download
occ.request <- occ_download(
  pred("speciesKey", speciesKey),
  pred("country", "SE"), 
  pred("hasGeospatialIssue", FALSE), 
  pred("hasCoordinate", TRUE), 
  pred("occurrenceStatus", "PRESENT"), 
  pred_gte("year", 1900), 
  user = Sys.getenv("GBIF_USER"), pwd = Sys.getenv("GBIF_PWD"), email = Sys.getenv("GBIF_EMAIL"), # these are my credentials
  format = "SIMPLE_CSV")

# this checks the status of the preparation for data download 
occ_download_wait(occ.request)

# load data from request which is stored in GBIF cloud, unzip and rename
occ_download_get(occ.request, path = "data/") 
unzip("data/0029308-260226173443078.zip", exdir = "data/")
file.rename(from = "data/0029308-260226173443078.csv", to = "data/MarshTit_gbif_download.csv")

# load data into R again
occ.data <- read.csv(file = "data/MarshTit_gbif_download.csv", header = T, sep = "\t") 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.2 Clean occ data ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# clean data 
occ.data <- occ.data %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) %>% 
  mutate(date = make_date(year, month, day)) %>%
  #filter(year(date) >= 2000) %>%
  select(stateProvince, date, collectionCode, coordinateUncertaintyInMeters, individualCount)

# plot data 
occ.data %>% ggplot() +
  geom_sf()

occ.data %>% mutate(year = year(date)) %>% group_by(year) %>% summarise(n = n()) %>%
  ggplot() + geom_col(mapping = aes(x = year, y = n))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3. Get environmental covariates ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# load libraries for data download 
library(sdmpredictors)
library(geodata)

# load administrative boundaries for sweden and it's counties 
sweden <- st_read("data/vect.data/Sweden_vect.shp") # from geodata::gadm(country = "Sweden", level = 0)
county <- st_read("data/vect.data/Counties_sweden_vect.shp") # from geodata::gadm(country = "Sweden", level = 1)
skane <- st_read("data/vect.data/Sweden_vect.shp") %>% filter(NAME_1 == "Skåne")

# get data 
sdmpredictors::get_layers_info() 
clim <- geodata::worldclim_global(var = "tavg", res = 10, country = "SE")


