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
# 2. Get data from GBIF ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
# 3. Clean data ####
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


