##### Marsh tit distribution modelling, script 1 to get marsh tit occurence data from gbif ####
##### Script written by Filibert Heim, filibert.heim@posteo.de, in March 2026 ####


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. Preps ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# load packages 
library(rgbif)
library(sf)
library(tidyverse)
library(CoordinateCleaner)
select <- dplyr::select; filter <- dplyr::filter; rename <- dplyr::rename

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Get Marsh Tit occ data from GBIF ####
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
# occ.data <- read.csv(file = "data/MarshTit_gbif_download.csv", header = T, sep = "\t") 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Get ALL Bird occ data from GBIF ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# get skane shp 
skane <- st_read("data/vect.data/Skane_vect.shp") %>% st_buffer(dist = 1000)
bbox <- st_bbox(skane)

# set key for birds 
birdKey <- name_backbone(name = "Aves")$usageKey

# this creates a request to prepare data from GBIF for download
occ.request <- occ_download(
  pred("taxonKey", birdKey),
  pred("decimalLongitude", paste(bbox["xmin"], bbox["xmax"], sep=",")),
  pred("decimalLatitude", paste(bbox["ymin"], bbox["ymax"], sep=",")),
  pred("hasGeospatialIssue", FALSE), 
  pred("hasCoordinate", TRUE), 
  pred("occurrenceStatus", "PRESENT"), 
  pred_gte("year", 2015), 
  user = Sys.getenv("GBIF_USER"), pwd = Sys.getenv("GBIF_PWD"), email = Sys.getenv("GBIF_EMAIL"), # these are my credentials
  format = "SIMPLE_CSV")

# this checks the status of the preparation for data download 
occ_download_wait(occ.request)

# load data from request which is stored in GBIF cloud, unzip and rename
occ_download_get(occ.request, path = "data/") 
unzip("data/0042183-260226173443078.zip", exdir = "data/")
file.rename(from = "data/0042183-260226173443078.csv", to = "data/Birds_skane_gbif_download.csv")
