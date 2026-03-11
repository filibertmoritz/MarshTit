##### Marsh tit distribution modelling, script 1 to get data ####
##### Script written by Filibert Heim, filibert.heim@posteo.de, in March 2026 ####


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. Preps ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# load packages 
library(rgbif)
library(tidyverse)
select <- dplyr::select; filter <- dplyr::filter; rename <- dplyr::rename


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Get data from GBIF ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# set password and stuff
occ_data(scientificName = "Poecile palustris", country = "SE")

occ.data <- occ_download(
  pred("scientificName", "Poecile palustris"),
  pred("country", "SE"), 
  pred("hasGeospatialIssue", FALSE), 
  pred("hasCoordinate", TRUE), 
  pred("occurrenceStatus", "PRESENT"), 
  pred_gte("year", 1900), 
  user = GBIF_USER, pwd = GBIF_PASSWORD, email = GBIF_EMAIL,
  format = "SIMPLE_CSV")
