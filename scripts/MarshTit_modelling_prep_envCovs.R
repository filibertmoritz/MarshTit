##### Marsh tit distribution modelling, script xx to get habitat variables ####
##### Script written by Filibert Heim, filibert.heim@posteo.de, in March 2026 ####



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. Preps ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# load packages 
library(tidyverse)
library(terra)
library(sf)
library(exactextractr)
select <- dplyr::select; filter <- dplyr::filter; rename <- dplyr::rename

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. load and prepare vector data or names of rasters ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# load basic variables 
load("data/basic.variables.RData")

# get all rasters 
rast.files <- list.files(path = "data/raster.data/")
lidar.files <- rast.files[grep(x = rast.files, pattern = "na.tif")]
lidar.names <- c("LiDAR.bottom", "LiDAR.lower", "LiDAR.upper", "LiDAR.top", "canopy.height")

# get occ data 
occ.data <- readRDS("data/occ.data.rds") 
occ.data <- occ.data %>% mutate(ID = row_number())
occ.buffer <- occ.data %>% st_buffer(dist = buffer.size, nQuadSegs = 1) 
occ.buffer.landscape <- occ.data %>% st_buffer(dist = buffer.landscape, nQuadSegs = 1) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3. Extract data from rasters ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 3.1 LiDAR-data  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Source: Ola and https://www.lantmateriet.se/en/geodata/our-products/product-list/laser-data-download-nh/ ?

# loop over all lidar files
for(i in 1:length(lidar.files)){
  # load and crop rasters 
  r <- terra::rast(x = paste0("data/raster.data/", lidar.files[i])) # load raster
  r <- terra::crop(x =r, y = occ.buffer) # crop to study area to avoid memory issues 
  
  # extract area-weighted means 
  extr.dat <- exact_extract(r, occ.buffer, fun = "mean", weights = "area", append_cols = "ID")
  names(extr.dat)[2] <- lidar.names[i]
  
  # join back to grid to store extracted data 
  occ.buffer <- occ.buffer %>% 
    left_join(extr.dat, join_by(ID))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 3.2 Land cover data  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Source: NMD basic 2018 version 1.1
# https://geodata.naturvardsverket.se/nedladdning/marktacke/NMD2018/NMD2018_ProductDescription_ENG.pdf
# https://geodatakatalogen.naturvardsverket.se/geonetwork/srv/swe/catalog.search#/metadata/8853721d-a466-4c01-afcc-9eae57b17b39

# pre-process land cover data 
r <- terra::rast(x = "data/raster.data/nmd2018bas_ogeneraliserad_v1_1.tif") # load .tif file
r <- terra::crop(r, study.area.buffer) # crop to study area
r <- terra::as.factor(r) # save as factorial values in raster 

# improve naming of the land-cover classes 
source("scripts/MarshTit_modelling_Land_use_labels.R") # create labels in english in other script
levels(r)[[1]] <- levels(r)[[1]] %>% left_join(labels.eng, join_by(ID)) %>% select(-Opacity, -Klass.eng) # set labels in levels of raster 

# extract data from raster 
extr.dat <- exactextractr::exact_extract(r, occ.buffer, fun = 'weighted_frac', weights = 'area', append_cols = 'ID')
classes.available <- as.numeric(gsub(".*?([0-9]+).*", "\\1", names(extr.dat))[-1]) # for a manual check which classes are there 
names(extr.dat) <- c('ID', paste('NMD', levels(r)[[1]]$Klass.eng.short[match(classes.available, levels(r)[[1]]$ID)], sep = '.')) # WATCH OUT - this lines removes class 0!
names(extr.dat) <- gsub(x = names(extr.dat),pattern = " ", replacement = ".") # make colnames prettier 
occ.buffer <- occ.buffer %>% left_join(extr.dat, join_by(ID)) # join back to df

# as.numeric(gsub(".*?([0-9]+).*", "\\1", names(extr.dat))[-1]) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 3.3 Moisture index  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Source https://geodatakatalogen.naturvardsverket.se/geonetwork/srv/swe/catalog.search#/metadata/cae71f45-b463-447f-804f-2847869b19b0

# pre-process moisture data 
r <- terra::rast(x = "data/raster.data/Markfuktighetsindex_NMD_del9.tif") # load .tif file
r <- terra::project(r, crs(occ.buffer))
r <- terra::crop(r, study.area.buffer) # crop to study area

# extract data from raster 
extr.dat <- exactextractr::exact_extract(r, occ.buffer, fun = 'mean', weights = 'area', append_cols = 'ID')
names(extr.dat) <- c("ID", "Moisture")
occ.buffer <- occ.buffer %>% left_join(extr.dat, join_by(ID)) # join back to df


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 3.4 Effort  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# load all bird observations from skane
birds.skane <- readRDS("data/occ.data.all.skane.rds")

# load package to count observations per grid cell
# library(sfhotspot) # too slow!

occ.buffer$Effort <- lengths(st_intersects(occ.buffer.landscape, birds.skane))  # count all observations 100ha around the presence

# rm all skane bird observations again to save memory
rm(birds.skane)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 3.4 Climate data  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# load libraries for data download 
# library(sdmpredictors)
# library(geodata)


# sdmpredictors::get_layers_info() 
# clim <- geodata::worldclim_global(var = "tavg", res = 10, country = "SE")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 3.5 Elevation and Slope data  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4. Export data ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

saveRDS(occ.buffer, "data/modelling.data.rds") # this is all data ready for modelling
rm(list = ls()[!grepl( "occ.buffer", x = ls())]) # clear workspace

