##### Marsh tit distribution modelling, script 2 to prepare LiDAR data ####
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
# 2. load and prepare vector data ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# load skane and create study area 
skane <- st_read(dsn = 'data/vect.data/Skane_vect.shp') %>% st_transform(crs = 3006)
study.area <- skane
study.area.buffer <- st_buffer(study.area, dist = 1000)
study.area.grid <- study.area.buffer %>% 
  st_make_grid(cellsize = 500, square = F) %>% 
  st_intersection(study.area.buffer) %>% 
  st_sf() %>% 
  filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) %>% # to avoid issues with exact_extract
  mutate(CellID = row_number())

# get all rasters 
rast.files <- list.files(path = "data/raster.data/")
lidar.files <- rast.files[grep(x = rast.files, pattern = "na.tif")]
lidar.names <- c("LiDAR.bottom", "LiDar.lower", "LiDAR.upper", "LiDAR.top", "canopy.height")


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
  r <- terra::crop(x =r, y = study.area.buffer) # crop to study area to avoid memory issues 
  
  # extract area-weighted means 
  extr.dat <- exact_extract(r, study.area.grid, fun = "mean", weights = "area", append_cols = "CellID")
  names(extr.dat)[2] <- lidar.names[i]
  
  # join back to grid to store extracted data 
  study.area.grid <- study.area.grid %>% 
    left_join(extr.dat, join_by(CellID))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 3.2 Land cover data  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Source: NMD basic 2023 version 2.0
# https://geodata.naturvardsverket.se/nedladdning/marktacke/NMD2023/Basskikt_v2_x/NMD2023_Produktbeskrivning_Basskikt_NMD2023_v2_x.pdf
# https://geodatakatalogen.naturvardsverket.se/geonetwork/srv/swe/catalog.search#/metadata/8853721d-a466-4c01-afcc-9eae57b17b39

# pre-process land cover data 
r <- terra::rast(x = "data/raster.data/NMD2023bas_v2_0.tif") # load .tif file
r <- terra::crop(r, study.area.buffer) # crop to study area
r <- terra::as.factor(r) # save as factorial values in raster 

# improve naming of the land-cover classes 
labels <- st_read("data/raster.data/NMD2023bas_v2_0.tif.vat.dbf") # get labels from .gdb file
source("scripts/MarshTit_modelling_Land_use_labels.R") # create labels in english in other script
labels <- labels %>% left_join(labels.eng, join_by(Value)) %>%
  rename(Klass.swe = Klass, ID = Value) %>% select(-Count) 
levels(r)[[1]] <- levels(r)[[1]] %>% left_join(labels, join_by(ID)) %>% select(-Layer_1, -Klass.swe, -Klass.eng) # set labels in levels of raster 

# extract data from raster 
extr.dat <- exactextractr::exact_extract(r, study.area.grid, fun = 'weighted_frac', weights = 'area', append_cols = 'CellID')
names(extr.dat) <- c('CellID', paste('NMD', levels(r)[[1]]$Klass.eng.short, sep = '.'))
names(extr.dat) <- sub(x = names(extr.dat),pattern = " ", replacement = ".") # make colnames prettier 
study.area.grid <- study.area.grid %>% left_join(extr.dat, join_by(CellID)) # join back to df

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 3.3 Climate data  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 3.4 Elevation and Slope data  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4. Save data  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

st_write(obj = study)

