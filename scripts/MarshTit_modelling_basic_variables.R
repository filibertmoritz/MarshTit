##### Marsh tit distribution modelling, script with basic decisions and  ####
##### Script written by Filibert Heim, filibert.heim@posteo.de, in March 2026 ####

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. basic decisons ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# habitat sizes: 35ha (birds of the world, Broughton et al. 2014, 2015)
buffer.size <- 367
grid.size <- 635.5

# breeding period: March-June (birds of the world, and Källander et al. 2017)
breeding.period <- 3:6

# study period: 2016 to 2026
study.period <- 2016:2026

# study crs: epsg 3006 (=SWEDEN99)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. basic geodata ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# load required shp files 
skane <- st_read(dsn = 'data/vect.data/Skane_vect.shp') %>% st_transform(crs = 3006) #  from geodata::gadm(country = "Sweden", level = 0) and 1

# define study area
study.area <- skane; rm(skane)
study.area.buffer <- st_buffer(study.area, dist = 1000) # create a buffer for cropping of rasters 

# define grid for study area 
study.area.grid <- study.area %>% 
  st_make_grid(cellsize = grid.size, square = F) %>% # leads to a cellsize of ~30 ha (home range size)
  st_intersection(study.area) %>% 
  st_sf() %>% 
  filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) %>% # to avoid issues with exact_extract
  mutate(CellID = row_number())

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3. export for ease of use ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

save.image("data/basic.variables.RData")
rm(list = ls()) # clear workspace
