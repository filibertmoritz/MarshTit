##### Marsh tit distribution modelling, script that brings all other scripts together  ####
##### Script written by Filibert Heim, filibert.heim@posteo.de, in March 2026 ####


# step 1: get presence observations from GBIF - do only once!
# source("scripts/MarshTit_modelling_get_data.R")

# step 2: set basic variables and decisons (study period, breeding period, territory size, etc)
source("scripts/MarshTit_modelling_basic_variables.R")

# setp 3: clean presence-only data and create presence-absence data
# here, presence data is filtered for study period, breeding season and study area
source("scripts/MarshTit_modelling_prep_occ_data.R")

# step 4: get environmental variables (LiDAR, NMD land use, NMD moisture) for occurence locations 
source("scripts/MarshTit_modelling_prep_envCovs.R") # for occurrence locations, and model training
source("scripts/MarshTit_modelling_get_LiDAR_data.R") # for study area, and sdm map prediction

# step 5: model species distribution 
# source("scripts/MarshTit_modelling_built_model.R")

# step 6: visualisations 


