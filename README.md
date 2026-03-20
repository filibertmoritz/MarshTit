# Marsh Tit Distribution Model for Scania, Sweden

We aim to understand the distribution and habitat requirements of **Marsh Tits** in Sweden, especially the southermost county Scania. Therefore, we employ breeding-season observations of **Marsh Tits** between 2016 and 2026, obtained from the GBIF database (mostly originating from Artportalen), in a presence-only (GLM) species distribution modelling approach. 

This project is part of the course BIOR83: 'Conservation Biology' at Lund University.  

To run the analysis, please run the [main analysis script](https://github.com/filibertmoritz/MarshTit](https://github.com/filibertmoritz/MarshTit/blob/main/scripts/MarshTit_modelling_main_script.R) which will run the required scripts in the adequate order:

1) Obtain presence-observations from GBIF
2) Set basic variables (to be called in each script)
3) Clean presence data and creating pseudo-absences
4) Extract environmental variables (LiDAR, land cover and moisture) for a) the presence-(pseudo)absence records, and b) whole Scania for later prediction
5) Built GLM and visualise results in marginal effect plots and predicted distribution. NB: There are two version of this script, for an analysis with and without consideration of observation bias or effort.

All output is saved in the [output folder](https://github.com/filibertmoritz/MarshTit/tree/main/output) including marginal effect plots and prediction maps of Marsh Tit occurence in Scania (also available as [.gpkg file](https://github.com/filibertmoritz/MarshTit/blob/main/output/Marh.tit.Occurence.Scania.predicted.effort.gpkg)).  
