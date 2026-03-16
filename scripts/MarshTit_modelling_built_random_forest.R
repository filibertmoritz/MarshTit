##### Marsh tit distribution modelling, script xx to built model(s) ####
##### Script written by Filibert Heim, filibert.heim@posteo.de, in March 2026 ####


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. Preps ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# load packages 
library(tidyverse)
library(knitr)
library(sf)
library(corrplot)
library(DHARMa)
library(MuMIn)
select <- dplyr::select; filter <- dplyr::filter; rename <- dplyr::rename

# load basic data (study area etc)
load("data/basic.variables.RData")

# load prepared presence-absence data and envCovs
occ.buffer <- readRDS("data/modelling.data.rds") # bufferes around the points
occ.data <- readRDS("data/occ.data.rds") # point observations 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. check correlations ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# check for correlations
cor.mat <- cor(occ.buffer[,8:39] %>% st_drop_geometry(), use = "pairwise.complete.obs")

corrplot(cor.mat, method = "color", type = "lower", tl.cex = 0.7, tl.col = "black", tl.srt = 45)


high_cor <- cor.mat %>%
  as.data.frame() %>%
  tibble::rownames_to_column("var1") %>%
  pivot_longer(-var1, names_to = "var2", values_to = "correlation") %>%
  filter(var1 != var2) %>%          # remove self-correlations
  filter(abs(correlation) > 0.7) %>% 
  distinct(correlation, .keep_all = T)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3. clean up data and calculate predictor variables ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# remove unneeded columns
data <- occ.buffer %>% 
  select(-stateProvince, -date, -collectionCode, -coordinateUncertaintyInMeters, -individualCount) %>% 
  st_drop_geometry() %>% drop_na()

# pool wet and dry forest types 
data <- data %>%
  mutate(NMD.Deciduous = NMD.Deciduous.dry + NMD.Deciduous.wet,
         NMD.Hardwood = NMD.Hardwood.dry + NMD.Hardwood.wet,
         NMD.Deciduous.hardwood = NMD.Deciduous.hardwood.dry + NMD.Deciduous.hardwood.wet,
         NMD.Pine = NMD.Pine.dry + NMD.Pine.wet,
         NMD.Spruce = NMD.Spruce.dry + NMD.Spruce.wet,
         NMD.Mixed.conifer = NMD.Mixed.conifer.dry + NMD.Mixed.conifer.wet,
         NMD.Mixed.forest = NMD.Mixed.forest.dry + NMD.Mixed.forest.wet,
         NMD.Temp.non.forest = NMD.Temp.non.nforest.dry + NMD.Temp.non.forest.wet, 
         NMD.Non.evergreen.mon = NMD.Deciduous + NMD.Hardwood + NMD.Deciduous.hardwood + NMD.Mixed.forest, 
         NMD.Non.evergreen = NMD.Deciduous + NMD.Hardwood + NMD.Deciduous.hardwood, 
         NMD.Open.lands = NMD.Veg.open + NMD.Non.veg.open, # bot equal to arable lands!
         NMD.Open.artificial = NMD.Building + NMD.Artificial.open + NMD.Road.rail) %>% 
  select(-matches("\\.dry$"), -matches("\\.wet$"))


# check for correlations
cor.mat <- cor(data %>% select(-ID, -occ))

corrplot(cor.mat, method = "color", type = "lower", tl.cex = 0.7, tl.col = "black", tl.srt = 45)


cor.mat %>%
  as.data.frame() %>%
  tibble::rownames_to_column("var1") %>%
  pivot_longer(-var1, names_to = "var2", values_to = "correlation") %>%
  filter(var1 != var2) %>%          # remove self-correlations
  filter(abs(correlation) > 0.7) %>% 
  distinct(correlation, .keep_all = T)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4. built conditional random forest ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# load required packages
library(partykit)  # for cond random forest
library(party) # for c_forest-control

# built formula
formula.all <- formula(paste("occ ~ " ,paste(names(data)[!names(data) %in% c("ID", "occ")], collapse =  "+")))
ntree <- 2000

# fit conditional random forest
rf <- cforest(formula =  formula.all, 
              data = data %>% group_by(occ) %>% slice_sample(n = length(data$occ[data$occ == 1])), # this samples away half of the absences for balanced data set for rf
              controls = cforest_control(ntree = ntree, mtry = length(attr(terms(formula.all), "term.labels"))/2)) # calculates number of predictors and divides by 2

# save rf object 
saveRDS(rf, "output/random.forest.rds")

# calculate conditional variable importance 10 times
iter <- 5
rf.vip <- data.frame(iter = numeric(), variable = character(), impoertance = numeric())

# call loop
for(i in 1:iter){
  v <- party::varimp(rf, conditional = T)
  r <- data.frame(iter = i, variable = names(v), importance = v)
  rf.vip <- rbind(rf.vip, r)
}

rf

## plot variable importance and save
plot <- results_vip %>% 
  ggplot() +
  geom_point(aes(x = reorder(Variable, mean_importance), y = importance, color = "Individual replicates"), alpha = 0.5, size = 1.5,
             position = position_jitter(width = 0.2)) +
  geom_segment(aes(x = reorder(Variable, mean_importance), xend = reorder(Variable, mean_importance),
                   y = 0, yend = mean_importance, color = "Mean importance"), linewidth = .9) +
  geom_point(aes(x = reorder(Variable, mean_importance), y = mean_importance, color = "Mean importance"), size = 1.8) +
  geom_hline(aes(yintercept = variation, color = "Random variation"), linetype = 2) +
  coord_flip() 
#  facet_grid(. ~ Model, scales = "free", switch = "y") + 
  scale_color_manual(name = "Legend",
                     values = c("Mean importance" = "firebrick", 
                                "Mean importance" = "firebrick", 
                                "Individual replicates" = "gray30",
                                "Random variation" = "grey50")) +
  labs(x = "Variable",
       y = "Relative Variable Importance",
       title = "Variable Importance") +
  theme_bw(base_size = 13) +
  theme(legend.background = element_rect(color = "black"), 
        legend.position = c(0.9, 0.15))





