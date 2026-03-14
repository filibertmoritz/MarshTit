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
# library(partykit)  # for cond random forest
# library(party) # for c_forest-control

# built formula
# formula.all <- formula(paste("occ ~ " ,paste(names(data)[!names(data) %in% c("ID", "occ")], collapse =  "+")))
# ntree <- 2000

# fit conditional random forest
# rf <- cforest(formula =  formula.all, 
#        data = data %>%
#          group_by(occ) %>%
#          slice_sample(n = length(data$occ[data$occ == 1]))), # this samples away half of the absences for balanced data set for rf
#        controls = cforest_control(ntree = ntree, mtry = length(attr(terms(formula.all), "term.labels"))/2)) # calculates number of predictors and divides by 2


# calculate conditional variable importance 10 times
# iter <- 1
# rf.vip <- data.frame(iter = numeric(), variable = character(), impoertance = numeric())

# call loop
# for(i in 1:iter){
#   v <- party::varimp(rf, conditional = T)
#   r <- data.frame(iter = i, variable = names(v), importance = v)
#   rf.vip <- rbind(results_vip, r)
# }


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4. built glm and perform model selection ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# scale all numeric predictor variables and store with a scaled prefix (ChatGPT help for shortening code)
data <- data %>%
  mutate(across(where(is.numeric), ~ as.numeric(scale(.x)),
                .names = "{.col}.scaled"))


# model selection 
# 1) preferred forest habitat: either all decidous + decidous-mixed with evergreen OR only decidous without mixed evergreen
formulas <- list(nullmodel = formula(occ ~ 1),
                 NMD.Deciduous = formula(occ ~ NMD.Deciduous.scaled),
                 NMD.Mixed.forest = formula(occ ~ NMD.Mixed.forest.scaled), 
                 NMD.Deciduous.hardwood = formula(occ ~ NMD.Deciduous.hardwood.scaled), 
                 NMD.Hardwood = formula(occ ~ NMD.Hardwood.scaled),
                 NMD.Spruce = formula(occ ~ NMD.Spruce.scaled), 
                 NMD.Pine = formula(occ ~ NMD.Pine.scaled), 
                 NMD.Non.evergreen.mon = formula(occ ~ NMD.Non.evergreen.mon.scaled), 
                 NMD.Non.evergreen = formula(occ ~ NMD.Non.evergreen.scaled))

models <- list()
for(f in names(formulas)){
  models[[f]] <- glm(formula = formulas[[f]], 
                         family = binomial(link = "logit"), 
                         data = data) 
}

aictable <- AICcmodavg::aictab(models)

# 2) preferred habitat structure from lidar data 
formulas <- c(formulas, list(`NMD.Deciduous * LiDAR.bottom` = formula(occ ~ NMD.Deciduous.scaled * LiDAR.bottom.scaled),
                             `NMD.Deciduous * LiDAR.lower` = formula(occ ~ NMD.Deciduous.scaled * LiDAR.lower.scaled), 
                             `NMD.Deciduous * LiDAR.upper` = formula(occ ~ NMD.Deciduous.scaled * LiDAR.upper.scaled), 
                             `NMD.Deciduous * LiDAR.top` = formula(occ ~ NMD.Deciduous.scaled * LiDAR.top.scaled), 
                             `NMD.Deciduous * canopy.height` = formula(occ ~ NMD.Deciduous.scaled * canopy.height.scaled)))

models <- list()
for(f in names(formulas)){
  models[[f]] <- glm(formula = formulas[[f]], 
                     family = binomial(link = "logit"), 
                     data = data) 
}

aictable <- AICcmodavg::aictab(models)

# 3) preferred moisture with and without quadratic effect
formulas <- c(formulas, list(`NMD.Deciduous * LiDAR.top + Moisture` = formula(occ ~ NMD.Deciduous.scaled * LiDAR.top.scaled + Moisture.scaled),
                             `NMD.Deciduous * LiDAR.top + Moisture2` = formula(occ ~ NMD.Deciduous.scaled * LiDAR.top.scaled + I(Moisture.scaled^2))))

models <- list()
for(f in names(formulas)){
  models[[f]] <- glm(formula = formulas[[f]], 
                     family = binomial(link = "logit"), 
                     data = data) 
}

aictable <- AICcmodavg::aictab(models)

# 4) do marsh tits avoid open lands or man-made infrastructure/open-lands or both?
formulas <- c(formulas, list(`NMD.Deciduous * LiDAR.top + Moisture2 + NMD.Open.lands` = formula(occ ~ NMD.Deciduous.scaled * LiDAR.top.scaled + I(Moisture.scaled^2) + NMD.Open.lands.scaled), 
                             `NMD.Deciduous * LiDAR.top + Moisture2 + NMD.Open.artificial` = formula(occ ~ NMD.Deciduous.scaled * LiDAR.top.scaled + I(Moisture.scaled^2) + NMD.Open.artificial.scaled), 
                             `NMD.Deciduous * LiDAR.top + Moisture2 + NMD.Open.lands + NMD.Open.artificial` = formula(occ ~ NMD.Deciduous.scaled * LiDAR.top.scaled + I(Moisture.scaled^2) + NMD.Open.lands.scaled + NMD.Open.artificial.scaled)))

models <- list()
for(f in names(formulas)){
  models[[f]] <- glm(formula = formulas[[f]], 
                     family = binomial(link = "logit"), 
                     data = data) 
}

aictable <- AICcmodavg::aictab(models)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4. Extract data from modelling and best model ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# AICc table for all models: 
aictable.pretty <- aictable %>% 
  tibble() %>%
  rename(`Number of model parameters` = K, `Delta AICc` = Delta_AICc, `AICc weights` = AICcWt, Model = Modnames) %>% 
  mutate(across(c(AICc, `Delta AICc`, `AICc weights`), ~ round(.x, 2))) %>% 
  select(-LL, -Cum.Wt) %>% 
  kable()


# extract best model
best.model <- models[[aictable[1,1]]]


# perform model diagnostics using DHARMa 
plot(simulateResiduals(best.model)) # looks alright considering small sample size and sensitivity of DHARMa's diagnostic 
testDispersion(simulateResiduals(best.model,re.form = NULL)) # re.form needed for conditional simulations, as recommended in https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html, 1.1 is alright - no overdispersion
# looks good, no obvious violations of model assumptions!

s <- summary(best.model)
s

# effect size table 
effect.sizes <- tibble(Parameters =  str_remove(rownames(s$coefficients), 
                                                pattern = '\\.scaled') ,
                       `Parameter estimates` = exp(s$coefficients[,1]), 
                       `2.5% confidence level` = exp(s$coefficients[,1] - s$coefficients[,2]), 
                       `97.5% confidence level` = exp(s$coefficients[,1] + s$coefficients[,2]),
                       `P-value` = s$coefficients[,4]) %>% 
  mutate(across(c('Parameter estimates', '2.5% confidence level',
                  '97.5% confidence level', 'P-value'), ~ round(.x, 2)))
effect.sizes.pretty <- effect.sizes %>%
  kable()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 5. Produce marginal effect plots ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# make data for prediction
n = 1000
make.seq <- function(x, n = 500) { # define a function to create a seq of data
  seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = n)
}

new.data <- data.frame(
  NMD.Deciduous.scaled = make.seq(data$NMD.Deciduous.scaled, n = n),
  NMD.Deciduous = make.seq(data$NMD.Deciduous, n = n),
  LiDAR.top.scaled = make.seq(data$LiDAR.top.scaled, n = n),
  LiDAR.top = make.seq(data$LiDAR.top, n = n),
  Moisture = make.seq(data$Moisture, n = n),
  Moisture.scaled = make.seq(data$Moisture.scaled, n = n),
  NMD.Open.lands = make.seq(data$NMD.Open.lands, n = n),
  NMD.Open.lands.scaled = make.seq(data$NMD.Open.lands.scaled, n = n),
  NMD.Open.artificial = make.seq(data$NMD.Open.artificial, n = n), 
  NMD.Open.artificial.scaled = make.seq(data$NMD.Open.artificial.scaled, n = n))

predictors <- names(new.data)[grep('scaled', x = names(new.data))]
# predictors <- predictors[!grepl('effort.scaled', x = predictors)] # remove effort

plotting.data <- data.frame(predictor = NULL, 
                            predictor.scaled = NULL, 
                            pred = NULL)

for(p in predictors){
  nd <- new.data %>% mutate(across(.cols = !all_of(p), ~0)) # set all other predictors to 0 - mean() of z transformed data
  pred <- predict(best.model, newdata = nd , se.fit = T, type = "link") # make prediction
  
  # gather data in one df
  dat <- data.frame(predictor = str_remove(p, pattern = '\\.scaled'), 
                    predictor.unscaled = 
                      new.data[[str_remove(p, pattern = '\\.scaled')]],
                    predictor.scaled = new.data[[p]], 
                    pred = plogis(pred$fit), 
                    # pred.se = exp(pred$se.fit), 
                    pred.upper = plogis(pred$fit + pred$se.fit*1.96),
                    pred.lower = plogis(pred$fit - pred$se.fit*1.96))
  # bringd ata together
  plotting.data <- bind_rows(plotting.data, dat)
}



# plot the marginal effects 
plot <- plotting.data %>% 
  mutate(predictor = if_else(predictor == 'land.use','land use', predictor)) %>% 
  ggplot() + 
  geom_line(mapping = aes(y = pred, x = predictor.unscaled, col = predictor),lwd = 1.2) + 
  geom_ribbon(mapping = aes(ymin = pred.lower, ymax = pred.upper, 
                            x = predictor.unscaled, fill = predictor), alpha = 0.4) + 
  scale_fill_discrete(name= 'Units of the Predictors', 
                      labels =  c('LiDAR top quarter', 
                                  'Soil moisture', 
                                  'Fraction of Decidous forest', 
                                  'Fraction of anthropogenic open land', 
                                  "Fraction of `natural` open land")) +
  scale_colour_discrete(name= 'Units of the Predictors', 
                        labels =  c('LiDAR top quarter', 
                                    'Soil moisture', 
                                    'Fraction of Decidous forest', 
                                    'Fraction of anthropogenic open land', 
                                    "Fraction of `natural` open land")) +
  facet_wrap(~predictor, scales = 'free_x') +
  labs(x = 'Predictor (for unit see legend)', y ='Predicted Occupancy') +
  theme_bw(base_size = 12) +
  theme(legend.position = c(0.85,0.2))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 6. Produce prediction map ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# load data for prediction 
data.map <- readRDS("data/prediction.data.sdm.map.rds")

# calculate the same predictors as for buffer training data 
data.map <- data.map %>%
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

# get sd and mean from training data set 
map.preds <- sub(x = predictors, pattern = ".scaled", replacement = "")
scale.values <- data.frame(predictor = character(), mean = numeric(), sd = numeric())
for(p in map.preds){
  m <- mean(data[[p]], na.rm = T)
  sd <- sd(data[[p]], na.rm =T)
  res <- data.frame(predictor = p, mean = m, sd = sd)
  scale.values <- bind_rows(scale.values, res)
  rm(res)
}

# call loop to finally scale all variables and store in new columns 
for(i in 1:nrow(scale.values)){
  # get values from scaled values df
  predictor <- scale.values$predictor[i]
  mean <- scale.values$mean[i]
  sd  <- scale.values$sd[i]
  #actual scaling
  data.map[[paste0(predictor, ".scaled")]] <- (data.map[[predictor]] - mean) / sd
}

# finally make prediction 
data.map$predicted <- predict(best.model, newdata = data.map , se.fit = T, type = "response")$fit

plot(data.map %>% select(predicted), main = "Predicted Marsh Tit Occupancy in Scania")













# dredge(global.model = glm())

# fit models for first step light 
# models <- list()
# for(f in names(formulas)){
#   models[[f]] <- glmmTMB(formula = formulas[[f]], 
#                          family = nbinom1(link = 'log'), 
#                          data = data) 
# }

# compare models using AIC
# aictable <- AICcmodavg::aictab(models) 
# m <- glm(formula = occ ~ Moisture.scaled + LiDAR.bottom.scaled*NMD.Deciduous.dry.scaled + NMD.Building.scaled, 
#          data = data , family = "binomial")




# formula.all <- formula(occ ~ paste0(names(occ.data)[8:39], collapse =  "*"))

# m <- glm(formula = formula.all, family = "binomial", data = occ.data)



# global.formula <- formula(paste("occ ~", paste(setdiff(names(data %>% select(-matches("\\.scaled$"))), c("ID", "occ")), collapse = "+")))
# global.model <- glm(formula = global.formula, data = data, family = "binomial", na.action = "na.fail")
# d <- dredge(global.model = global.model, rank = "AICc")


