##### Marsh tit distribution modelling, script 3 to prepare Land-cover class labels in english ####
##### Script written by Filibert Heim, filibert.heim@posteo.de, in March 2026 ####


english.labels <- c(
  "0" = "Outside mapping area",
  "2" = "Open wetland",
  "3" = "Arable land",
  "41" = "Non-vegetated other open land",
  "42" = "Vegetated other open land",
  "51" = "Artificial surfaces, building",
  "52" = "Artificial surfaces, not building or road/railway",
  "53" = "Artificial surfaces, road/railway",
  "61" = "Inland water",
  "62" = "Marine water",
  "111" = "Pine forest not on wetland",
  "112" = "Spruce forest not on wetland",
  "113" = "Mixed coniferous forest not on wetland",
  "114" = "Mixed forest not on wetland",
  "115" = "Deciduous forest not on wetland",
  "116" = "Deciduous hardwood forest not on wetland",
  "117" = "Deciduous forest with deciduous hardwood forest not on wetland",
  "118" = "Temporarily non-forest not on wetland",
  "121" = "Pine forest on wetland",
  "122" = "Spruce forest on wetland",
  "123" = "Mixed coniferous forest on wetland",
  "124" = "Mixed forest on wetland",
  "125" = "Deciduous forest on wetland",
  "126" = "Deciduous hardwood forest on wetland",
  "127" = "Deciduous forest with deciduous hardwood forest on wetland",
  "128" = "Temporarily non-forest on wetland")

short.labels <- c(
  "0" = "Outside area",
  "2" = "Open wetland",
  "3" = "Arable",
  "41" = "Non-veg open",
  "42" = "Veg open",
  "51" = "Building",
  "52" = "Artificial open",
  "53" = "Road/rail",
  "61" = "Inland water",
  "62" = "Marine water",
  "111" = "Pine dry",
  "112" = "Spruce dry",
  "113" = "Mixed conifer dry",
  "114" = "Mixed forest dry",
  "115" = "Deciduous dry",
  "116" = "Hardwood dry",
  "117" = "Deciduous+hardwood dry",
  "118" = "Temp non-forest dry",
  "121" = "Pine wet",
  "122" = "Spruce wet",
  "123" = "Mixed conifer wet",
  "124" = "Mixed forest wet",
  "125" = "Deciduous wet",
  "126" = "Hardwood wet",
  "127" = "Deciduous+hardwood wet",
  "128" = "Temp non-forest wet")

# create df which contains the labels 
labels.eng <- data.frame(ID = as.numeric(names(short.labels)), 
           Klass.eng =english.labels, 
           Klass.eng.short = short.labels)
