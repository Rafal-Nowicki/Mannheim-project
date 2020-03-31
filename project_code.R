#ENVIORNMENT PREPARATION
rm(list = ls())
if (!is.null(dev.list())) dev.off()
cat("\014")

library(eurostat)
library(bdl)

library(rgdal)
library(sp)
library(spdep)
library(tidyverse)
library(ggcorrplot)

#### EUROSTAT API ####
nuts2 <- search_eurostat("NUTS 2") %>%
  filter(`data end` >= 2017) %>%
  arrange(desc(`last update of data`))

# GDP at current market prices, (in PPS) per inhabitant
gdp <- get_eurostat("nama_10r_2gdp") %>%
  subset(grepl("DE([0-9]|[A-Z]){2}", geo)) %>%
  filter(time == "2018-01-01", unit == "PPS_HAB") %>%
  transmute(geo, gdp = values)

#Unemployment in 20-64 age group
unemp <- get_eurostat("lfst_r_lfu3rt") %>%
  subset(grepl("DE([0-9]|[A-Z]){2}", geo)) %>%
  filter(time == "2018-01-01", unit == "PC", sex == "T", age == "Y20-64") %>%
  transmute(geo, unemployment = values)

#Disposable income per capita PPS
hh_inc <- get_eurostat("nama_10r_2hhinc") %>%
  subset(grepl("DE([0-9]|[A-Z]){2}", geo)) %>%
  filter(time == "2017-01-01", unit == "PPS_HAB", na_item == "B6N") %>%
  transmute(geo, household_income = values)

#Gross Value Added Real Growth Rate
gva_grwth <- get_eurostat("nama_10r_2gvagr") %>%
  subset(grepl("DE([0-9]|[A-Z]){2}", geo)) %>%
  filter(time == "2018-01-01", unit == "PCH_PRE") %>%
  transmute(geo, gva_growth = values)

# Research and Developement Expenditures (euro per inhabitant)
res_dev <- get_eurostat("rd_e_gerdreg") %>%
  subset(grepl("DE([0-9]|[A-Z]){2}", geo)) %>%
  filter(time == "2017-01-01", sectperf == "TOTAL", unit == "EUR_HAB") %>%
  transmute(geo, res_dev = values)

# Percent of people aged 20-64 with tertiary degree ISCED 5-8
tert <- get_eurostat("edat_lfse_04") %>%
  subset(grepl("DE([0-9]|[A-Z]){2}", geo)) %>%
  filter(time == "2018-01-01", sex == "T", isced11 == "ED5-8") %>%
  transmute(geo, tert = values)

# Total fertility rate
fert <- get_eurostat("demo_r_frate2") %>%
  subset(grepl("DE([0-9]|[A-Z]){2}", geo)) %>%
  filter(time == "2018-01-01", age == "TOTAL", unit == "NR") %>%
  transmute(geo, fert = values)

# Average number of usual weekly hours of work in main job for people aged 20-64
work_h <- get_eurostat("lfst_r_lfe2ehour") %>%
  subset(grepl("DE([0-9]|[A-Z]){2}", geo)) %>%
  filter(time == "2018-01-01", sex == "T", age == "Y20-64", unit == "HR") %>%
  transmute(geo, work_h = values)


df <- gdp %>%
  inner_join(hh_inc, by = "geo") %>%
  inner_join(unemp, by = "geo") %>%
  inner_join(gva_grwth, by = "geo") %>%
  inner_join(res_dev, by = "geo") %>%
  inner_join(tert, by = "geo") %>%
  inner_join(fert, by = "geo") %>%
  inner_join(work_h, by = "geo")


cor(df[,-1]) %>%
  ggcorrplot(hc.order = T, type = "lower", lab = T)

# map preparing
map <- get_eurostat_geospatial(output_class = "spdf", resolution = "60", nuts_level = "2")

map <- map[map@data$CNTR_CODE %in% "DE", ]

#spatial data merging

spatial_data <- merge(y = df, x = map, by.y = "geo", by.x = "NUTS_ID")

# linear model for Germany #

model_germany <- lm(data = spatial_data@data, household_income ~ unemployment  + res_dev  + gva_growth)
summary(model_germany)


spatial_data$res <- model_germany$residuals


#### DE MAP NUTS2 ####
pal <- colorRampPalette(c("red","white" ,"green"))

spplot(spatial_data, zcol = "res", colorkey = TRUE, col.regions = pal(100), cuts = 99,
       par.settings = list(axis.line = list(col =  'transparent')),
       main = "Residuals from regression")

### Weight matrices ###
centroids <- coordinates(spatial_data)

cont <- poly2nb(spatial_data, queen = T)
W_list <- nb2listw(cont, style = "W") # row normalization
W <- listw2mat(W_list)
plot.nb(cont, centroids, pch = 7)

# Spatial dependency test

moran.test(spatial_data@data$res, W_list)
moran.plot(spatial_data@data$res, W_list)


loc_mor <- localmoran(spatial_data@data$res, W_list, p.adjust.method = "bonferroni")
spatial_data@data$loc_mor_i <- loc_mor[,1]
spatial_data@data$loc_mor_pv <- loc_mor[,5]


spplot(spatial_data, zcol = "loc_mor_pv", colorkey = TRUE, col.regions = pal(100), cuts = 99,
       par.settings = list(axis.line = list(col =  'transparent')),
       main = "LISA bonferroni p-value")


#### BDL API ####
#search_variables("bezrobocia") %>%
#  View()

get_variables("P2497")

wages_pow <- get_data_by_variable("64428", unitLevel = "5", year = 2017)

get_variables("P2392")

unemp_pow <- get_data_by_variable("60270", unitLevel = "5", year = 2017)

#search_variables("uczelnie") %>%
#  View()







