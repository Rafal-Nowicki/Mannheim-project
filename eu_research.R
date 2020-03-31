#ENVIORNMENT PREPARATION
rm(list = ls())
if (!is.null(dev.list())) dev.off()
cat("\014")

## APIs libraries ##
library(eurostat)
library(bdl)

library(rgdal)
library(sp)
library(spdep)
library(tidyverse)
library(ggcorrplot)
library(latticeExtra)
library(gridExtra)

`%notin%` <- Negate(`%in%`)

#Unemployment in 20-64 age group
unemp <- get_eurostat("lfst_r_lfu3rt") %>%
  subset(grepl("(DE|AT|FR|NL|BE)([0-9]|[A-Z]){2}", geo)) %>%
  filter(time == "2018-01-01", unit == "PC", sex == "T", age == "Y20-64") %>%
  transmute(geo, unemployment = values)

#Disposable income per capita PPS
hh_inc <- get_eurostat("nama_10r_2hhinc") %>%
  subset(grepl("(DE|AT|FR|NL|BE)([0-9]|[A-Z]){2}", geo)) %>%
  filter(time == "2017-01-01", unit == "PPS_HAB", na_item == "B6N") %>%
  transmute(geo, household_income = values)

#Gross Value Added Real Growth Rate
gva_grwth <- get_eurostat("nama_10r_2gvagr") %>%
  subset(grepl("(DE|AT|FR|NL|BE)([0-9]|[A-Z]){2}", geo)) %>%
  filter(time == "2018-01-01", unit == "PCH_PRE") %>%
  transmute(geo, gva_growth = values)

# Research and Developement Expenditures (euro per inhabitant)
res_dev <- get_eurostat("rd_e_gerdreg") %>%
  subset(grepl("(DE|AT|FR|NL|BE)([0-9]|[A-Z]){2}", geo)) %>%
  filter(time == "2013-01-01", sectperf == "TOTAL", unit == "EUR_HAB") %>%
  transmute(geo, res_dev = values)

# Percent of people aged 20-64 with tertiary degree ISCED 5-8
tert <- get_eurostat("edat_lfse_04") %>%
  subset(grepl("(DE|AT|FR|NL|BE)([0-9]|[A-Z]){2}", geo)) %>%
  filter(time == "2018-01-01", sex == "T", isced11 == "ED5-8") %>%
  transmute(geo, tert = values)

# Total fertility rate
fert <- get_eurostat("demo_r_frate2") %>%
  subset(grepl("(DE|AT|FR|NL|BE)([0-9]|[A-Z]){2}", geo)) %>%
  filter(time == "2018-01-01", age == "TOTAL", unit == "NR") %>%
  transmute(geo, fert = values)

df <- unemp %>%
  inner_join(hh_inc, by = "geo") %>%
  left_join(gva_grwth, by = "geo") %>%
  left_join(res_dev, by = "geo") %>%
  left_join(tert, by = "geo") %>% 
  left_join(fert, by = "geo")
  

map <- get_eurostat_geospatial(output_class = "spdf",
                               resolution = "60", nuts_level = "2")


map <- map[map@data$CNTR_CODE %in% 
             c("DE","NL","AT","BE", "FR"), ] #choosing countries of interest (WUE)

map <- map[map@data$id %notin% 
             c("FRY1", "FRY2", "FRY3", "FRY4", "FRY5", "FRM0"),] #excluding french islands
plot(map)


spatial_data <- merge(y = df, x = map, by.y = "geo", by.x = "NUTS_ID")

pal <- colorRampPalette(c("red","white" ,"green"))

hh_plot <- spplot(spatial_data, zcol = "household_income", colorkey = TRUE, col.regions = pal(100), cuts = 99,
       par.settings = list(axis.line = list(col =  'transparent')),
       main = "Disposable household income in PPS") +
  layer_(sp.polygons(spatial_data, fill='grey'))


unempt_plot <- spplot(spatial_data, zcol = "unemployment", colorkey = TRUE, col.regions = pal(100), cuts = 99,
       par.settings = list(axis.line = list(col =  'transparent')),
       main = "Unemployment in age group 20-64") +
  layer_(sp.polygons(spatial_data, fill='grey'))


res_dev_plot <- spplot(spatial_data, zcol = "res_dev", colorkey = TRUE, col.regions = pal(100), cuts = 99,
       par.settings = list(axis.line = list(col =  'transparent')),
       main = "R&D expenditures per inhabitant") +
  layer_(sp.polygons(spatial_data, fill='grey'))

tert_plot <- spplot(spatial_data, zcol = "tert", colorkey = TRUE, col.regions = pal(100), cuts = 99,
                       par.settings = list(axis.line = list(col =  'transparent')),
                       main = "tertiary attachement in age group 20-64") +
  layer_(sp.polygons(spatial_data, fill='grey'))


gva_grwth_plot <- spplot(spatial_data, zcol = "gva_growth", colorkey = TRUE, col.regions = pal(100), cuts = 99,
                    par.settings = list(axis.line = list(col =  'transparent')),
                    main = "GVA real growth rate") +
  layer_(sp.polygons(spatial_data, fill='grey'))

fert_plot <- spplot(spatial_data, zcol = "fert", colorkey = TRUE, col.regions = pal(100), cuts = 99,
                    par.settings = list(axis.line = list(col =  'transparent')),
                    main = "total fertility rate") +
  layer_(sp.polygons(spatial_data, fill='grey'))



grid.arrange(hh_plot, unempt_plot, res_dev_plot, tert_plot, gva_grwth_plot, fert_plot)


model_eu <- lm(data = spatial_data@data, household_income ~ unemployment + tert + fert )
summary(model_eu)

spatial_data$res <- model_eu$residuals

res_plot <- spplot(spatial_data, zcol = "res", colorkey = TRUE, col.regions = pal(100), cuts = 99,
       par.settings = list(axis.line = list(col =  'transparent')),
       main = "Residuals from regression")


### Weight matrices ###
centroids <- coordinates(spatial_data)

cont <- poly2nb(spatial_data, queen = T)
W_list <- nb2listw(cont, style = "W", zero.policy = T) # row normalization
W <- listw2mat(W_list)
plot.nb(cont, centroids, pch = 16, col = "grey")

# Spatial dependency test

moran.test(spatial_data@data$res, W_list)
moran.plot(spatial_data@data$res, W_list)
