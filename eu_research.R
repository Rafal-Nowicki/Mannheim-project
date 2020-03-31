rm(list = ls())

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

df <- unemp %>%
  inner_join(hh_inc, by = "geo") 
  

map <- get_eurostat_geospatial(output_class = "spdf", resolution = "60", nuts_level = "2")


map <- map[map@data$CNTR_CODE %in% c("DE","NL","AT","BE", "FR"), ]
plot(map)
`%notin%` <- Negate(`%in%`)

map <- map[map@data$id %notin% c("FRY1", "FRY2", "FRY3", "FRY4", "FRY5", "FRM0"),]
plot(map)

spatial_data <- merge(y = df, x = map, by.y = "geo", by.x = "NUTS_ID")

pal <- colorRampPalette(c("red","white" ,"green"))

spplot(spatial_data, zcol = "household_income", colorkey = TRUE, col.regions = pal(100), cuts = 99,
       par.settings = list(axis.line = list(col =  'transparent')),
       main = "Residuals from regression")

spplot(spatial_data, zcol = "unemployment", colorkey = TRUE, col.regions = pal(100), cuts = 99,
       par.settings = list(axis.line = list(col =  'transparent')),
       main = "Residuals from regression")

dim(spatial_data@data)
model_eu <- lm(data = spatial_data@data, household_income ~ unemployment)
summary(model_eu)

length(model_eu$residuals)
spatial_data$res <- model_eu$residuals

spplot(spatial_data, zcol = "res", colorkey = TRUE, col.regions = pal(100), cuts = 99,
       par.settings = list(axis.line = list(col =  'transparent')),
       main = "Residuals from regression")


### Weight matrices ###
centroids <- coordinates(spatial_data)

cont <- poly2nb(spatial_data, queen = T)
W_list <- nb2listw(cont, style = "W", zero.policy = T) # row normalization
W <- listw2mat(W_list)
plot.nb(cont, centroids, pch = 7)

# Spatial dependency test

moran.test(spatial_data@data$res, W_list)
moran.plot(spatial_data@data$res, W_list)
