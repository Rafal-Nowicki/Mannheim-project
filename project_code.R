rm(list = ls())
if (!is.null(dev.list())) dev.off()
cat("\014")

library(eurostat)
library(bdl)

library(rgdal)
library(sp)
library(spdep)
library(tidyverse)

#### EUROSTAT API ####


search_eurostat("unemployment") %>%
  arrange(desc(`last update of data`)) %>%
  head(10)


tertiary <- search_eurostat("tertiary") %>%
  arrange(desc(`last update of data`))

idx_t <- which(grepl("NUTS2", tertiary$title))

tertiary[idx_t,] %>%
  View()

tertiary_ratio <- get_eurostat("educ_uoe_enrt06") %>%
  subset(grepl("DE([0-9]|[A-Z]){2}", geo))


gdp <- get_eurostat("nama_10r_2gdp") %>%
  subset(grepl("DE([0-9]|[A-Z]){2}", geo)) %>%
  filter(time == "2017-01-01", unit == "EUR_HAB") %>%
  transmute(geo, gdp = values)

unemp <- get_eurostat("lfst_r_lfu3rt") %>%
  subset(grepl("DE([0-9]|[A-Z]){2}", geo)) %>%
  filter(time == "2017-01-01", unit == "PC", sex == "T", age == "Y15-74") %>%
  transmute(geo, unemployment = values)

hh_inc <- get_eurostat("nama_10r_2hhinc") %>%
  subset(grepl("DE([0-9]|[A-Z]){2}", geo)) %>%
  filter(time == "2017-01-01", unit == "EUR_HAB", na_item == "B5N") %>%
  transmute(geo, household_income = values)

gva_grwth <- get_eurostat("nama_10r_2gvagr") %>%
  subset(grepl("DE([0-9]|[A-Z]){2}", geo)) %>%
  filter(time == "2017-01-01", unit == "PCH_PRE") %>%
  transmute(geo, gva_growth = values)


df <- gdp %>%
  inner_join(hh_inc, by = "geo") %>%
  inner_join(unemp, by = "geo") %>%
  inner_join(gva_grwth, by = "geo")

#### DE MAP NUTS2 ####

map <- readOGR(".", "NUTS_RG_10M_2016_4326_LEVL_2") %>%
  spTransform("+proj=longlat")

map <- map[map@data$CNTR_CODE %in% "DE", ]

plot(map)

# linear model for Germany #

model_germany <- lm(data = df, household_income ~ unemployment + gva_growth)
summary(model_germany)

ger_res <- model_germany$residuals

df_res <- cbind(df, ger_res)
map$NUTS_ID

spatial_data <- merge(y = df_res, x = map, by.y = "geo", by.x = "NUTS_ID")

green_area <- rgb(24, 121, 104, 80, names = NULL, maxColorValue = 255)
pal <- colorRampPalette(c("white", green_area), bias = 1)
pal <- colorRampPalette(c("red", "green"))

spplot(spatial_data, zcol = "ger_res", colorkey = TRUE, col.regions = pal(100), cuts = 99,
       par.settings = list(axis.line = list(col =  'transparent')),
       main = "residuals from regression")


#### BDL API ####
search_variables("bezrobocia") %>%
  View()

get_variables("P2497")

wages_pow <- get_data_by_variable("64428", unitLevel = "5", year = 2017)

get_variables("P2392")

unemp_pow <- get_data_by_variable("60270", unitLevel = "5", year = 2017)

search_variables("uczelnie") %>%
  View()







