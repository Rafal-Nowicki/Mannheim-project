rm(list = ls())
if(!is.null(dev.list())) dev.off()
cat("\014")

library(eurostat)
library(rgdal)
library(sp)
library(spdep)
library(tidyverse)

search_eurostat("unemployment") %>%
  arrange(desc(`last update of data`)) %>%
  head(10)


gdp <- get_eurostat("nama_10r_2gdp") %>%
  subset(grepl("DE([0-9]|[A-Z]){2}", geo))%>%
  filter(time == "2017-01-01", unit == "EUR_HAB")%>%
  transmute(geo , gdp = values)

unemp <- get_eurostat("lfst_r_lfu3rt") %>%
  subset(grepl("DE([0-9]|[A-Z]){2}", geo))%>%
  filter(time == "2017-01-01", unit == "PC", sex == "T", age == "Y15-74") %>%
  transmute(geo, unemployment = values)

hh_inc <- get_eurostat("nama_10r_2hhinc") %>%
  subset(grepl("DE([0-9]|[A-Z]){2}", geo))%>%
  filter(time == "2017-01-01", unit == "EUR_HAB", na_item == "B5N")%>%
  transmute(geo, houshold_income = values)

gva_grwth <- get_eurostat("nama_10r_2gvagr")%>%
  subset(grepl("DE([0-9]|[A-Z]){2}", geo))%>%
  filter(time == "2017-01-01", unit == "PCH_PRE")%>%
  transmute(geo, gva_growth = values)


df <- gdp %>%
  inner_join(hh_inc, by = "geo") %>%
  inner_join(unemp, by = "geo") %>%
  inner_join(gva_grwth, by = "geo")


map <- readOGR(".",'NUTS_RG_10M_2016_4326_LEVL_2') %>%
  spTransform("+proj=longlat")

map <-map[map@data$CNTR_CODE %in% "DE",]

plot(map)

summary(lm(data = df, houshold_income ~ unemployment))

tertiary <- search_eurostat("tertiary") %>%
  arrange(desc(`last update of data`))


