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
library(mice)
library(latex2exp)

`%notin%` <- Negate(`%in%`)

### looking for the data for NUTS 2 regions###
nuts2 <- search_eurostat("NUTS 2") %>%
  filter(`data end` >= 2017) %>%
  arrange(desc(`last update of data`))

#demo_r_pjanind2 - some demografic variables (to consider)#

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
  filter(time == "2017-01-01", sectperf == "TOTAL", unit == "EUR_HAB") %>%
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

# Total number of households
hh_num_t <- get_eurostat("lfst_r_lfsd2hh") %>%
  subset(grepl("(DE|AT|FR|NL|BE)([0-9]|[A-Z]){2}", geo)) %>%
  filter(time == "2018-01-01", deg_urb == "TOTAL") %>%
  transmute(geo, hh_num_t = values)

# Number of households in the cities
hh_num_cities <- get_eurostat("lfst_r_lfsd2hh") %>%
  subset(grepl("(DE|AT|FR|NL|BE)([0-9]|[A-Z]){2}", geo)) %>%
  filter(time == "2018-01-01", deg_urb == "DEG1") %>%
  transmute(geo, hh_num_c = values)

# Number of households in the towns and suburbs
hh_num_suburbs <- get_eurostat("lfst_r_lfsd2hh") %>%
  subset(grepl("(DE|AT|FR|NL|BE)([0-9]|[A-Z]){2}", geo)) %>%
  filter(time == "2018-01-01", deg_urb == "DEG2") %>%
  transmute(geo, hh_num_s = values)

# Percent of households in the cieties, towns and suburbs
hh_urb_percent <- hh_num_t %>%
  inner_join(hh_num_cities, by = "geo") %>%
  inner_join(hh_num_suburbs, by = "geo") %>%
  transmute(geo,hh_urb_per = (hh_num_c + hh_num_s)/hh_num_t*100)

#Percent of employees employed in high-tech and knowledge intesive sectors (by NACE rev.2)
tech_emp_perc <- get_eurostat("htec_emp_reg2") %>%
  subset(grepl("(DE|AT|FR|NL|BE)([0-9]|[A-Z]){2}", geo)) %>%
  filter(time == "2018-01-01", sex == "T", unit == "PC_EMP", nace_r2 == "HTC") %>%
  transmute(geo, ht_perc = values)


### COMBINIG THE DATA ###

df <- unemp %>%
  inner_join(hh_inc, by = "geo") %>%
  left_join(gva_grwth, by = "geo") %>%
  left_join(res_dev, by = "geo") %>%
  left_join(tert, by = "geo") %>% 
  left_join(fert, by = "geo") %>%
  left_join(hh_urb_percent, by = "geo") %>%
  left_join(tech_emp_perc, by = "geo")

# MAP PREPARATION #

map <- get_eurostat_geospatial(output_class = "spdf",
                               resolution = "60", nuts_level = "2")


map <- map[map@data$CNTR_CODE %in% 
             c("DE","NL","AT","BE", "FR"), ] #choosing countries of interest (WUE)

map <- map[map@data$id %notin% 
             c("FRY1", "FRY2", "FRY3", "FRY4", "FRY5", "FRM0"),] #excluding french islands
plot(map)

# MERGING DATA WITH THE MAP
spatial_data <- merge(y = df, x = map, by.y = "geo", by.x = "NUTS_ID")



# SOME PLOTS
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


hh_urb_percent_plot <- spplot(spatial_data, zcol = "hh_urb_per", colorkey = TRUE, col.regions = pal(100), cuts = 99,
                         par.settings = list(axis.line = list(col =  'transparent')),
                         main = "percent of households in the urban areas") +
  layer_(sp.polygons(spatial_data, fill='grey'))

ht_emp_perc_plot <- spplot(spatial_data, zcol = "ht_perc", colorkey = TRUE, col.regions = pal(100), cuts = 99,
                           par.settings = list(axis.line = list(col =  'transparent')),
                           main = "percent of employees in high-tech industries") +
  layer_(sp.polygons(spatial_data, fill='grey'))


grid.arrange(hh_plot, unempt_plot, res_dev_plot, tert_plot,
             gva_grwth_plot, fert_plot, hh_urb_percent_plot, ht_emp_perc_plot, ncol = 2)


### LINEAR MODELS ###
model_eu <- lm(data = spatial_data@data, household_income ~ unemployment)
summary(model_eu)
length(model_eu$residuals)

model_eu2 <- lm(data = spatial_data@data, household_income ~ unemployment + hh_urb_per +
                  res_dev + gva_growth + ht_perc)
summary(model_eu2)
length(model_eu2$residuals)


#### NAs imputation
df_imputed <- mice(df[,-1], m=1, maxit = 50, method = 'pmm', seed = 500)
df_complete <- complete(df_imputed,1) %>%
  cbind(geo = df$geo)

### LINEAR MODELS on completed data ###
spatial_data_complete <- merge(y = df_complete, x = map, by.y = "geo", by.x = "NUTS_ID")


#SIMPLE REGRESSION
model_eu_complete <- lm(data = spatial_data_complete@data, household_income ~ unemployment)
summary(model_eu_complete)
length(model_eu_complete$residuals)


#MULTIPLE REGRESSION
model_eu2_complete <- lm(data = spatial_data_complete@data, household_income ~ unemployment + hh_urb_per +
                  res_dev + gva_growth + ht_perc)
summary(model_eu2_complete)
length(model_eu2$residuals)

## Residuals spatial distribution ##

spatial_data_complete$res <- model_eu_complete$residuals
spatial_data_complete$res2 <- model_eu2_complete$residuals

res_plot <- spplot(spatial_data_complete, zcol = "res", colorkey = TRUE, col.regions = pal(100), cuts = 99,
                   par.settings = list(axis.line = list(col =  'transparent')),
                   main = "Residuals from simple regression")

res_plot2 <- spplot(spatial_data_complete, zcol = "res2", colorkey = TRUE, col.regions = pal(100), cuts = 99,
                   par.settings = list(axis.line = list(col =  'transparent')),
                   main = "Residuals from multiple regression")

grid.arrange(res_plot, res_plot2)


### Weight matrices ###
centroids <- coordinates(spatial_data_complete)

#first order neighbourhood
cont <- poly2nb(spatial_data_complete, queen = T)
W_list <- nb2listw(cont, style = "W", zero.policy = T) # row normalization
W <- listw2mat(W_list)
plot.nb(cont, centroids, pch = 16, col = "grey")


#second order neighbourhood
cont2 <- nblag_cumul(nblag(cont, maxlag = 2))
W2_list <- nb2listw(cont2, style = "W", zero.policy = T) # row normalization
W2 <- listw2mat(W2_list)
plot.nb(cont2, centroids, pch = 16, col = "grey")


############################
# Spatial dependency test

#W1
# on residuals from simple regression

moran.test(spatial_data_complete@data$res, W_list) # same as line below
lm.morantest(model_eu_complete, listw = W_list) #same as line above

moran.plot(spatial_data_complete@data$res, W_list)

loc_mor <- localmoran(spatial_data_complete@data$res, W_list, p.adjust.method = "bonferroni")
spatial_data_complete@data$loc_mor_i_res_w <- loc_mor[,1]
spatial_data_complete@data$loc_mor_pv_res_w <- loc_mor[,5]

spplot(spatial_data_complete, zcol = "loc_mor_pv_res_w", colorkey = TRUE, col.regions = pal(100), cuts = 99,
       par.settings = list(axis.line = list(col =  'transparent')),
       main = "LISA bonferroni p-value on res from simp reg W1")

################################################################
lm.LMtests(model_eu_complete, listw = W_list, test = "all")
##################################################################

# on residuals from multiple regression
moran.test(spatial_data_complete@data$res2, W_list) #same as line below
lm.morantest(model_eu_complete, listw = W_list) #same as line above

moran.plot(spatial_data_complete@data$res2, W_list)

loc_mor <- localmoran(spatial_data_complete@data$res2, W_list, p.adjust.method = "bonferroni")
spatial_data_complete@data$loc_mor_i_res2_w <- loc_mor[,1]
spatial_data_complete@data$loc_mor_pv_res2_w <- loc_mor[,5]

spplot(spatial_data_complete, zcol = "loc_mor_pv_res2_w", colorkey = TRUE, col.regions = pal(100), cuts = 99,
       par.settings = list(axis.line = list(col =  'transparent')),
       main = "LISA bonferroni p-value on res from mult reg W1")

################################################################
lm.LMtests(model_eu2_complete, listw = W_list, test = "all")
##################################################################


#W2
#on residuals from simple regression
moran.test(spatial_data_complete@data$res, W2_list) #same as line below
lm.morantest(model_eu_complete, listw = W2_list) #same as line above

moran.plot(spatial_data_complete@data$res, W2_list)

loc_mor2 <- localmoran(spatial_data_complete@data$res, W2_list, p.adjust.method = "bonferroni")
spatial_data_complete@data$loc_mor_i_res_w2 <- loc_mor2[,1]
spatial_data_complete@data$loc_mor_pv_res_w2 <- loc_mor2[,5]

spplot(spatial_data_complete, zcol = "loc_mor_pv_res_w2", colorkey = TRUE, col.regions = pal(100), cuts = 99,
       par.settings = list(axis.line = list(col =  'transparent')),
       main = "LISA bonferroni p-value on res from simp reg W2")

################################################################
lm.LMtests(model_eu_complete, listw = W2_list, test = "all")
##################################################################


#on residuals from multiple regression

moran.test(spatial_data_complete@data$res2, W2_list)
lm.morantest(model_eu2_complete, listw = W2_list) #same as line above

moran.plot(spatial_data_complete@data$res2, W2_list)

loc_mor2 <- localmoran(spatial_data_complete@data$res2, W2_list, p.adjust.method = "bonferroni")
spatial_data_complete@data$loc_mor_i_res2_w2 <- loc_mor2[,1]
spatial_data_complete@data$loc_mor_pv_res2_w2 <- loc_mor2[,5]

spplot(spatial_data_complete, zcol = "loc_mor_pv_res2_w2", colorkey = TRUE, col.regions = pal(100), cuts = 99,
       par.settings = list(axis.line = list(col =  'transparent')),
       main = "LISA bonferroni p-value on res from mult reg W2")

################################################################
lm.LMtests(model_eu2_complete, listw = W2_list, test = "all")
##################################################################




############################
##########################
spatial_data_complete$res_lagged <-lag.listw(x = W_list, var = spatial_data_complete$res)

plot(spatial_data_complete@data$res, spatial_data_complete$res_lagged)


moran_plot_data <- data.frame(spatial_data_complete@data$res, spatial_data_complete$res_lagged, 
                              spatial_data_complete@data$loc_mor_pv_res_w)

colnames(moran_plot_data) <- c("res", "res_lagged", "p_val")

moran_plot_data$sign <- ifelse(moran_plot_data$p_val<=0.1,1,0)



moran_plot_data$sign_col <- ifelse(moran_plot_data$sign == 1 & 
                                     moran_plot_data$res<0 & moran_plot_data$res_lagged<0,
                                   "significant low-low", "unsignificant")

moran_plot_data$sign_col <- ifelse(moran_plot_data$sign == 1 & 
                                     moran_plot_data$res>0 & moran_plot_data$res_lagged>0,
                                   "significant high-high", moran_plot_data$sign_col)


morans_plot_res <- ggplot(data = moran_plot_data, aes(res, res_lagged) )+
  geom_point(aes(colour = factor(sign_col)),size = 1.5)+
  stat_smooth(method = "lm", geom = "line", col = "orange", alpha = 0.4, size = 2)+
  scale_color_manual(values = c('red','blue', "grey")) +
  geom_hline(yintercept=0, color = "black", linetype = "dashed")+
  geom_vline(xintercept=0, color = "black", linetype = "dashed")+
  xlab("Residual value") + 
  ylab("Spatially lagged residual value")+
  theme_classic()+
  theme(legend.position = c(0.15,0.8))+
  labs(col = "cluster significance (LISA)")+
  ggtitle(TeX("Moran's plot for residuals from regression $income = \\beta unemp + \\epsilon$ (Western Europe NUTS 2 regions, 2018)"))

morans_plot_res
##########################################
########## SPATIAL MODELS ############
# w1

## pure SAR model

model_pSAR <- spautolm(spatial_data_complete@data$household_income ~ 1, listw = W_list)
summary(model_pSAR)


## SAR (SLM) model
# ML estimation
model_SAR_ML <- lagsarlm(household_income ~ unemployment + hh_urb_per +
                           res_dev + gva_growth + ht_perc, listw = W_list,
                         data = spatial_data_complete)
summary(model_SAR_ML)

#2SLS estimation
model_SAR_2SLS <- stsls(household_income ~ unemployment + hh_urb_per +
                          res_dev + gva_growth + ht_perc, listw = W_list,
                        data = spatial_data_complete)
summary(model_SAR_2SLS)


## SEM model
# ML estimation
model_SEM_ML <- errorsarlm(household_income ~ unemployment + hh_urb_per +
                             res_dev + gva_growth + ht_perc, listw = W_list,
                           data = spatial_data_complete)
summary(model_SEM_ML)

#SGLS estimation
model_SEM_SGLS <- GMerrorsar(household_income ~ unemployment + hh_urb_per +
                               res_dev + gva_growth + ht_perc, listw = W_list,
                             data = spatial_data_complete)
summary(model_SEM_SGLS)

## SLX model
model_SLX <- lmSLX(household_income ~ unemployment + hh_urb_per +
                     res_dev + gva_growth + ht_perc, listw = W_list,
                   data = spatial_data_complete)
summary(model_SLX)

