library(bdl)
#### BDL API ####
#search_variables("bezrobocia") %>%
#  View()

get_variables("P2497")

wages_pow <- get_data_by_variable("64428", unitLevel = "5", year = 2017)

get_variables("P2392")

unemp_pow <- get_data_by_variable("60270", unitLevel = "5", year = 2017)

#search_variables("uczelnie") %>%
#  View()







