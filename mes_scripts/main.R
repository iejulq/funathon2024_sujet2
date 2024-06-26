#Scripts R 

source("R/create_data_list.R")
source("R/import_data.R")  
source("R/clean_dataframe.R")
source("R/figures.R")
source("R/divers_fonctions.R")

library(tidyverse)
library(sf)
library(lubridate)
library(plotly)

MONTHS_LIST = 1:12
YEARS_LIST  <- as.character(2018:2022)


annee <- YEARS_LIST[1]
mois <- MONTHS_LIST[1]

# Load data ----------------------------------
urls <- create_data_list("./sources.yml")


pax_apt_all <- importer_donnees_aeroports(unlist(urls$airports))
pax_cie_all <- importer_donnees_compagnies(unlist(urls$compagnies))
pax_lsn_all <- import_liaisons_data(unlist(urls$liaisons))

airports_location <- st_read(urls$geojson$airport)

pax_apt_all<-pax_apt_all %>% mutate(trafic=apt_pax_dep + apt_pax_tr + apt_pax_arr) 


liste_aeroports <- unique(pax_apt_all$apt)#liste des aéroports
default_airport <- liste_aeroports[1] #aéroport par defaut 



stats_aeroports <-summary_stat_airport(create_data_from_input(pax_apt_all,annee,mois))


stats_liaisons <-summary_stat_liaisons(create_data_from_input(pax_lsn_all,annee,mois))



#Figure
figure_plotly <-plot_airport_line(pax_apt_all,"FMEE")




