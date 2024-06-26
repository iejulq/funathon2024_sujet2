library(yaml)
library(readr)
library(tidyverse)
library(readr)

yaml::read_yaml("sources.yml")


create_data_list<-function(source_file){
  
  liste_sources<-yaml::read_yaml(source_file)
  
  return(liste_sources)
}

urls<-create_data_list("sources.yml")  

urls_aeroports_unlist<-unlist(urls$airports)

urls_compagnies_unlist<-unlist(urls$compagnies)



#un exemple 

#lecture d'un fichier de la liste aeroports 
data<-readr::read_csv2(
  urls_unlist[1],
  col_types = cols(
    ANMOIS = col_character(),
    APT = col_character(),
    APT_NOM = col_character(),
    APT_ZON = col_character(),
    .default = col_double()
  )
)

# creation mois et annees 
data<-data %>% mutate(
  ANMOIS=as.character(ANMOIS),
  AN=stringr::str_sub(ANMOIS,1,4),
  MOIS=stringr::str_sub(ANMOIS,5,6)) %>% 
  mutate(MOIS=str_remove(MOIS,pattern = "^0+"))#zéro au de

)



clean_dataframe<-function(data){
  
  # creation mois et annees 
  data<-data %>% mutate(
    ANMOIS=as.character(ANMOIS),
    AN=stringr::str_sub(ANMOIS,1,4),
    MOIS=stringr::str_sub(ANMOIS,5,6)) %>% 
    mutate(MOIS=str_remove(MOIS,pattern = "^0+"))#zéro au de
  
  
  colnames(data)<-tolower(colnames(data))
  
  return(data)
  
}

importer_donnees_aeroports<-function(list_files){
  
  data<-readr::read_csv2(
    list_files,
    col_types = cols(
      ANMOIS = col_character(),
      APT = col_character(),
      APT_NOM = col_character(),
      APT_ZON = col_character(),
      .default = col_double()
    )
  )  %>% clean_dataframe()
  
  return(data)
  
}

res<-map(urls_aeroports_unlist,importer_donnees_aeroports) %>% list_rbind()



data_compagnies<-readr::read_csv2(unlist(urls$compagnies)[1],
                                  col_types = cols(
                                    ANMOIS = col_character(),
                                    CIE = col_character(),
                                    CIE_NOM = col_character(),
                                    CIE_NAT = col_character(),
                                    CIE_PAYS = col_character(),
                                    .default = col_double()
                                  ))


importer_donnees_compagnies<-function(list_files){
  
  data<-readr::read_csv2(
    list_files,
    col_types = cols(
      ANMOIS = col_character(),
      CIE = col_character(),
      CIE_NOM = col_character(),
      CIE_NAT = col_character(),
      CIE_PAYS = col_character(),
      .default = col_double()
    )
  )  %>% clean_dataframe()
  
  return(data)
  
}


import_liaisons_data <- function(list_files){
  
  pax_lsn_all <- readr::read_csv2(
    file = list_files,
    col_types = cols(
      ANMOIS = col_character(),
      LSN = col_character(),
      LSN_DEP_NOM = col_character(),
      LSN_ARR_NOM = col_character(),
      LSN_SCT = col_character(),
      LSN_FSC = col_character(),
      .default = col_double()
    ) 
  ) %>% 
    clean_dataframe()
  
  return(pax_lsn_all)
  
  
}




source("R/import/data.R")



importer_donnees_aeroports(urls_aeroports_unlist)


aeroport_localisation<-sf::st_read(urls$geojson$airport)

crs<-sf::st_crs(aeroport_localisation)


crs$input

plot(aeroport_localisation$geometry)


library(leaflet)

ls(package:leaflet)

leaflet(aeroport_localisation) %>% 
  addMarkers(popup = ~Nom) %>% 
  addTiles()



mois<- 1
year <- 2019

palette <- c("green", "blue", "red")


rm(trafic_aeroports)

trafic_date<-pax_apt_all %>% filter(an=="2020" & mois=="3") 

trafic_aeroports<-airports_location%>% inner_join(trafic_date$,by=c("Code.OACI"="apt"))

data(quakes)

# Show first 20 rows from the `quakes` dataset
leaflet(data = trafic_aeroports) %>% addTiles() %>%
  addMarkers(~long, ~lat, popup = ~as.character(mag), label = ~as.character(mag))
