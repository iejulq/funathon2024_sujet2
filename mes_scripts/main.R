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
library(gt)

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



stats_aeroports_table <- stats_aeroports %>%
  mutate(name_clean = paste0(str_to_sentence(apt_nom), " _(", apt, ")_")
  ) %>%
  select(name_clean, everything())

#  Arrêt ici (package gt)

table_aeroports <- gt(stats_aeroports_table)

table_aeroports <- table_aeroports %>%
  cols_hide(columns = starts_with("apt"))
table_aeroports

table_aeroports <- table_aeroports %>%
  fmt_number(columns = starts_with("pax"), suffixing = TRUE)
table_aeroports

table_aeroports <- table_aeroports %>%
  fmt_markdown(columns = "name_clean")
table_aeroports

table_aeroports <- table_aeroports %>%
  cols_label(
    name_clean = md("**Aéroport**"),
    paxdep = md("**Départs**"),
    paxarr = md("**Arrivée**"),
    paxtra = md("**Transit**")
  ) %>%
  tab_header(
    title = md("**Statistiques de fréquentation**"),
    subtitle = md("Classement des aéroports")
  ) %>%
  tab_style(
    style = cell_fill(color = "powderblue"),
    locations = cells_title()
  ) %>%
  tab_source_note(source_note = md("_Source: DGAC, à partir des données sur data.gouv.fr_"))

table_aeroports

table_aeroports <- table_aeroports %>%
  opt_interactive()
table_aeroports





trafic_date <- pax_apt_all %>%
  mutate(
    date = as.Date(paste(anmois, "01", sep=""), format = "%Y%m%d")
  ) %>%
  filter(mois == month, an == year)

trafic_aeroports <- airports_location %>%
  inner_join(trafic_date, by = c("Code.OACI" = "apt"))


library(leaflet)

leaflet(trafic_aeroports) %>% addTiles() %>%
  addMarkers(popup = ~paste0(Nom, ": ", trafic)) 

trafic_aeroports <- trafic_aeroports %>%
  mutate(
    volume = ntile(trafic, 3)
  ) %>%
  mutate(
    color = palette[volume]
  )


icons <- awesomeIcons(
  icon = 'plane',
  iconColor = 'black',
  library = 'fa',
  markerColor = trafic_aeroports$color
)


carte_interactive <- leaflet(trafic_aeroports) %>% addTiles() %>%
  addAwesomeMarkers(
    icon=icons[],
    label=~paste0(Nom, "", " (",Code.OACI, ") : ", trafic, " voyageurs")
  )
