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