plot_airport_line<-function(data,choix_airport=default_airport){
  
  trafic_aeroports<-data %>%
    mutate(trafic=apt_pax_dep + apt_pax_tr + apt_pax_arr) %>% 
    filter(apt %in% choix_airport) %>% 
    mutate(date = as.Date(paste(anmois, "01", sep=""), format = "%Y%m%d") ) 
  
  fig <- plotly::plot_ly(trafic_aeroports, x = ~date, y = ~trafic, type = 'scatter', mode = 'lines+markers')  
  
  fig
}
