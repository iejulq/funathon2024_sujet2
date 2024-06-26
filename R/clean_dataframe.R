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