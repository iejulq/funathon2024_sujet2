create_data_list<-function(source_file){
  
  liste_sources<-yaml::read_yaml(source_file)
  
  return(liste_sources)
}
