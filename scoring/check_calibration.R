check_calibration<-function(data){
  require(tidyverse)
  require(janitor)
  
  clean_data=data%>%
    dplyr::select(contains('Calibration'))%>%
    mutate(across(everything(),as.numeric))%>%
    mutate(calibration_score=rowSums(dplyr::select(.,contains(c('Calibration'))),na.rm=T))
  
  calibration_score=clean_data%>%
    pull(calibration_score)
  
  return(calibration_score)
}