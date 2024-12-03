score_lwl_sp<-function(data,age){
  
  require(tidyverse)
  require(mirt)
  require(mirtCAT)

  # clean input data for scoring
  clean_data=data%>%
    clean_names()%>%
    dplyr::select(contains('_G'))%>%
    mutate(across(everything(),as.numeric))%>%
    dplyr::select_if(~sum(!is.na(.))>0)
  
  # pull # correct items
  correct_items=clean_data%>%
    rowwise()%>%
    mutate(sum_correct=sum(.))%>%
    pull(sum_correct)
  
  # pull # items completed
  items_completed=length(clean_data)
  
  # scoring routine starts here
  if(items_completed>=6){
    out<-c(correct_items/items_completed*100,correct_items,items_completed)
    names(out)<-c('percent_correct','correct','completed')
  }else if(items_completed<6){
    out=paste0('not enough items completed to receive a score - there are only ', items_completed,' items')
  }
  return(out)
}
