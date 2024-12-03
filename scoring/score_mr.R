score_mr<-function(data){
  require(tidyverse)
  require(janitor)
  require(mirt)
  require(mirtCAT)
  require(mnormt)
  require(nlme)
  source('scoring/myMAP.R')
  
  # load ipars
  load('ipar/mr_ipar.RData')
  
  # clean up ipar for scoring
  mr_ipar<-mr_ipar[rowSums(is.na(mr_ipar)) != ncol(mr_ipar), ]%>%
    mutate(ItemID=tolower(ItemID))
  
  # clean input data for scoring + imputation
  mr_cols<-c(RL1=NA_real_,RL3=NA_real_,RL5=NA_real_,RL6=NA_real_,
             RL9=NA_real_,RL10=NA_real_,RL13=NA_real_,RL15=NA_real_,
             RL16=NA_real_,RL17=NA_real_,RL18=NA_real_,RL22=NA_real_)
  
  clean_data=data%>%
    mutate(across(contains('RL'),as.numeric))%>%
    add_column(!!!mr_cols[!names(mr_cols) %in% names(.)])%>%
    dplyr::select(all_of(names(mr_cols)))%>%
    clean_names()
  
  # pull # correct items
  correct_items=clean_data%>%
    rowwise()%>%
    mutate(sum_correct=sum(.))%>%
    pull(sum_correct)
  
  items_completed=clean_data%>%dplyr::select_if(~sum(!is.na(.))>0)%>%with(length(.))
  
  # refined for our revised MAP calculator
  if(items_completed==0){
    out=tibble(mirtTheta_1=NA_real_,analyticSE1=NA_real_)
  }else{
    # scoring routine starts here
    out<-myMAP(dat=clean_data,par=mr_ipar)
  }

  return(out)
}