score_me<-function(data){
  require(tidyverse)
  require(janitor)
  require(mirt)
  require(mirtCAT)
  require(mnormt)
  require(nlme)
  source('scoring/myMAP.R')
  
  # load ipars
  load('ipar/me_ipar.RData')
  
  # clean up ipar for scoring
  me_ipar<-me_ipar[rowSums(is.na(me_ipar)) != ncol(me_ipar), ]%>%
    mutate(ItemID=tolower(ItemID))
  
  # get list of items
  me_items<-me_ipar%>%
    subset(!is.na(ItemID))%>%
    pull(ItemID)
  
  # clean input data for scoring + combine items
  me_cols<-c(el2=NA_real_,el3=NA_real_,el4=NA_real_,el5=NA_real_,
             el6=NA_real_,el7=NA_real_,el8=NA_real_,el10=NA_real_,el11=NA_real_,el12=NA_real_,
             el13=NA_real_,el14=NA_real_,el17=NA_real_,el19=NA_real_,el22=NA_real_,
             el15=NA_real_,el16=NA_real_,el2_el8=NA_real_,el12_el22=NA_real_,el15_el16=NA_real_)
  
  clean_data=data%>%
    clean_names()%>%
    add_column(!!!me_cols[!names(me_cols) %in% names(.)])%>%
    mutate(across(contains('el'),as.numeric))%>%
    rowwise()%>%
    mutate(el2_el8=case_when(is.na(el2)&is.na(el3)&is.na(el5)&is.na(el6)&is.na(el8)~NA,
                             el8==1~5,
                             is.na(el8)|(el8==0&el6==1)~4,
                             (is.na(el8)|el8==0)&(!is.na(el6)|el6==0)&el5==1~3,
                             (is.na(el8)|el8==0)&(!is.na(el6)|el6==0)&(!is.na(el5)|el5==0)&el4==1~2,
                             (is.na(el8)|el8==0)&(!is.na(el6)|el6==0)&(!is.na(el5)|el5==0)&(!is.na(el4)|el4==0)&el3==1~1,
                             (is.na(el8)|el8==0)&(!is.na(el6)|el6==0)&(!is.na(el5)|el5==0)&(!is.na(el4)|el4==0)&(!is.na(el3)|el3==0)&el2==1~0),
           el12_el22=case_when(is.na(el12)&is.na(el13)&is.na(el14)&is.na(el17)&is.na(el19)&is.na(el22)~NA,
                               el22==1~5,
                               (!is.na(el22)|el22==0)&el19==1~4,
                               (!is.na(el22)|el22==0)&(!is.na(el19)|el19==0)&el17==1~3,
                               (!is.na(el22)|el22==0)&(!is.na(el19)|el19==0)&(!is.na(el17)|el17==0)&el14==1~2,
                               (!is.na(el22)|el22==0)&(!is.na(el19)|el19==0)&(!is.na(el17)|el17==0)&(!is.na(el14)|el14==0)&el13==1~1,
                               (!is.na(el22)|el22==0)&(!is.na(el19)|el19==0)&(!is.na(el17)|el17==0)&(!is.na(el14)|el14==0)&(!is.na(el13)|el13==0)&el12==1~0),
           el15_el16=case_when(!is.na(el15)&is.na(el16)~el15,
                               is.na(el15)&!is.na(el16)~el16,
                               !is.na(el15)&!is.na(el16)~sum(el15,el16),
                               is.na(el15)&is.na(el16)~NA))%>%
    rename_all(~str_remove(.,'_score'))%>%
    dplyr::select(any_of(me_items))#%>%
    # dplyr::select_if(~sum(!is.na(.))>0)
  
  # check how many items from mullen prompted: EL7, EL10, EL11 and EL15_EL16
  items_completed=clean_data%>%dplyr::select_if(~sum(!is.na(.))>0)%>%with(length(.))
  prompted_completed=clean_data%>%dplyr::select(c('el7','el10','el11','el15_el16'))%>%dplyr::select_if(~sum(!is.na(.))>0)%>%with(length(.))
  
  # check how many items from mullen observational: EL2_EL8, EL12_EL22
  observational_completed=clean_data%>%dplyr::select(c('el2_el8','el12_el22'))%>%dplyr::select_if(~sum(!is.na(.))>0)%>%with(length(.))
  
  # refined for our revised MAP calculator
  # at least one prompted and observational response is needed for a score!
  if(prompted_completed>=1&&observational_completed>=1){
    out<-myMAP(dat=clean_data,par=me_ipar)
  }else{
    out=tibble(mirtTheta_1=NA_real_,analyticSE1=NA_real_)
    print('not enough items towards scoring for mullen expressive')
  }
    
  return(out)
}
