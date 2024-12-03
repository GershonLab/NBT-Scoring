score_lwl<-function(data,age){
  
  require(tidyverse)
  require(janitor)
  require(mirt)
  require(mirtCAT)
  require(mnormt)
  require(nlme)
  source('scoring/myMAP.R')

  # load lwl ipars
  load('ipar/lwl_younger_ipar.RData')
  load('ipar/lwl_older_ipar.RData')
  
  # clean up ipar for scoring
  lwl_younger_ipar<-lwl_younger_ipar[rowSums(is.na(lwl_younger_ipar)) != ncol(lwl_younger_ipar), ]%>%
    mutate(ItemID=tolower(ItemID))
  lwl_older_ipar<-lwl_older_ipar[rowSums(is.na(lwl_older_ipar)) != ncol(lwl_older_ipar), ]%>%
    mutate(ItemID=tolower(ItemID))
  
  # get list of items
  lwl_cols<-c(book_nose_g1=NA_real_, foot_cup_g1=NA_real_, door_sock_g1=NA_real_, playground_shoulder_g5=NA_real_, eye_jar_g3=NA_real_, 
               hose_rooster_g5=NA_real_, car_shoe_g1=NA_real_, cup_shoe_g1=NA_real_, ladder_melon_g5=NA_real_, table_necklace_g3=NA_real_, 
               pen_jeans_g4=NA_real_, nose_car_g1=NA_real_, jar_wolf_g3=NA_real_, toothbrush_block_g2=NA_real_, melon_sneaker_g5=NA_real_, 
               block_cow_g2=NA_real_, window_table_g3=NA_real_, wolf_flower_g3=NA_real_, hand_crib_g2=NA_real_, lion_zipper_g4=NA_real_, 
               muffin_lion_g4=NA_real_, blanket_toothbrush_g2=NA_real_, flower_eye_g3=NA_real_, shoulder_hose_g5=NA_real_, 
               zipper_donkey_g4=NA_real_, deer_pen_g4=NA_real_, foot_book_g1=NA_real_, tricycle_ladder_g5=NA_real_, apple_truck_g2=NA_real_, 
               rooster_playground_g5=NA_real_, crib_apple_g2=NA_real_, jeans_donkey_g4=NA_real_, cow_blanket_g2=NA_real_, 
               shovel_window_g3=NA_real_, truck_hand_g2=NA_real_, bucket_deer_g4=NA_real_, sneaker_tricycle_g5=NA_real_, 
               necklace_shovel_g3=NA_real_, sock_door_g1=NA_real_, bucket_muffin_g4=NA_real_)
  
  # clean input data for scoring
  clean_data=data%>%
    clean_names()%>%
    dplyr::select(contains(c('g1','g2','g3','g4','g5')))%>%
    mutate(across(everything(),as.numeric))%>%
    dplyr::select_if(~sum(!is.na(.))>0)
  
  # pull # correct items
  correct_items=clean_data%>%
    rowwise()%>%
    mutate(sum_correct=sum(.))%>%
    pull(sum_correct)
  
  # pull # items completed
  items_completed=length(clean_data)
  
  # add items not completed as NA
  clean_data=clean_data%>%
    add_column(!!!lwl_cols[!names(lwl_cols) %in% names(.)])
  
  # scoring routine starts here
  if(items_completed>=6){
    if(age<15){
      out<-myMAP(dat=clean_data,par=lwl_younger_ipar)
    }
    else if (age>=15){
      
      out<-myMAP(dat=clean_data,par=lwl_older_ipar)
      
    }
  }
  else if(items_completed<6){
    out=tibble(mirtTheta_1=NA_real_,analyticSE1=NA_real_)
    print(paste0('not enough items completed to receive a score - there are only ', items_completed,' items'))
  }
  return(out)
}