#############
# FUNCTIONS #
itemPDF_2pl=function(theta,
                     a,d,
                     response){
  p=1/(1+exp(-sum(theta*a)+d))
  if(response==0) 1-p else p
}
getPstars=function(theta,
                   a,d,which='all'){
  #intermediate probability terms
  pstar=1/(1+exp(-sum(theta*a)+d))
  pstar=c(1,pstar,0)
  if(which=='all')pstar else pstar[which]
}
itemPDF_grm=function(theta,
                     a,d,
                     response){
  #intermediate probability terms
  pstar=getPstars(theta,
                  a,d)
  p=pstar[1:(length(pstar)-1)]-pstar[2:length(pstar)]
  p[response+1]
}
getAnalyticSEs=function(th,datRow,par,hypar,itemScores,vcMultiplier=1,par_items){
  require(tidyverse)
  require(janitor)
  require(purrr)
  # th=mirtScores[[1]][dataRow,]%>%select(contains('Theta'))%>%unlist
  # datRow=dataRow
  totalInfo=matrix(0,length(th),length(th))
  #get ingredients for each item, only if non-missing item response
  for(r in 1:ncol(itemScores)){
    if(!is.na(itemScores[datRow,r])){
      obsRange=range(itemScores[,r],na.rm=T)
      # pars=pars
      # hypars=hypars
      # theta=seq(-3,3,0.1)
      # dataRow=1
      # fds=(obsRange[1]:obsRange[2])%>%
      #   map(~fdHess(th,getPosterior.item.nlme,
      #               r=item,r=.))#%>%transpose
      #is item in table? error otherwise
      if(!any(par_items==names(itemScores)[r])) stop('Item not found in parameter table!')
      if(par$g[r]%in%c(0,NA)){
        a=par[which(par_items==names(itemScores)[r]),]%>%
          dplyr::select(starts_with('a'))%>%
          unlist%>%na.omit%>%c%>%setNames(NULL)
        d=par[which(par_items==names(itemScores)[r]),]%>%
          dplyr::select(starts_with('d'))%>%
          unlist%>%na.omit%>%c%>%setNames(NULL)
        #pstars
        probs=getPstars(th,a,d)
        getPstars=function(theta,
                           a,d,which='all'){
          #intermediate probability terms
          pstar=1/(1+exp(-sum(theta*a)+d))
          pstar=c(1,pstar,0)
          if(which=='all')pstar else pstar[which]
        }
        #derivatives of pstars
        dprobs=(1:length(probs))%>%map(~fdHess(th,getPstars,
                                               a=a,d=d,which=.))%>%
          map(~.$gradient)
        # #analytic?!
        # dprobs2=getGradPstars(th,a,d)
        # dprobs
        # dprobs2
        # dprobs=dprobs2
        
        #add term!
        for(i in 1:(length(probs)-1))
          totalInfo=totalInfo+
          (dprobs[[i]]-dprobs[[i+1]])%*%t((dprobs[[i]]-dprobs[[i+1]]))/
          (probs[i]-probs[i+1])
      } else {
        # print(r)
        #3pl
        a=par[which(par_items==names(itemScores)[r]),]%>%
          dplyr::select(starts_with('a'))%>%
          unlist%>%na.omit%>%c%>%setNames(NULL)
        d=par[which(par_items==names(itemScores)[r]),]%>%
          dplyr::select(starts_with('d'))%>%
          unlist%>%na.omit%>%c%>%setNames(NULL)
        g=par$g[which(par_items==names(itemScores)[r])]
        p=g+(1-g)/(1+exp(-sum(th*a)+d))
        #my attempt
        # totalInfo=totalInfo-(
        #   p*c((1-p)*(p-g)*(g*-p^2)/
        #       (p^2*(1-g)^2))*
        #     (a%*%t(a))+
        #     (1-p)*c((1-p)*(p-g)*(-p^2)/
        #         (p^2*(1-g)^2))*
        #     (a%*%t(a))
        # )
        #Lihua's equation 73
        newTerm=(
          ((p-g)^2)*(1-p)/(p*(1-g)^2)*(a%*%t(a))
        )
        totalInfo=totalInfo+newTerm
        # print(newTerm)
      }
    }
  }
  #add prior
  totalInfo=totalInfo+
    solve(hypar$varcov*vcMultiplier)
  # print(totalInfo)
  # ti<<-totalInfo
  sqrt(diag(solve(totalInfo)))
}

#the MAP calculator
myMAP=function(dat,#mod,
               par){
  # dat=data
  # mod=model
  # par=pars
  
  ########################
  # CONVERT ipar TO mirt #
  
  require(tidyverse)
  require(janitor)
  require(purrr)
  
  # pull out hyperparameters
  pars.mean<-par%>%
    subset(HyperParam=='MEAN')%>%
    dplyr::select(starts_with('a'))%>%unlist%>%setNames(NULL)
  
  pars.cov<-par%>%
    subset(HyperParam%in%paste0('COV_',1:100000))%>%
    dplyr::select(starts_with('a'))%>%as.matrix
  rownames(pars.cov)=NULL
  colnames(pars.cov)=NULL
  
  # clean up ipar for scoring 
  ipar_extra_cols<-c(d1=NA_real_,d2=NA_real_,d3=NA_real_,d4=NA_real_,
                     d5=NA_real_,d6=NA_real_,d7=NA_real_,d8=NA_real_)
  est.par<-par%>%
    filter(HyperParam==''|is.na(HyperParam))%>%dplyr::select(-HyperParam)%>%
    add_column(!!!ipar_extra_cols[!names(ipar_extra_cols) %in% names(.)])%>%
    mutate(model=case_when(model=='GRM'~'graded',
                           .default=model),
           d=case_when(model!='graded'~d1),
           d1=case_when(model=='graded'~d1),
           g=case_when(model!='graded'~g),
           d=-d,
           d1=case_when(!is.na(d1)~-d1),
           d2=case_when(!is.na(d2)~-d2),
           d3=case_when(!is.na(d3)~-d3),
           d4=case_when(!is.na(d4)~-d4),
           d5=case_when(!is.na(d5)~-d5),
           d6=case_when(!is.na(d6)~-d6),
           d7=case_when(!is.na(d7)~-d7),
           d8=case_when(!is.na(d8)~-d8))
  
  est.par<-est.par[,colSums(is.na(est.par))<nrow(est.par)]
  
  # clean up items 
  par_items=par%>%
    dplyr::select(ItemID)%>%
    drop_na()%>%
    pull(ItemID)
  
  #make a mirt model?
  mod=mirtCAT::generate.mirt_object(parameters=est.par%>%
                                           dplyr::select(-ItemID,-model),
                                         itemtype=est.par$model%>%
                                      setNames(est.par$ItemID))

  #check matching mins and k's
  dataRanges=dat%>%
    dplyr::select(any_of(par_items))%>%
    purrr::map(~c(min(.,na.rm=T),max(.,na.rm=T)))%>%
    as_tibble%>%
    mutate(across(everything(),as.double))
  names(mod@Data$mins)=est.par$ItemID
  mirtRanges=rbind(mod@Data$mins,mod@Data$K+mod@Data$mins-1)%>%
    as_tibble%>%
    rename_with(~gsub('_Score','',.,fixed=T))%>%
    mutate(across(everything(),as.double))
  
  
  if(any(dataRanges[1,]<mirtRanges[1,] | 
         dataRanges[2,]>mirtRanges[2,])) stop('Data out of bounds')
  
  ###########################################################
  # append extra rows to make mirt behave maybe (?!??!?!?!) #
  appendData=mirtRanges%>%
    purrr::map(~.[1]:.[2])
  longestLength=map_dbl(appendData,length)%>%max
  appendData=appendData%>%
    purrr::map2(mirtRanges,function(x,y)if(length(x)<longestLength) x=c(x,rep(x,100)[1:(longestLength-length(x))])-y[1] else x-y[1])%>%
    as_tibble
  
  #hypar
  hypar=list(mean=pars.mean)
  ndim=length(hypar$mean)
  hypar$varcov=pars.cov
  kThetaMax=par%>%subset(HyperParam=='kThetaMax')%>%pull(a1)
  
  #custom density for prior
  funM=function(Theta,...)dmnorm(Theta,
                                 mean=hypar$mean,
                                 varcov=as.matrix(hypar$varcov))
  
  #get mirt scores, no SE's
  require(mirt)
  require(mnormt)
  require(nlme)
  require(mirtCAT)
  pct=proc.time()
  
  mirtScores=fscores(mod,response.pattern=dat%>%
                       dplyr::select(any_of(par_items))%>%
                       bind_rows(appendData),
                     method='MAP',custom_den=funM,
                     max_theta=kThetaMax)%>%
    as_tibble%>%slice_head(n=nrow(dat))
  dat2score=dat
  #remove SE's
  mirtScores=mirtScores%>%as.data.frame%>%as_tibble%>%
    dplyr::select(-starts_with('SE_'))
  
  itemScores=dat2score%>%
    dplyr::select(any_of(par_items))
  
  mirtScores=mirtScores%>%
    as_tibble%>%
    dplyr::select(-starts_with('SE_'))%>%
    rename_with(~c(paste0('mirtTheta_',1:(length(.)))))
  
  #next, "analytical" SE's
  mirtScores%>%
    mutate(analytic=apply(across(starts_with('mirtTheta')),1,list)%>%
             setNames(1:n())%>%
             purrr::imap(~getAnalyticSEs(.x[[1]],as.numeric(.y),par,hypar,itemScores,par_items=par_items)))%>%
    mutate(analytic2=analytic%>%map(~setNames(.,paste0('analyticSE',1:length(.)))%>%
                                      as.list%>%as_tibble))%>%
    unnest(cols=analytic2)%>%
    dplyr::select(-analytic)
}
