library(readr)
library(readxl)
library(dplyr)
library(plm)
library(pdR)
library(Rsolnp)

`%!in%` = Negate(`%in%`)

#### Panel estimation fonction

f.pmodel<-function(p.df,DVar,Vars){
  # p.df is a panel dataframe Utilise function "pdata.frame" of package "plm" to create a panel dataframe
  # DVar represent the name of dependant varianble
  # Vars represent the names of independantes variables
  
  ind<-names(index(p.df))
  type<-lapply(p.df[,Vars],function(x) setdiff(class(x), "pseries"))
  if(length(Vars)==1) type<-setdiff(class(p.df[,Vars]), "pseries")
  fVars<-Vars[which(type=="factor")]
  Vars<-Vars[which(type=="numeric")]
  df.Base<-data.frame(pays=p.df[[ind[1]]],date=p.df[[ind[2]]],y=p.df[[DVar]])
  if(length(Vars)>0) df.Base<-cbind(df.Base,p.df[,match(Vars,names(p.df))])
  if(length(fVars)>0) df.Base<-cbind(df.Base,p.df[,match(fVars,names(p.df))])
  names(df.Base)<-c("pays","date","y",Vars,fVars)
  
  p.Base<-pdata.frame(df.Base,index=c("pays","date"))
  
  frm<-paste("y ~ ")
  if(length(Vars)>0) frm<-paste(frm,paste(Vars, collapse= "+"))
  if(length(fVars)>0) {
    if(length(Vars)>0) {
      frm<-paste(frm,"+",paste(fVars, collapse= "+"))
      } else {
      frm<-paste(frm,paste(fVars, collapse= "+"))
      }
  }
  frm<-as.formula(frm)
  grun.fe <- plm(frm, data = p.Base, model = "within")
  
  X<-NULL
  if(length(fVars)>0){
    for(i in 1:length(fVars)){
      frm<-as.formula(paste(" ~ ", paste(fVars[i], collapse= "+"),"-1"))
      X<-cbind(X,model.matrix(frm, data=df.Base)[,-1])
    }
    # df.Base<-df.Base[,!(names(df.Base) %in% fVars)]
    df.Base<-cbind(df.Base,X)
  }
  
  if(length(Vars)>0) {
    Y<-matrix(as.matrix(df.Base[,Vars]),ncol=length(Vars),nrow=nrow(df.Base))
  }else{
    Y<-NULL
  }
  
  s<-residuals(grun.fe)
  df.Base$resid<-s[match(rownames(df.Base),names(s))]
  s<-fixef(grun.fe)
  df.Base$fixef<-s[match(df.Base$pays,names(s))]
  
  df.Base$y_esti<-as.numeric(df.Base$fixef)+as.numeric(cbind(Y,X)%*%coefficients(grun.fe))
  df.Base$uit_hat<-as.numeric(df.Base$y)-as.numeric(cbind(Y,X)%*%coefficients(grun.fe))
  
  return(list(Base=df.Base,SCE=sum((residuals(grun.fe))^2),Estim = grun.fe))
}

#### Threshold Panel estimation fonction

f.Thpmodel<-function(p.df,ThVar,trim,DVar,Vars,nTh=1){
  # p.df is a panel dataframe Utilise function "pdata.frame" of package "plm" to create a panel dataframe
  # ThVar represent the name of Threshold varianble : Threshold variable is the only one affected by threshold
  # trim is the pourcentage of data to let before and after for the range of Threshold (trim in (0,0.5))
  # DVar represent the name of dependant varianble
  # Vars represent the names of independantes variables
  
  ind<-names(index(p.df))
  type<-lapply(p.df[,Vars],function(x) setdiff(class(x), "pseries"))
  if(length(Vars)==1) type<-setdiff(class(p.df[,Vars]), "pseries")
  fVars<-Vars[which(type=="factor")]
  Vars<-Vars[which(type=="numeric")]
  df.Base<-data.frame(pays=p.df[[ind[1]]],date=p.df[[ind[2]]],y=p.df[[DVar]])
  df.Base$ThVarLow<-df.Base$ThVarHig<-rep(NA,nrow(df.Base))
  if(length(Vars)>0) df.Base<-cbind(df.Base,p.df[,match(Vars,names(p.df))])
  if(length(fVars)>0) df.Base<-cbind(df.Base,p.df[,match(fVars,names(p.df))])
  names(df.Base)<-c("pays","date","y","ThVarLow","ThVarHig",Vars,fVars)
  
  temp<-as.numeric(p.df[[ThVar]])
  b<-as.numeric(quantile(temp,na.rm=TRUE,probs=trim[1]))
  B<-as.numeric(quantile(temp,na.rm=TRUE,probs=1-trim[1]))
  
  gamma_range<-as.numeric(names(table(floor(temp[(temp>b)&(temp<B)]))))
  gamma<-gamma_opt<-gamma_range[1]
  df.Base$ThVarLow<-temp*(temp<=gamma)
  df.Base$ThVarHig<-temp*(temp>gamma)
  p.Base<-pdata.frame(df.Base,index=c("pays","date"))
  
  frm<-paste("y ~ ThVarLow+ThVarHig")
  if(length(Vars)>0) frm<-paste(frm,"+",paste(Vars, collapse= "+"))
  if(length(fVars)>0) frm<-paste(frm,"+",paste(fVars, collapse= "+"))
  frm<-as.formula(frm)
  
  grun.fe <- grun.fe_opt <- plm(frm, data = p.Base, model = "within")
  Lik<-sum((residuals(grun.fe))^2)
  
  gamma_range<-gamma_range[2:length(gamma_range)]
  
  for(gamma in gamma_range){
    df.Base$ThVarLow<-temp*(temp<=gamma)
    df.Base$ThVarHig<-temp*(temp>gamma)
    
    p.Base<-pdata.frame(df.Base,index=c("pays","date"))
    grun.fe <- plm(frm, data = p.Base, model = "within")
    Lik2<-sum((residuals(grun.fe))^2)
    
    if(Lik2<Lik) {
      Lik<-Lik2
      gamma_opt<-gamma
      grun.fe_opt<-grun.fe
    } 
  }
  
  df.Base$ThVarLow<-temp*(temp<=gamma_opt)
  df.Base$ThVarHig<-temp*(temp>gamma_opt)
  SCE<-sum((residuals(grun.fe_opt))^2)
  
  if(nTh==1){
    X<-NULL
    if(length(fVars)>0){
      for(i in 1:length(fVars)){
        frm<-as.formula(paste(" ~ ", paste(fVars[i], collapse= "+"),"-1"))
        X<-cbind(X,model.matrix(frm, data=df.Base)[,-1])
      }
      # df.Base<-df.Base[,!(names(df.Base) %in% fVars)]
      df.Base<-cbind(df.Base,X)
    }
    
    s<-residuals(grun.fe_opt)
    df.Base$resid<-s[match(rownames(df.Base),names(s))]
    s<-fixef(grun.fe_opt)
    df.Base$fixef<-s[match(df.Base$pays,names(s))]
    
    df.Base$y_esti<-as.numeric(df.Base$fixef)+as.numeric(cbind(as.matrix(df.Base[,c("ThVarLow","ThVarHig",Vars)]),X)%*%coefficients(grun.fe_opt))
    df.Base$uit_hat<-as.numeric(df.Base$y)-as.numeric(cbind(as.matrix(df.Base[,c("ThVarLow","ThVarHig",Vars)]),X)%*%coefficients(grun.fe_opt))
    
    res<-list(Base=df.Base,Threshold=gamma_opt,SCE=SCE,Estim=grun.fe_opt)
    
  }else{
    opt<-grun.fe_opt
    if(length(trim)==1) trim<-trim*rep(1,nTh)
    df.Base$ThVarMed<-rep(NA,nrow(df.Base))
    df.Base<-df.Base[,c("pays","date","y","ThVarLow","ThVarMed","ThVarHig",Vars,fVars)]
    
    b<-as.numeric(quantile(temp[temp<=gamma_opt],na.rm=TRUE,probs=trim[2]))
    B<-as.numeric(quantile(temp[temp<=gamma_opt],na.rm=TRUE,probs=1-trim[2]))
    gamma_range<-as.numeric(names(table(floor(temp[(temp>b)&(temp<B)]))))
    
    gamma<-gamma_opt2<-gamma_range[1]
    df.Base$ThVarLow<-temp*(temp<=gamma)
    df.Base$ThVarMed<-temp*((temp>gamma)&(temp<=gamma_opt))
    df.Base$ThVarHig<-temp*(temp>gamma_opt)
    p.Base<-pdata.frame(df.Base,index=c("pays","date"))
    
    frm<-paste("y ~ ThVarLow+ThVarMed+ThVarHig")
    if(length(Vars)>0) frm<-paste(frm,"+",paste(Vars, collapse= "+"))
    if(length(fVars)>0) frm<-paste(frm,"+",paste(fVars, collapse= "+"))
    frm<-as.formula(frm)
    
    grun.fe <- grun.fe_opt <- plm(frm, data = p.Base, model = "within")
    Lik<-sum((residuals(grun.fe))^2)
    
    gamma_range<-gamma_range[2:length(gamma_range)]
    
    for(gamma in gamma_range){
      df.Base$ThVarLow<-temp*(temp<=gamma)
      df.Base$ThVarMed<-temp*((temp>gamma)&(temp<=gamma_opt))
      df.Base$ThVarHig<-temp*(temp>gamma_opt)
      
      p.Base<-pdata.frame(df.Base,index=c("pays","date"))
      grun.fe <- plm(frm, data = p.Base, model = "within")
      Lik2<-sum((residuals(grun.fe))^2)
      
      if(Lik2<Lik) {
        Lik<-Lik2
        gamma_opt2<-gamma
        grun.fe_opt<-grun.fe
      } 
    }
    
    b<-as.numeric(quantile(temp[temp>gamma_opt],na.rm=TRUE,probs=trim[2]))
    B<-as.numeric(quantile(temp[temp>gamma_opt],na.rm=TRUE,probs=1-trim[2]))
    gamma_range<-as.numeric(names(table(floor(temp[(temp>b)&(temp<B)]))))
  
    for(gamma in gamma_range){
      df.Base$ThVarLow<-temp*(temp<=gamma_opt)
      df.Base$ThVarMed<-temp*((temp>gamma_opt)&(temp<=gamma))
      df.Base$ThVarHig<-temp*(temp>gamma)
      
      p.Base<-pdata.frame(df.Base,index=c("pays","date"))
      grun.fe <- plm(frm, data = p.Base, model = "within")
      Lik2<-sum((residuals(grun.fe))^2)
      
      if(Lik2<Lik) {
        Lik<-Lik2
        gamma_opt2<-gamma
        grun.fe_opt<-grun.fe
      } 
    }
    
    ### Refinement
    
    b<-as.numeric(quantile(temp[temp<=gamma_opt2],na.rm=TRUE,probs=trim[2]))
    B<-as.numeric(quantile(temp[temp<=gamma_opt2],na.rm=TRUE,probs=1-trim[2]))
    gamma_range<-as.numeric(names(table(floor(temp[(temp>b)&(temp<B)]))))
    
    for(gamma in gamma_range){
      df.Base$ThVarLow<-temp*(temp<=gamma)
      df.Base$ThVarMed<-temp*((temp>gamma)&(temp<=gamma_opt2))
      df.Base$ThVarHig<-temp*(temp>gamma_opt2)
      
      p.Base<-pdata.frame(df.Base,index=c("pays","date"))
      grun.fe <- plm(frm, data = p.Base, model = "within")
      Lik2<-sum((residuals(grun.fe))^2)
      
      if(Lik2<Lik) {
        Lik<-Lik2
        gamma_opt<-gamma
        grun.fe_opt<-grun.fe
      } 
    }
    
    b<-as.numeric(quantile(temp[temp>gamma_opt2],na.rm=TRUE,probs=trim[2]))
    B<-as.numeric(quantile(temp[temp>gamma_opt2],na.rm=TRUE,probs=1-trim[2]))
    gamma_range<-as.numeric(names(table(floor(temp[(temp>b)&(temp<B)]))))
    
    for(gamma in gamma_range){
      df.Base$ThVarLow<-temp*(temp<=gamma_opt2)
      df.Base$ThVarMed<-temp*((temp>gamma_opt2)&(temp<=gamma))
      df.Base$ThVarHig<-temp*(temp>gamma)
      
      p.Base<-pdata.frame(df.Base,index=c("pays","date"))
      grun.fe <- plm(frm, data = p.Base, model = "within")
      Lik2<-sum((residuals(grun.fe))^2)
      
      if(Lik2<Lik) {
        Lik<-Lik2
        gamma_opt<-gamma
        grun.fe_opt<-grun.fe
      } 
    }
    
    df.Base$ThVarLow<-temp*(temp<=min(gamma_opt,gamma_opt2))
    df.Base$ThVarMed<-temp*((temp>min(gamma_opt,gamma_opt2))&(temp<=max(gamma_opt,gamma_opt2)))
    df.Base$ThVarHig<-temp*(temp>max(gamma_opt,gamma_opt2))
    
    X<-NULL
    if(length(fVars)>0){
      for(i in 1:length(fVars)){
        frm<-as.formula(paste(" ~ ", paste(fVars[i], collapse= "+"),"-1"))
        X<-cbind(X,model.matrix(frm, data=df.Base)[,-1])
      }
      df.Base<-cbind(df.Base,X)
    }
    
    s<-residuals(grun.fe_opt)
    df.Base$resid<-s[match(rownames(df.Base),names(s))]
    s<-fixef(grun.fe_opt)
    df.Base$fixef<-s[match(df.Base$pays,names(s))]
    
    df.Base$y_esti<-as.numeric(df.Base$fixef)+as.numeric(cbind(as.matrix(df.Base[,c("ThVarLow","ThVarMed","ThVarHig",Vars)]),X)%*%coefficients(grun.fe_opt))
    df.Base$uit_hat<-as.numeric(df.Base$y)-as.numeric(cbind(as.matrix(df.Base[,c("ThVarLow","ThVarMed","ThVarHig",Vars)]),X)%*%coefficients(grun.fe))
    
    gamma_opt<-c(gamma_opt,gamma_opt2)
    gamma_opt<-sort(gamma_opt)
    SCE <-c(SCE,sum((residuals(grun.fe_opt))^2))
    res<-list(Base=df.Base,Threshold=gamma_opt,SCE=SCE,Estim=grun.fe_opt,Estim1=opt)
  }
  
  return(res)
}

#### Resampling fonction

f.Ti<-function(df.Base,Var="state"){ # Fonction to add the number of Ti to each state or country
  # df.Base is a dataBase
  # Vars is the Identification variable name : should be the country or state etc..
  
  PAYS<-names(table(df.Base[[Var]]))
  df.Base$T_i<-NA
  
  X<-data.frame(PAYS=PAYS,T_i=NA)
  
  for(i in 1:length(PAYS)){
    d.temp<-df.Base[df.Base[[Var]]==PAYS[i],]
    df.Base$T_i[df.Base[[Var]]==PAYS[i]]<-nrow(d.temp)
    X$T_i[i]<-nrow(d.temp)
  }
  
  return(list(COUNTRY=X,Base=df.Base))
}

f.Resampling<-function(df.Base,Var="state"){ # Resampling function.
  # df.Base is a dataBase
  # Vars is the Identification variable name : should be the country or state etc..
  
  X<-f.Ti(df.Base,Var)$COUNTRY
  X$PAYS<-as.character(X$PAYS)
  N_i_range<-table(X$T_i)
  Y<-rep("",nrow(X))
  k<-1
  for(i in 1:length(N_i_range)){
    l<-N_i_range[i]
    temp<-X$PAYS[X$T_i==as.numeric(names(N_i_range[i]))]
    Y[k:(k+l-1)]<-sample(temp,l,replace=TRUE)
    k<-k+l
  }
  
  return(list(sample=Y,T_i=cbind(Y,X$T_i)))
}

#### Bootstrap fonction

f.Boot<-function(p.df,ThVar,trim,DVar,Vars,B,nTh=1){
  # p.df is a panel dataframe Utilise function "pdata.frame" of package "plm" to create a panel dataframe
  # ThVar represent the name of Threshold varianble : Threshold variable is the only one affected by threshold
  # trim is the pourcentage of data to let before and after for the range of Threshold (trim in (0,1))
  # DVar represent the name of dependant varianble
  # Vars represent the names of independantes variables
  # B is the number of bootstrap to made
  
  ind<-names(index(p.df))
  df.Base<-data.frame(pays=p.df[[ind[1]]],date=p.df[[ind[2]]],y=p.df[[DVar]])
  df.Base<-cbind(df.Base,p.df[,match(c(ThVar,Vars),names(p.df))])
  p.Base<-pdata.frame(df.Base,index=c("pays","date"))
  
  # Estimation of the model without thresold (model_0)
  model0<-f.pmodel(p.df=p.Base,DVar="y",Vars=c(ThVar,Vars))
  S0<-model0$SCE
  df.Base<-model0$Base
  df.Base<-f.Ti(df.Base,Var="pays")$Base # Add the time length of each country
  print("=============  Simple Model  ==========")
  print(summary(model0$Estim))
  
  # Estimation of the model with thresold (model_1)
  model1<-f.Thpmodel(p.Base,ThVar,trim,DVar="y",Vars)
  S1<-model1$SCE
  print("===========  Threshold Model  ==========")
  print(summary(model1$Estim))
  gamma_opt<-model1$Threshold
  df.Base$resid<-model1$Base$resid
  print(paste0("=== The threshold is ", gamma_opt, " ==="))
  sig<-(1/(df.residual(model1$Estim)+length(coefficients(model1$Estim))))*S1
  
  F0<-(S0-S1)/sig
  print(paste0("=== The statistique F0 is ", round(F0,3), " ==="))
  
  ### Bootstrapping
  p.Base<-pdata.frame(df.Base,index=c("pays","date"))
  p.Base<-p.Base[order(p.Base$T_i,p.Base$pays,p.Base$date),]
  gamma<-numeric(B)
  Fi<-numeric(B)

  for(b in 1:B){
    Y<-f.Resampling(df.Base=p.Base,Var="pays")$sample
    resids<-NULL

    for(y in 1:length(Y)) {
      resids<-c(resids,as.numeric(p.Base$resid[p.Base$pays==Y[y]]))
    }

    p.Base$y_hat<-p.Base$y_esti+resids

    # Estimation of the model without thresold (model_0)
    model_0 <- f.pmodel(p.Base,"y_hat",c(ThVar,Vars))
    S0<-model_0$SCE

    # Estimation of the model with thresold (model_1)
    model_1<-f.Thpmodel(p.Base,ThVar,trim,"y_hat",Vars)
    S1<-model_1$SCE
    gamma[b]<-model_1$Threshold
    sig<-(1/(df.residual(model_1$Estim)+length(coefficients(model_1$Estim))))*S1

    # Calcul de la statistique Fi
    Fi[b]<-(S0-S1)/sig

    print(paste0("=== Bootstrap ", b, " : Fi = ", round(Fi[b],3), " ==="))
  }

  p_value<- sum(F0<Fi)/B
  Fi_1<-quantile(Fi,na.rm=TRUE,probs=0.99)
  Fi_5<-quantile(Fi,na.rm=TRUE,probs=0.95)
  Fi_10<-quantile(Fi,na.rm=TRUE,probs=0.9)

  print(paste0("=== P-value : ", p_value, " ==="))
  print(paste0("=== Critical Value : === 99 % : ", round(Fi_1,3), ", === 95 % : ", round(Fi_5,3), ", === 90 % : ", round(Fi_10,3)))

  plot(Fi,type="l")
  lines(rep(F0,B),col="red",type="l",lty=2)

  res<-list(model0=model0$Estim,model1=model1$Estim,Threshold=gamma_opt,F0=F0,Fi=Fi,p_value=p_value)
  
  if(nTh>1){
    F0i<-Fi
    # Estimation of the model with thresold (model_1)
    model2<-f.Thpmodel(p.Base,ThVar,trim,DVar="y",Vars,nTh=nTh)
    S1<-model2$SCE[1]
    S2<-model2$SCE[2]
    print("===========  Double Threshold Model  ==========")
    print(summary(model2$Estim))
    gamma_opt<-model2$Threshold
    df.Base<-model1$Base
    df.Base[[ThVar]]<-df.Base$ThVarLow+df.Base$ThVarHig
    df.Base<-df.Base[,names(df.Base) %!in% c("ThVarLow","ThVarHig")]
    df.Base<-f.Ti(df.Base,Var="pays")$Base # Add the time length of each country
    df.Base$resid<-model2$Base$resid
    print(paste0("=== The threshold is ", gamma_opt, " ==="))
    sig<-(1/(df.residual(model2$Estim)+length(coefficients(model2$Estim))))*S2
    
    F1<-(S1-S2)/sig
    print(paste0("=== The statistique F1 is ", round(F1,3), " ==="))
    
    ### Bootstrapping
    p.Base<-pdata.frame(df.Base,index=c("pays","date"))
    p.Base<-p.Base[order(p.Base$T_i,p.Base$pays,p.Base$date),]
    gamma<-numeric(B)
    Fi<-numeric(B)
    
    for(b in 1:B){
      Y<-f.Resampling(df.Base=p.Base,Var="pays")$sample
      resids<-NULL
      
      for(y in 1:length(Y)) {
        resids<-c(resids,as.numeric(p.Base$resid[p.Base$pays==Y[y]]))
      }
      
      p.Base$y_hat<-p.Base$y_esti+resids
      
      model_1<-f.Thpmodel(p.df=p.Base,ThVar=ThVar,trim=trim,DVar="y_hat",Vars=Vars,nTh=nTh)
      S1<-model_1$SCE[1]
      S2<-model_1$SCE[2]
      sig<-(1/(df.residual(model_1$Estim)+length(coefficients(model_1$Estim))))*S2
      
      # Calcul de la statistique Fi
      Fi[b]<-(S1-S2)/sig
      
      print(paste0("=== Bootstrap ", b, " : Fi = ", round(Fi[b],3), " ==="))
    }
    
    p_value<- sum(F1<Fi)/B
    Fi_1<-quantile(Fi,na.rm=TRUE,probs=0.99)
    Fi_5<-quantile(Fi,na.rm=TRUE,probs=0.95)
    Fi_10<-quantile(Fi,na.rm=TRUE,probs=0.9)
    
    print(paste0("=== P-value : ", p_value, " ==="))
    print(paste0("=== Critical Value : === 99 % : ", round(Fi_1,3), ", === 95 % : ", round(Fi_5,3), ", === 90 % : ", round(Fi_10,3)))
    
    plot(Fi,type="l")
    lines(rep(F1,B),col="red",type="l",lty=2)
    
    res<-list(model0=model0$Estim,model1=model1$Estim,model2=model2$Estim,Threshold=gamma_opt,F0=F0,F0i=F0i,F1=F1,F1i=Fi,p_value=p_value)
  }
  
  return(res)
}

# A function to demean matrix M
f.Demean <- function(M){
  MDmean <- M - as.matrix(rep(1,nrow(M)),nrow=nrow(M),ncol=ncol(M))%*%colMeans(M)
}

f.Var<-function(p.df,DVar,Vars,ZVars,ThVar=NULL,trim=0.05,nTh=1){
  
  ### Step 1 : First step estimation
  if(is.null(ThVar)){
    Step1Reg<-f.pmodel(p.df=p.df,DVar=DVar,Vars=Vars)
  }else{
    Step1Reg<-f.Thpmodel(p.df=p.df,ThVar=ThVar,trim=trim,DVar=DVar,Vars=Vars,nTh=nTh)
  }
  
  print("=============  First Step Model  ==========")
  print(summary(Step1Reg$Estim))
  
  ### Step 2 : Second step estimation
  df.Base<-Step1Reg$Base
  ind<-names(index(df.Base))
  type<-lapply(df.Base[,Vars],function(x) setdiff(class(x), "pseries"))
  if(length(Vars)==1) type<-setdiff(class(df.Base[,Vars]), "pseries")
  fVars<-Vars[which(type=="factor")]
  
  df.Base<-df.Base[,!(names(df.Base) %in% fVars)]
  # Mean of all variable of x by state
  df.Base<-df.Base %>%
    group_by(pays) %>%
    summarise_at(vars(-date),list(bar = ~ mean(.x, na.rm = TRUE)))
  
  for(i in 1:length(ZVars)){
    df.Base[[ZVars[i]]]<-p.df[match(df.Base$pays,p.df[[names(index(p.df))[1]]]),ZVars[i]]
  }
  
  frm<-as.formula(paste("uit_hat_bar ~ ", paste(ZVars, collapse= "+")))
  Step2Reg <- lm(frm,data = df.Base)
  
  print("=============  Second Step Model without variance correction  ==========")
  print(summary(Step2Reg))
  
  ### Step 3: Inference: compute the variance of Gamma_hat
  
  type<-lapply(df.Base[,ZVars],function(x) setdiff(class(x), "pseries"))
  if(length(ZVars)==1) type<-setdiff(class(df.Base[,ZVars]), "pseries")
  temp<-ZVars[1]
  fZVars<-ZVars[which(type=="factor")]
  ZVars<-ZVars[which(type=="numeric")]
  
  X<-NULL
  if(length(fZVars)>0){
    for(i in 1:length(fZVars)){
      frm<-as.formula(paste(" ~ ", paste(fZVars[i], collapse= "+"),"-1"))
      X<-cbind(X,model.matrix(frm, data=df.Base)[,-1])
    }
    df.Base<-df.Base[,names(df.Base) %!in% fZVars]
    fZVars<-colnames(X)
    df.Base<-cbind(df.Base,X)
  }
  
  Object.Z <- Step2Reg$model[temp] # Independent variables in step 2
  df.Base<-df.Base[as.numeric(row.names(Object.Z)),]
  NN <- nrow(Object.Z) # number N of observations at step 2
  Object.Z <- as.matrix(df.Base[,c(ZVars,fZVars)])
  RowNames <- df.Base[["pays"]]
  X_bar_bar<-colMeans(as.matrix(Step1Reg$Base[Step1Reg$Base$pays %in% RowNames,names(Step1Reg$Base) %!in% c("pays","date",fVars)]),na.rm = TRUE)
  # df.Base<-cbind(df.Base,rep(1,nrow(df.Base))%*%t(X_bar_bar))
  
  # Object QZZN
  QZZN <- (1/NN)*t(f.Demean(Object.Z))%*%(f.Demean(Object.Z))
  
  # Object.Yi_bar and Object.Xi_bar
  Object.Yi_bar <- as.matrix(df.Base[,"y_bar"])
  Object.Xi_bar <- as.matrix(df.Base[,-c(1:2,(ncol(df.Base)-3-length(ZVars)-length(fZVars)):(ncol(df.Base)))])

  # Object Zetai = Yi_bar - Y_bar + (Yi_bar - Y_bar)beta_hat + (Zi_bar - Z_bar)gamma_FFE
  Y_bar <- X_bar_bar['y']
  X_bar <- X_bar_bar[-c(1,(length(X_bar_bar)-3):(length(X_bar_bar)))]
  
  Object.Zetai <- Object.Yi_bar - Y_bar -
    as.matrix(Object.Xi_bar - as.matrix(rep(1,nrow(Object.Xi_bar)))%*%X_bar)%*%coefficients(Step1Reg$Estim) -
    as.matrix(f.Demean(Object.Z))%*%(coefficients(Step2Reg)[-1])# to be corrected
  Object.Zetai <- as.matrix(Object.Zetai%*%rep(1,length(ZVars)+length(fZVars)))
  Zeta_bar <- mean(Object.Zetai)
  
  # VZZN
  VZZN <- (1/NN)*t(Object.Zetai*(f.Demean(Object.Z)))%*%(Object.Zetai*(f.Demean(Object.Z)))
  
  # QZXN
  QZXN <- (1/NN)*t(f.Demean(Object.Z))%*%(Object.Xi_bar - as.matrix(rep(1,nrow(Object.Xi_bar)))%*%X_bar)
  #Resultat très intriguant. A vérifier.
  
  # QZXN2 <- (1/NN)*t(f.Demean(Object.Z))%*%(f.Demean(Object.Xi_bar))
  # 
  # A<-(Object.Xi_bar - as.matrix(rep(1,nrow(Object.Xi_bar)))%*%X_bar)
  # B<-(f.Demean(Object.Xi_bar))
  # 
  # K<-t(f.Demean(Object.Z))
  # 
  # A1<-t(f.Demean(Object.Z))%*%A
  # B1<-t(f.Demean(Object.Z))%*%B
  
  # Object: Var_beta_har
  Var_beta_hat <- Step1Reg$Estim$vcov
  
  # Variance of the second step estimation
  Var_gamma_hat <- (1/NN)*(solve(QZZN))%*%(VZZN + QZXN%*%(NN*Var_beta_hat)%*%t(QZXN))%*%solve(QZZN)
  
  TStat <- Step2Reg$coefficients[2:length(Step2Reg$coefficients)]/sqrt(diag(Var_gamma_hat))
  TStat <- rbind(Estimate = coefficients(Step2Reg)[-1], `Std. Error` = sqrt(diag(Var_gamma_hat)), `t value` = TStat, `Pr(>|t|)` = 2-2*pt(abs(TStat),Step2Reg$df.residual))
  
  print("=============  Second Step Model with variance correction  ==========")
  print(t(TStat))
  
  return(list(Step1Estim=Step1Reg$Estim,Step2Estim=Step2Reg,Step1Base=Step1Reg$Base,Step2Base=df.Base, TStat=t(TStat)))
  
}
  
  