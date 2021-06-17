source("functions.R")

d.Base<-read_csv("Data/DATABASE_NON_BALANCED.csv")
vars<-c("gsem","mr","phm","sd","lockdown", "gseml5","mrl5","phml5","sdl5","lockdownl5")
d.Base[vars]<-lapply(d.Base[vars], factor)
for(i in 1:length(vars)){
  d.Base[[vars[i]]] <- relevel(d.Base[[vars[i]]], ref = "None")
}

p.Base<-pdata.frame(d.Base,index=c("state","DateBon"))

# Available functions:
  
  # f.Var := les deux étapes d'estimation
  # f.pmodel := panel sans threshold
  # f.Thmodel := threshold panel model
  # f.Boot := bootstrap pour tester la significativité du threshold

G<-f.Var(p.df=p.Base,DVar="gth",Vars=c("gseml5","lockdownl5"),ZVars=c("gdpgrowthannual","currenthealthexpenditureofgdp201"),ThVar="templ5",trim=0.05)

A<-f.pmodel(p.df=p.Base,DVar="gth",Vars=c("templ5"))

B<-f.Thpmodel(p.df=p.Base,ThVar="templ5",trim=0.05,DVar="gth",Vars=c("gseml5","lockdownl5"))

C<-f.Ti(df.Base=p.Base)

D<-f.Resampling(df.Base=p.Base)

set.seed(185)
E<-f.Boot(p.df=p.Base,ThVar="templ5",trim=0.05,DVar="gth",Vars=c("gseml5","lockdownl5"),B=500)


# estimations and variable selection

 
Variables <- c("templ5")  # average daily temperature
A<-f.pmodel(p.df=p.Base,DVar="gth",Vars=Variables)
summary(A$Estim) # neg and sign.

d.Base$tempvarl5 <- d.Base$maxl5 - d.Base$minl5
p.Base<-pdata.frame(d.Base,index=c("state","DateBon"))


Variables <- c("templ5","tempvarl5")  # + temperature variation
A<-f.pmodel(p.df=p.Base,DVar="gth",Vars=Variables)
summary(A$Estim) # neg and sign.

Variables <- c("templ5","tempvarl5","prcp")  # + temperature variation+prcp
A<-f.pmodel(p.df=p.Base,DVar="gth",Vars=Variables)
summary(A$Estim) # precipitation has positive effect (??)

Variables <- c("templ5","tempvarl5","wdsp")  # + temperature variation+wind speed
A<-f.pmodel(p.df=p.Base,DVar="gth",Vars=Variables)
summary(A$Estim) # wind has positive effect, but more sign. than precipitation

Variables <- c("templ5","tempvarl5","stp")  # + temperature variation+pressure
A<-f.pmodel(p.df=p.Base,DVar="gth",Vars=Variables)
summary(A$Estim) # pressure has positive effect, but not sign.

Variables <- c("templ5","tempvarl5","wdsp","prcp")  # + temperature variation+wdsp+prcp
A<-f.pmodel(p.df=p.Base,DVar="gth",Vars=Variables)
summary(A$Estim) # both two good, but tempvar no more sign.

Variables <- c("templ5","wdsp","prcp")  # temp + wdsp+prcp+precipitation
A<-f.pmodel(p.df=p.Base,DVar="gth",Vars=Variables)
summary(A$Estim) # all are sign.

Variables <- c("templ5","tempvarl5","wdsp","prcp","stp")  # + temperature variation+wdsp+prcp+pressure
A<-f.pmodel(p.df=p.Base,DVar="gth",Vars=Variables)
summary(A$Estim) # only temp and tempvar sign.


# Recap pour  climatic variables:"templ5","wdsp","prcp"
Variables <- c("templ5","wdspl5","prcpl5")  # temp + wdsp+prcp
A<-f.pmodel(p.df=p.Base,DVar="gth",Vars=Variables)
summary(A$Estim) # all are sign.

# GOVERNMENT MEASURES
Variables <- c("templ5","wdspl5","prcpl5","lockdownl5")  # temp + wdsp+prcp+lockdown
A<-f.pmodel(p.df=p.Base,DVar="gth",Vars=Variables)
summary(A$Estim) # all are sign and good.

Variables <- c("templ5","wdspl5","prcpl5","lockdownl5","mrl5")  # temp + wdsp+prcp+lockdown+mr
A<-f.pmodel(p.df=p.Base,DVar="gth",Vars=Variables)
summary(A$Estim) # all are sign and good.

Variables <- c("templ5","wdspl5","prcpl5","lockdownl5","mrl5","sdl5")  # temp + wdsp+prcp+lockdown+mr+sd
A<-f.pmodel(p.df=p.Base,DVar="gth",Vars=Variables)
summary(A$Estim) # all are sign and good, but ....

Variables <- c("templ5","wdspl5","prcpl5","lockdownl5","mrl5","sdl5","gseml5")  # temp + wdsp+prcp+lockdown+mr+sd + gsem
A<-f.pmodel(p.df=p.Base,DVar="gth",Vars=Variables)
summary(A$Estim) # all are sign and good, but ....

Variables <- c("templ5","wdspl5","prcpl5","lockdownl5","mrl5","sdl5","gseml5","phml5")  # temp + wdsp+prcp+lockdown+mr+sd + gsem + phm
A<-f.pmodel(p.df=p.Base,DVar="gth",Vars=Variables)
summary(A$Estim) # lockdown is no more sign; and phm has positive sign. ...

Variables <- c("templ5","wdspl5","prcpl5","mrl5","sdl5","gseml5","phml5")  # temp + wdsp+prcp+mr+sd + gsem + phm (-lockdown)
A<-f.pmodel(p.df=p.Base,DVar="gth",Vars=Variables)
summary(A$Estim) # prcp is no more sign. ...

Variables <- c("templ5","wdspl5","mrl5","sdl5","gseml5","phml5")  # temp + wdsp+prcp+mr+sd + gsem + phm (-lockdown) (- prcp)
A<-f.pmodel(p.df=p.Base,DVar="gth",Vars=Variables)
summary(A$Estim) # all are good. ...

Variables <- c("templ5","wdspl5","lockdownl5","mrl5","sdl5","gseml5","phml5")  # temp + wdsp+prcp+mr+sd + gsem + phm (+ockdown) (- prcp)
A<-f.pmodel(p.df=p.Base,DVar="gth",Vars=Variables)
summary(A$Estim) # lockdown is not signic. ...

Variables <- c("templ5","wdspl5","lockdownl5","mrl5","sdl5","gseml5")  # temp + wdsp+prcp+mr+sd + gsem  (+ockdown) (- prcp) (-phm)
A<-f.pmodel(p.df=p.Base,DVar="gth",Vars=Variables)
summary(A$Estim) # 

Variables <- c("templ5","wdspl5","prcpl5","gseml5","lockdownl5","mrl5")  # temp + wdsp+prcp+mr+sd + gsem  (+ockdown) (- prcp) (-phm)
A<-f.pmodel(p.df=p.Base,DVar="gth",Vars=Variables)
summary(A$Estim) # 

Variables <- c("templ5","wdspl5","prcpl5","gseml5","lockdownl5","phml5")  # temp + wdsp+prcp+mr+sd + gsem  (+ockdown) (- prcp) (-phm)
A<-f.pmodel(p.df=p.Base,DVar="gth",Vars=Variables)
summary(A$Estim) # 

# RECAP: CLIMATE:= (temp,wdsp, prcp) and GVT MEASURES:=(gsem,lockdown,phm)
Variables <- c("templ5","wdspl5","prcpl5","gseml5","lockdownl5","phml5")  # temp + wdsp+prcp+mr+sd + gsem + phm (-lockdown) (- prcp)
A<-f.pmodel(p.df=p.Base,DVar="gth",Vars=Variables)
summary(A$Estim) # all are good. ...

# TIME-INVARIANT FACTORS ...

Variables <- c("templ5","wdspl5","prcpl5","gseml5","lockdownl5","phml5")  # temp + wdsp+prcp+mr+sd + gsem + phm (-lockdown) (- prcp)
Zvariables <- c("gdppercapitapppcurrentinternatio") # GDP per cap. PPP
G<-f.Var(p.df=p.Base,DVar="gth",Vars=Variables,ZVars=Zvariables,ThVar=NULL,trim=0.05)
# non sign.

Variables <- c("templ5","wdspl5","prcpl5","gseml5","lockdownl5","phml5")  # temp + wdsp+prcp+mr+sd + gsem + phm (-lockdown) (- prcp)
Zvariables <- c("gdppercapitaconstant2010us") # GDP per cap. 2010 US $
G<-f.Var(p.df=p.Base,DVar="gth",Vars=Variables,ZVars=Zvariables,ThVar=NULL,trim=0.05)
# sign. but unexpected sign.

Variables <- c("templ5","wdspl5","prcpl5","gseml5","lockdownl5","phml5")  # temp + wdsp+prcp+mr+sd + gsem + phm (-lockdown) (- prcp)
Zvariables <- c("gdpconstant2010us") # GDP 2010 US $
G<-f.Var(p.df=p.Base,DVar="gth",Vars=Variables,ZVars=Zvariables,ThVar=NULL,trim=0.05)
# Good in second step

Variables <- c("templ5","wdspl5","prcpl5","gseml5","lockdownl5","phml5")  # temp + wdsp+prcp+mr+sd + gsem + phm (-lockdown) (- prcp)
Zvariables <- c("gdpconstant2010us","gdppercapitapppcurrentinternatio") # GDP 2010 US $ + GDP per capita PPP
G<-f.Var(p.df=p.Base,DVar="gth",Vars=Variables,ZVars=Zvariables,ThVar=NULL,trim=0.05)
# Good in second step, but ... GDP per capita has positive effect

Variables <- c("templ5","wdspl5","prcpl5","gseml5","lockdownl5","phml5")  # temp + wdsp+prcp+mr+sd + gsem + phm (-lockdown) (- prcp)
Zvariables <- c("gdpconstant2010us","gdppercapitaconstant2010us") # GDP 2010 US $ + GDP per capita PPP
G<-f.Var(p.df=p.Base,DVar="gth",Vars=Variables,ZVars=Zvariables,ThVar=NULL,trim=0.05)
# error: sigularity

#Variables <- c("templ5","wdspl5","mr","sd","gseml5","phml5")  # temp + wdsp+prcp+mr+sd + gsem + phm (-lockdown) (- prcp)
Zvariables <- c("gdpconstant2010us","density") # GDP 2010 US $ + GDP per capita PPP + density
G<-f.Var(p.df=p.Base,DVar="gth",Vars=Variables,ZVars=Zvariables,ThVar=NULL,trim=0.05)
# Erro message because of density ?

#Variables <- c("templ5","wdspl5","mr","sd","gseml5","phml5")  # temp + wdsp+prcp+mr+sd + gsem + phm (-lockdown) (- prcp)
Zvariables <- c("density") # only density
G<-f.Var(p.df=p.Base,DVar="gth",Vars=Variables,ZVars=Zvariables,ThVar=NULL,trim=0.05)
# sign. but negative sign

#Variables <- c("templ5","wdspl5","mr","sd","gseml5","phml5")  # temp + wdsp+prcp+mr+sd + gsem + phm (-lockdown) (- prcp)
Zvariables <- c("gdpconstant2010us","density") # GDP 2010 US $ + density
G<-f.Var(p.df=p.Base,DVar="gth",Vars=Variables,ZVars=Zvariables,ThVar=NULL,trim=0.05)
# Erro message 

#Variables <- c("templ5","wdspl5","mr","sd","gseml5","phml5")  # temp + wdsp+prcp+mr+sd + gsem + phm (-lockdown) (- prcp)
Zvariables <- c("gdppercapitapppcurrentinternatio","density") # GDP per capita PPP + density
G<-f.Var(p.df=p.Base,DVar="gth",Vars=Variables,ZVars=Zvariables,ThVar=NULL,trim=0.05)
# Erro message 

#Variables <- c("templ5","wdspl5","mr","sd","gseml5","phml5")  # temp + wdsp+prcp+mr+sd + gsem + phm (-lockdown) (- prcp)
Zvariables <- c("currenthealthexpenditureofgdp201") # Gonly health exp.
G<-f.Var(p.df=p.Base,DVar="gth",Vars=Variables,ZVars=Zvariables,ThVar=NULL,trim=0.05)
# sign. but positive sign

Variables <- c("templ5","wdspl5","mr","sd","gseml5","phml5")  # temp + wdsp+prcp+mr+sd + gsem + phm (-lockdown) (- prcp)
Zvariables <- c("currenthealthexpenditureofgdp201","density") # Gonly health exp. + density
G<-f.Var(p.df=p.Base,DVar="gth",Vars=Variables,ZVars=Zvariables,ThVar=NULL,trim=0.05)
# sign. but unexpected sign

#Variables <- c("templ5","wdspl5","mr","sd","gseml5","phml5")  # temp + wdsp+prcp+mr+sd + gsem + phm (-lockdown) (- prcp)
Zvariables <- c("gdppercapitapppcurrentinternatio","currenthealthexpenditureofgdp201") # GDP per cap. PPP+health exp.
G<-f.Var(p.df=p.Base,DVar="gth",Vars=Variables,ZVars=Zvariables,ThVar=NULL,trim=0.05)
# error

#Variables <- c("templ5","wdspl5","mr","sd","gseml5","phml5")  # temp + wdsp+prcp+mr+sd + gsem + phm (-lockdown) (- prcp)
Zvariables <- c("gdpconstant2010us","currenthealthexpenditureofgdp201") # GDP 2010 US $ + health exp.
G<-f.Var(p.df=p.Base,DVar="gth",Vars=Variables,ZVars=Zvariables,ThVar=NULL,trim=0.05)
# error

#Variables <- c("templ5","wdspl5","mr","sd","gseml5","phml5")  # temp + wdsp+prcp+mr+sd + gsem + phm (-lockdown) (- prcp)
Zvariables <- c("gdppercapitaconstant2010us","currenthealthexpenditureofgdp201") # GDP 2010 US $ + health exp.
G<-f.Var(p.df=p.Base,DVar="gth",Vars=Variables,ZVars=Zvariables,ThVar=NULL,trim=0.05)
# 

#Variables <- c("templ5","wdspl5","mr","sd","gseml5","phml5")  # temp + wdsp+prcp+mr+sd + gsem + phm (-lockdown) (- prcp)
Zvariables <- c("temp_sd") # temp variability
G<-f.Var(p.df=p.Base,DVar="gth",Vars=Variables,ZVars=Zvariables,ThVar=NULL,trim=0.05)
# good in second step

#Variables <- c("templ5","wdspl5","mr","sd","gseml5","phml5")  # temp + wdsp+prcp+mr+sd + gsem + phm (-lockdown) (- prcp)
Zvariables <- c("gdpconstant2010us","temp_sd") # temp variability
G<-f.Var(p.df=p.Base,DVar="gth",Vars=Variables,ZVars=Zvariables,ThVar=NULL,trim=0.05)
# error: singularity

#Variables <- c("templ5","wdspl5","mr","sd","gseml5","phml5")  # temp + wdsp+prcp+mr+sd + gsem + phm (-lockdown) (- prcp)
Zvariables <- c("gdppercapitaconstant2010us","temp_sd") # temp variability
G<-f.Var(p.df=p.Base,DVar="gth",Vars=Variables,ZVars=Zvariables,ThVar=NULL,trim=0.05)
# 

#Variables <- c("templ5","wdspl5","mr","sd","gseml5","phml5")  # temp + wdsp+prcp+mr+sd + gsem + phm (-lockdown) (- prcp)
Zvariables <- c("gdppercapitaconstant2010us","temp_sd","currenthealthexpenditureofgdp201") # temp variability
G<-f.Var(p.df=p.Base,DVar="gth",Vars=Variables,ZVars=Zvariables,ThVar=NULL,trim=0.05)
# 

#Variables <- c("templ5","wdspl5","mr","sd","gseml5","phml5")  # temp + wdsp+prcp+mr+sd + gsem + phm (-lockdown) (- prcp)
Zvariables <- c("temp_sd","currenthealthexpenditureofgdp201") # temp variability
G<-f.Var(p.df=p.Base,DVar="gth",Vars=Variables,ZVars=Zvariables,ThVar=NULL,trim=0.05)
# 

#Variables <- c("templ5","wdspl5","mr","sd","gseml5","phml5")  # temp + wdsp+prcp+mr+sd + gsem + phm (-lockdown) (- prcp)
Zvariables <- c("temp_sd","currenthealthexpenditurepercapit") # temp variability
G<-f.Var(p.df=p.Base,DVar="gth",Vars=Variables,ZVars=Zvariables,ThVar=NULL,trim=0.05)
# 

#Variables <- c("templ5","wdspl5","mr","sd","gseml5","phml5")  # temp + wdsp+prcp+mr+sd + gsem + phm (-lockdown) (- prcp)
Zvariables <- c("temp_sd","currenthealthexpenditurepercapit","gdppercapitaconstant2010us") # temp variability
G<-f.Var(p.df=p.Base,DVar="gth",Vars=Variables,ZVars=Zvariables,ThVar=NULL,trim=0.05)
# 

#Variables <- c("templ5","wdspl5","mr","sd","gseml5","phml5")  # temp + wdsp+prcp+mr+sd + gsem + phm (-lockdown) (- prcp)
Zvariables <- c("temp_sd","currenthealthexpenditurepercapit","density","gdppercapitaconstant2010us") # temp variability
G<-f.Var(p.df=p.Base,DVar="gth",Vars=Variables,ZVars=Zvariables,ThVar=NULL,trim=0.05)
# 

p.Base$temp_sd_Low = p.Base$temp_sd_quali=="Low"
p.Base$temp_sd_Medium = p.Base$temp_sd_quali=="Medium"
p.Base$temp_sd_High = p.Base$temp_sd_quali=="High"

#Variables <- c("templ5","wdspl5","mr","sd","gseml5","phml5")  # temp + wdsp+prcp+mr+sd + gsem + phm (-lockdown) (- prcp)
Zvariables <- c("temp_sd_Medium","temp_sd_High") # temp variability
G<-f.Var(p.df=p.Base,DVar="gth",Vars=Variables,ZVars=Zvariables,ThVar=NULL,trim=0.05)
# good in second step

Variables <- c("templ5","wdspl5","mr","sd","gseml5","phml5")  # temp + wdsp+prcp+mr+sd + gsem + phm (-lockdown) (- prcp)
Zvariables <- c("temp_sd_Low","temp_sd_Medium") # temp variability
G<-f.Var(p.df=p.Base,DVar="gth",Vars=Variables,ZVars=Zvariables,ThVar=NULL,trim=0.05)
# good in second step

Variables <- c("templ5","wdspl5","mr","sd","gseml5","phml5")  # temp + wdsp+prcp+mr+sd + gsem + phm (-lockdown) (- prcp)
Zvariables <- c("temp_sd_Low","temp_sd_Medium","gdpconstant2010us") # temp variability + GDP
G<-f.Var(p.df=p.Base,DVar="gth",Vars=Variables,ZVars=Zvariables,ThVar=NULL,trim=0.05)
# error: singularity

# RECAP: IN 2ND STEP: (temp_sd,currenthealthexpenditurepercapit,density ??,gdppercapitaconstant2010us ??)
Variables <- c("templ5","wdspl5","prcpl5","gseml5","lockdownl5","phml5")  # temp + wdsp+prcp+mr+sd + gsem + phm (-lockdown) (- prcp)
Zvariables <- c("temp_sd","currenthealthexpenditurepercapit") # ,"density","gdppercapitaconstant2010us"
G<-f.Var(p.df=p.Base,DVar="gth",Vars=Variables,ZVars=Zvariables,ThVar=NULL,trim=0.05)

# THRESHOLD
Variables <- c("wdspl5","prcpl5","gseml5","lockdownl5","phml5")  # temp + wdsp+prcp+mr+sd + gsem + phm (-lockdown) (- prcp)
Zvariables <- c("temp_sd","currenthealthexpenditurepercapit") # ,"density","gdppercapitaconstant2010us"
G<-f.Var(p.df=p.Base,DVar="gth",Vars=Variables,ZVars=Zvariables,ThVar="templ5",trim=0.05)
# 

Variables <- c("wdspl5","prcpl5","gseml5","lockdownl5","phml5")  # temp + wdsp+prcp+mr+sd + gsem + phm (-lockdown) (- prcp)
Zvariables <- c("temp_sd","currenthealthexpenditurepercapit") # ,"density","gdppercapitaconstant2010us"
G<-f.Var(p.df=p.Base,DVar="gth",Vars=Variables,ZVars=Zvariables,ThVar="templ5",trim=0.05,nTh = 2)
# 

E<-f.Boot(p.df=p.Base,ThVar="templ5",trim=0.05,DVar="gth",Vars=Variables,B=500,nTh = 2)

summary(E$model1)
summary(E$model2)


#############################################################
############   B A L A N C E D     P A N E L    #############
#############################################################

d.Base2<-NULL
N0<-50
PAYS<-names(table(d.Base$state)[table(d.Base$state)!=0])

vars<-c("temp","dewp","slp","stp","visib","wdsp","mxspd","gust","max","min","prcp","sndp","clic1","clic",
        "templ5","dewpl5","slpl5","stpl5","visibl5","wdspl5","mxspdl5","gustl5","maxl5","minl5","prcpl5","sndpl5")

for(i in 1:length(PAYS)){
  d.temp<-d.Base[d.Base$state==PAYS[i],]
  if(nrow(d.temp)>=50)  {
    d.temp$N0<-max(d.temp$DateBon)
    for(j in 2:nrow(d.temp)) if(is.na(d.temp$temp[j])) 
      d.temp[j,vars]<-d.temp[j-1,vars]
    for(j in 2:nrow(d.temp)) if(is.na(d.temp$prcpl5[j])) 
      d.temp$prcpl5[j]<-d.temp$prcpl5[j-1]
    d.Base2<-rbind.data.frame(d.Base2,d.temp,stringsAsFactors=F)
  }
}

pays<-names(table(d.Base2$state))
N0<-min(d.Base2$N0)

d.Base2<-d.Base2[d.Base2$DateBon<=N0,]
d.Base2$gth[(d.Base2$state=="NORTHERN TERRITORY")&(d.Base2$DateBon==3)]<-0
d.Base2$prcpl5[is.na(d.Base2$prcpl5)]<-d.Base2$prcpl5[4565]
d.Base2<-d.Base2[!is.na(d.Base2$gth),]

p.Base2<-pdata.frame(d.Base2,index=c("state","DateBon"))

sum(is.na(p.Base2$templ5))
sum(is.na(p.Base2$wdspl5))
sum(is.na(p.Base2$prcpl5))


# RECAP: IN 2ND STEP: (temp_sd,currenthealthexpenditurepercapit,density ??,gdppercapitaconstant2010us ??)
Variables <- c("wdspl5","prcpl5","gseml5","lockdownl5","phml5")  # temp + wdsp+prcp+mr+sd + gsem + phm (-lockdown) (- prcp)
Zvariables <- c("temp_sd","currenthealthexpenditurepercapit") # ,"density","gdppercapitaconstant2010us"
G<-f.Var(p.df=p.Base2,DVar="gth",Vars=c("templ5",Variables),ZVars=Zvariables,ThVar=NULL,trim=0.05,nTh = 2)
G1<-f.Var(p.df=p.Base2,DVar="gth",Vars=Variables,ZVars=Zvariables,ThVar="templ5",trim=0.05,nTh = 1)
G2<-f.Var(p.df=p.Base2,DVar="gth",Vars=Variables,ZVars=Zvariables,ThVar="templ5",trim=0.05,nTh = 2)

E<-f.Boot(p.df=p.Base2,ThVar="templ5",trim=0.05,DVar="gth",Vars=Variables,B=500,nTh = 2)

layout(matrix(c(1, 2), 1, 2), widths=c(4, 1))
par(mar=c(4.1, .5, .5, .5), oma=c(0, 4.1, 0, 0))
plot(E$F1i, type="s", xpd=NA, ylab="Statistics F1", xlab="Sample", las=1)
lines(rep(E$F1,length(E$F1i)),col="red",type="l",lty=2)
d<-density(E$F1i)
plot(d$y,d$x, type="l", yaxs="i", axes=FALSE, xlab="")
lines(y=rep(E$F1,15),x=seq(0,0.145,length=15),col="red",type="l",lty=2)

#============================
# TRESHOLD
#============================
t <- 49
nt <- nrow(d.Base2)
n <- nt/t


dep<- as.matrix(d.Base2$gth) # investment/assets
# th1<- as.matrix(d.Base.Util2$templ5) #Tobin's Q
d <- as.matrix(d.Base2$templ5) # Threshold variable
# ind2 <- cbind(d.Base.Util2$gseml5=="Weak",d.Base.Util2$gseml5=="Medium",d.Base.Util2$gseml5=="High", 
#               d.Base.Util2$lockdownl5=="Weak",d.Base.Util2$lockdownl5=="Meduim",d.Base.Util2$lockdownl5=="High") # regime-indep covariates:

ind2 <- cbind(d.Base2$gseml5=="High",d.Base2$gseml5=="Medium",d.Base2$gseml5=="Weak",
              d.Base2$lockdownl5=="High",d.Base2$lockdownl5=="Medium", d.Base2$lockdownl5=="Weak",
              d.Base2$phml5=="High",d.Base2$phml5=="Medium", d.Base2$phml5=="Weak") # regime-indep covariates:

bootn<-c(500,500,500)
trimn<-c(0.05,0.05,0.05) #trimmed percentage for each threshold esitmation
qn<-400
conf_lev<-0.95
Output=ptm(dep,d,ind2,d,bootn,trimn,qn,conf_lev,t,n)

Output[[1]]

