library(stockassessment)
library(Matrix)
library(gridExtra)
fit<- fitfromweb("WBSS_HAWG_2019")
source("stockassessment/R/forecast2.R")

nsim=5000
seed=12345
# To change between MS scenarios
Fmsy=0.31 # MSY=0.31, MAP_lower=0.216, MAP_upper=0.379 for Blim=120000 and MSYBtrig=150000 (2018 WBSS Advice)
FmsyL=0.216
FmsyH=0.379
RW=FALSE
Rdist=FALSE
F.RW=FALSE
ny=50

FC <- list()

set.seed(seed) # same seed to repeat output
cf.cv.keep.cv<-matrix(NA,ncol=4*2,nrow=ny+2)
cf.cv.keep.cv[2,] <- c(NA,NA,NA,NA,NA,12352,469,9001)
FC[[length(FC)+1]] <- forecast2(fit, fscale=c(1,NA,rep(NA,ny)), 
                                catchval=c(NA,23367,rep(NA,ny)),
                                fval=c(NA,NA,rep(Fmsy,ny)),
                                cf.cv.keep.cv=cf.cv.keep.cv, 
                                rec.years=2013:2017, 
                                label=paste0("cst Fmsy=",Fmsy,"_Rdist=",Rdist),
                                nosim=nsim,estimate=median, deterministic=FALSE,
                                RW=RW,
                                Rdist=Rdist,
                                F.RW=F.RW
)   


set.seed(seed) # same seed to repeat output
cf.cv.keep.cv<-matrix(NA,ncol=4*2,nrow=ny+2)
cf.cv.keep.cv[2,] <- c(NA,NA,NA,NA,NA,12352,469,9001)
FC[[length(FC)+1]] <- forecast2(fit, fscale=c(1,NA,rep(NA,ny)), 
                                catchval=c(NA,23367,rep(NA,ny)),
                                fval=c(NA,NA,rep(FmsyL,ny)),
                                cf.cv.keep.cv=cf.cv.keep.cv, 
                                rec.years=2013:2017, 
                                label=paste0("cst Fmsy=",FmsyL,"_Rdist=",Rdist),
                                nosim=nsim,estimate=median, deterministic=FALSE,
                                RW=RW,
                                Rdist=Rdist,
                                F.RW=F.RW
)   


set.seed(seed) # same seed to repeat output
cf.cv.keep.cv<-matrix(NA,ncol=4*2,nrow=ny+2)
cf.cv.keep.cv[2,] <- c(NA,NA,NA,NA,NA,12352,469,9001)
FC[[length(FC)+1]] <- forecast2(fit, fscale=c(1,NA,rep(NA,ny)), 
                                catchval=c(NA,23367,rep(NA,ny)),
                                fval=c(NA,NA,rep(FmsyH,ny)),
                                cf.cv.keep.cv=cf.cv.keep.cv, 
                                rec.years=2013:2017, 
                                label=paste0("cst Fmsy=",FmsyH,"_Rdist=",Rdist),
                                nosim=nsim,estimate=median, deterministic=FALSE,
                                RW=RW,
                                Rdist=Rdist,
                                F.RW=F.RW
)   





Rdist=TRUE

set.seed(seed) # same seed to repeat output
cf.cv.keep.cv<-matrix(NA,ncol=4*2,nrow=ny+2)
cf.cv.keep.cv[2,] <- c(NA,NA,NA,NA,NA,12352,469,9001)
FC[[length(FC)+1]] <- forecast2(fit, fscale=c(1,NA,rep(NA,ny)), 
                                catchval=c(NA,23367,rep(NA,ny)),
                                fval=c(NA,NA,rep(Fmsy,ny)),
                                cf.cv.keep.cv=cf.cv.keep.cv, 
                                rec.years=2013:2017, 
                                label=paste0("cst Fmsy=",Fmsy,"_Rdist=",Rdist),
                                nosim=nsim,estimate=median, deterministic=FALSE,
                                RW=RW,
                                Rdist=Rdist,
                                F.RW=F.RW
)   


set.seed(seed) # same seed to repeat output
cf.cv.keep.cv<-matrix(NA,ncol=4*2,nrow=ny+2)
cf.cv.keep.cv[2,] <- c(NA,NA,NA,NA,NA,12352,469,9001)
FC[[length(FC)+1]] <- forecast2(fit, fscale=c(1,NA,rep(NA,ny)), 
                                catchval=c(NA,23367,rep(NA,ny)),
                                fval=c(NA,NA,rep(FmsyL,ny)),
                                cf.cv.keep.cv=cf.cv.keep.cv, 
                                rec.years=2013:2017, 
                                label=paste0("cst Fmsy=",FmsyL,"_Rdist=",Rdist),
                                nosim=nsim,estimate=median, deterministic=FALSE,
                                RW=RW,
                                Rdist=Rdist,
                                F.RW=F.RW
)   


set.seed(seed) # same seed to repeat output
cf.cv.keep.cv<-matrix(NA,ncol=4*2,nrow=ny+2)
cf.cv.keep.cv[2,] <- c(NA,NA,NA,NA,NA,12352,469,9001)
FC[[length(FC)+1]] <- forecast2(fit, fscale=c(1,NA,rep(NA,ny)), 
                                catchval=c(NA,23367,rep(NA,ny)),
                                fval=c(NA,NA,rep(FmsyH,ny)),
                                cf.cv.keep.cv=cf.cv.keep.cv, 
                                rec.years=2013:2017, 
                                label=paste0("cst Fmsy=",FmsyH,"_Rdist=",Rdist),
                                nosim=nsim,estimate=median, deterministic=FALSE,
                                RW=RW,
                                Rdist=Rdist,
                                F.RW=F.RW
) 


SSB<-array(dim=c(nsim,length(FC[[1]]),length(FC)))
Catch<-array(dim=c(nsim,length(FC[[1]]),length(FC)))
Rec<-array(dim=c(nsim,length(FC[[1]]),length(FC)))
Fbar<-array(dim=c(nsim,length(FC[[1]]),length(FC)))
for (k in 1:length(FC)){
  for (t in 1:length(FC[[1]])){ # each forecast year
    SSB[,t,k]<-FC[[k]][t][[1]]$ssb
    Catch[,t,k]<-FC[[k]][t][[1]]$catch
    Rec[,t,k]<-FC[[k]][t][[1]]$rec
    Fbar[,t,k]<-FC[[k]][t][[1]]$fbar
  }
}

risk.val<-array(dim=c(2,length(FC),length(FC[[1]]))) # coordinates for risk plot row1=x=risk, row2=median catch
for (k in 1:length(FC)){
  for (t in 1:length(FC[[1]])){
    risk.val[1,k,t] <- sum(SSB[,t,k]<Blim)/nrow(SSB) # prob SSB<Blim
    risk.val[2,k,t] <- median(Catch[,t,k]) # median catch
  }
}

years=2018:(2018+length(FC[[1]])-1)
t1=3 

setwd("~/DTU/Manuscripts/MSE_forecast_paper/results")

save.image("extra_eq_runs.RData")

#### Plot results in pdf ####


is.even <- function(x) x %% 2 == 0

pdf(file ="extra_eq_runs.pdf", width = 12, height = 12 )


par(mfrow=c(ceiling(length(FC)/2),2))
for (k in 1:length(FC)){
  fbarplot(FC[[k]], main=attr(FC[[k]],"label"))
  # abline(h=Fmsy,col="red",lty=2)
  # abline(h=Flow,col="blue",lty=2)
}
if(!is.even(length(FC))) plot.new()
for (k in 1:length(FC)){
  ssbplot(FC[[k]], main=attr(FC[[k]],"label"))
  abline(h=MSYBtrig,col="red",lty=2)
  abline(h=Blim,col="blue",lty=2)
  
}
if(!is.even(length(FC))) plot.new()
for (k in 1:length(FC)){
  catchplot(FC[[k]], main=attr(FC[[k]],"label"))
}
if(!is.even(length(FC))) plot.new()
for (k in 1:length(FC)){
  recplot(FC[[k]], main=attr(FC[[k]],"label"))
}

legend=c()
for (k in 1:length(FC)) legend=c(legend,attr(FC[[k]],"label"))
par(mfrow=c(1,1))
if(!is.even(length(FC))) plot.new()
#par(mfrow=c(2,ceiling((length(FC[[1]])-(t1-1))/2)),oma=c(4,4,2,1),mar=c(0,0,0,0))
#for (t in t1:length(FC[[1]])){
t=length(FC[[1]])
  plot(x=risk.val[1,,t],y=risk.val[2,,t],pch=1:(length(FC)),xlim=c(0,1),ylim=c(0,max(risk.val[2,,])*1.1),xaxt="n",yaxt="n", col=1:1:ncol(risk.val))
  text(x=0.5,y=max(risk.val[2,,]),labels = years[t],cex=2)
  legend("topleft",legend=legend,pch=1:(length(FC)) ,bty="n", col=1:ncol(risk.val))
  axis(2)
  axis(1)
#}
mtext("Probability SSB<Blim",side=1,line=3,outer=TRUE)
mtext("Median catch",side=2,line=3,outer=TRUE)


dev.off()


