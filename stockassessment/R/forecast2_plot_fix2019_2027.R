# fscale=NULL; catchval=NULL; fval=NULL; nosim=1000; year.base=max(fit$data$years); ave.years=max(fit$data$years)+(-4:0); rec.years=max(fit$data$years)+(-9:0); label=NULL; overwriteSelYears=NULL; deterministic=FALSE; 
# cf.cv.keep.cv=matrix(NA, ncol=2*sum(fit$data$fleetTypes==0), nrow=length(catchval)); cf.cv.keep.fv=matrix(NA, ncol=2*sum(fit$data$fleetTypes==0), nrow=length(catchval)); cf.keep.fv.offset=matrix(0, ncol=sum(fit$data$fleetTypes==0), nrow=length(catchval)); estimate=median

library(stockassessment)
library(Matrix)
fit<- fitfromweb("WBSS_HAWG_2018")

Blim=120000
MSYBtrig=150000
Fmsy=0.31 # MSY=0.31, MAP_lower=0.216, MAP_upper=0.379 for Blim=120000 and MSYBtrig=150000 (2018 WBSS Advice)
Flow=0.1



FC <- list()

set.seed(12345) # same seed to repeat output
cf.cv.keep.cv<-matrix(NA,ncol=4*2,nrow=11)
cf.cv.keep.cv[2,] <- c(NA,NA,NA,NA,NA,18272,1215,17309)
cf.cv.keep.cv[3,] <- c(NA,NA,NA,NA,NA,11085,1225,9001)
for (i in 4:nrow(cf.cv.keep.cv)){
  cf.cv.keep.cv[i,] <- cf.cv.keep.cv[3,] 
}
# util2=matrix(c(1,0.54,0.46,1), nrow=sum(fit$data$fleetTypes==0), ncol=8, byrow=FALSE)
# util2[,1:2]<-1
# util2[,4:8]=1
#catch fraction catch weight keeping total catch w
#There are ncol 4 fleets * 2 ways to define catches 1=fraction;  2=tons 
# only specify 3 because total  cv is given in "catchval" below
#************************ Here we iterate manually year by year with a new SSB each year *************************** 
FC[[length(FC)+1]] <- forecast2(fit, fscale=c(1,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA), 
                                catchval=c(NA,38354,23236,23236,23236,23236,23236,23236,23236,23236,23236),
                                fval=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
                                cf.cv.keep.cv=cf.cv.keep.cv, 
                                rec.years=2013:2017, 
                                label="Base scenario, Fixed 2019 TAC all years",
                                nosim=1000,estimate=median, deterministic=FALSE,
                                Fscenario=1,
                                #MSYBtrig=c(NA, NA, NA, MSYBtrig, MSYBtrig, MSYBtrig, MSYBtrig, MSYBtrig, MSYBtrig, MSYBtrig, MSYBtrig), 
                                #Blim=c(NA, NA, NA, Blim, Blim, Blim, Blim, Blim, Blim, Blim, Blim),
                                Fmsy=Fmsy,
                                Flow=Flow#, #for Fscenario= 3, 4 or 5
                                #util=util2 # for implementation error runs
                                #util_prop=c(1,0.54,0.46,1)
)   


set.seed(12345) # same seed to repeat output
cf.cv.keep.cv<-matrix(NA,ncol=4*2,nrow=11)
cf.cv.keep.cv[2,] <- c(NA,NA,NA,NA,NA,18272,1215,17309)
cf.cv.keep.cv[3,] <- c(NA,NA,NA,NA,NA,11085,1225,9001)
# util2=matrix(c(1,0.54,0.46,1), nrow=sum(fit$data$fleetTypes==0), ncol=8, byrow=FALSE)
# util2[,1:2]<-1
# util2[,4:8]=1
#catch fraction catch weight keeping total catch w
#There are ncol 4 fleets * 2 ways to define catches 1=fraction;  2=tons 
# only specify 3 because total  cv is given in "catchval" below
#************************ Here we iterate manually year by year with a new SSB each year *************************** 
FC[[length(FC)+1]] <- forecast2(fit, fscale=c(1,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA), 
                               catchval=c(NA,38354,23236,NA,NA,NA,NA,NA,NA,NA,NA),
                               fval=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
                               cf.cv.keep.cv=cf.cv.keep.cv, 
                               rec.years=2013:2017, 
                               label="MSE forecast Fscenario=1",
                               nosim=1000,estimate=median, deterministic=FALSE,
                               Fscenario=1,
                               MSYBtrig=c(NA, NA, NA, MSYBtrig, MSYBtrig, MSYBtrig, MSYBtrig, MSYBtrig, MSYBtrig, MSYBtrig, MSYBtrig), 
                               Blim=c(NA, NA, NA, Blim, Blim, Blim, Blim, Blim, Blim, Blim, Blim),
                               Fmsy=Fmsy,
                               Flow=Flow#, #for Fscenario= 3, 4 or 5
                               #util=util2 # for implementation error runs
                               #util_prop=c(1,0.54,0.46,1)
               )     


set.seed(12345) # same seed to repeat output
cf.cv.keep.cv<-matrix(NA,ncol=4*2,nrow=11)
cf.cv.keep.cv[2,] <- c(NA,NA,NA,NA,NA,18272,1215,17309)
cf.cv.keep.cv[3,] <- c(NA,NA,NA,NA,NA,11085,1225,9001)
#catch fraction catch weight keeping total catch w
#There are ncol 4 fleets * 2 ways to define catches 1=fraction;  2=tons 
# only specify 3 because total  cv is given in "catchval" below
#************************ Here we iterate manually year by year with a new SSB each year *************************** 
FC[[length(FC)+1]] <- forecast2(fit, fscale=c(1,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA), 
                 catchval=c(NA,38354,23236,NA,NA,NA,NA,NA,NA,NA,NA),
                 fval=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
                 cf.cv.keep.cv=cf.cv.keep.cv, 
                 rec.years=2013:2017, 
                 label="MSE forecast Fscenario=2",
                 nosim=1000,estimate=median, deterministic=FALSE,
                 Fscenario=2,
                 MSYBtrig=c(NA, NA, NA, MSYBtrig, MSYBtrig, MSYBtrig, MSYBtrig, MSYBtrig, MSYBtrig, MSYBtrig, MSYBtrig), 
                 Blim=c(NA, NA, NA, Blim, Blim, Blim, Blim, Blim, Blim, Blim, Blim),
                 Fmsy=Fmsy,
                 Flow=Flow#, #for Fscenario= 3, 4 or 5
                 #utilization=c(NA, NA, 1, 1, 1, 1), # or c(1,0.54,0.46,1) for implementation error runs
                 #util_prop=c(1,0.54,0.46,1)
)     


set.seed(12345) # same seed to repeat output
cf.cv.keep.cv<-matrix(NA,ncol=4*2,nrow=11)
cf.cv.keep.cv[2,] <- c(NA,NA,NA,NA,NA,18272,1215,17309)
cf.cv.keep.cv[3,] <- c(NA,NA,NA,NA,NA,11085,1225,9001)
#catch fraction catch weight keeping total catch w
#There are ncol 4 fleets * 2 ways to define catches 1=fraction;  2=tons 
# only specify 3 because total  cv is given in "catchval" below
#************************ Here we iterate manually year by year with a new SSB each year *************************** 
FC[[length(FC)+1]] <- forecast2(fit, fscale=c(1,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA), 
                 catchval=c(NA,38354,23236,NA,NA,NA,NA,NA,NA,NA,NA),
                 fval=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
                 cf.cv.keep.cv=cf.cv.keep.cv, 
                 rec.years=2013:2017, 
                 label="MSE forecast Fscenario=3",
                 nosim=1000,estimate=median, deterministic=FALSE,
                 Fscenario=3,
                 MSYBtrig=c(NA, NA, NA, MSYBtrig, MSYBtrig, MSYBtrig, MSYBtrig, MSYBtrig, MSYBtrig, MSYBtrig, MSYBtrig), 
                 Blim=c(NA, NA, NA, Blim, Blim, Blim, Blim, Blim, Blim, Blim, Blim),
                 Fmsy=Fmsy,
                 Flow=Flow#, #for Fscenario= 3, 4 or 5
                 #utilization=c(NA, NA, 1, 1, 1, 1), # or c(1,0.54,0.46,1) for implementation error runs
                 #util_prop=c(1,0.54,0.46,1)
)     



set.seed(12345) # same seed to repeat output
cf.cv.keep.cv<-matrix(NA,ncol=4*2,nrow=11)
cf.cv.keep.cv[2,] <- c(NA,NA,NA,NA,NA,18272,1215,17309)
cf.cv.keep.cv[3,] <- c(NA,NA,NA,NA,NA,11085,1225,9001)
#catch fraction catch weight keeping total catch w
#There are ncol 4 fleets * 2 ways to define catches 1=fraction;  2=tons 
# only specify 3 because total  cv is given in "catchval" below
#************************ Here we iterate manually year by year with a new SSB each year *************************** 
FC[[length(FC)+1]] <- forecast2(fit, fscale=c(1,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA), 
                 catchval=c(NA,38354,23236,NA,NA,NA,NA,NA,NA,NA,NA),
                 fval=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
                 cf.cv.keep.cv=cf.cv.keep.cv, 
                 rec.years=2013:2017, 
                 label="MSE forecast Fscenario=4",
                 nosim=1000,estimate=median, deterministic=FALSE,
                 Fscenario=4,
                 MSYBtrig=c(NA, NA, NA, MSYBtrig, MSYBtrig, MSYBtrig, MSYBtrig, MSYBtrig, MSYBtrig, MSYBtrig, MSYBtrig), 
                 Blim=c(NA, NA, NA, Blim, Blim, Blim, Blim, Blim, Blim, Blim, Blim),
                 Fmsy=Fmsy,
                 Flow=Flow#, #for Fscenario= 3, 4 or 5
                 #utilization=c(NA, NA, 1, 1, 1, 1), # or c(1,0.54,0.46,1) for implementation error runs
                 #util_prop=c(1,0.54,0.46,1)
)     

set.seed(12345) # same seed to repeat output
cf.cv.keep.cv<-matrix(NA,ncol=4*2,nrow=11)
cf.cv.keep.cv[2,] <- c(NA,NA,NA,NA,NA,18272,1215,17309)
cf.cv.keep.cv[3,] <- c(NA,NA,NA,NA,NA,11085,1225,9001)
#catch fraction catch weight keeping total catch w
#There are ncol 4 fleets * 2 ways to define catches 1=fraction;  2=tons 
# only specify 3 because total  cv is given in "catchval" below
#************************ Here we iterate manually year by year with a new SSB each year *************************** 
FC[[length(FC)+1]] <- forecast2(fit, fscale=c(1,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA), 
                                catchval=c(NA,38354,23236,NA,NA,NA,NA,NA,NA,NA,NA),
                                fval=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
                                cf.cv.keep.cv=cf.cv.keep.cv, 
                                rec.years=2013:2017, 
                                label="MSE forecast Fscenario=5",
                                nosim=1000,estimate=median, deterministic=FALSE,
                                Fscenario=5,
                                MSYBtrig=c(NA, NA, NA, MSYBtrig, MSYBtrig, MSYBtrig, MSYBtrig, MSYBtrig, MSYBtrig, MSYBtrig, MSYBtrig), 
                                Blim=c(NA, NA, NA, Blim, Blim, Blim, Blim, Blim, Blim, Blim, Blim),
                                Fmsy=Fmsy,
                                Flow=Flow#, #for Fscenario= 3, 4 or 5
                                #utilization=c(NA, NA, 1, 1, 1, 1), # or c(1,0.54,0.46,1) for implementation error runs
                                #util_prop=c(1,0.54,0.46,1)
)     




# FC[[Fscenario]][Y][[1]]$..
SSB<-array(dim=c(1000,length(FC[[1]]),length(FC)))
Catch<-array(dim=c(1000,length(FC[[1]]),length(FC)))
Rec<-array(dim=c(1000,length(FC[[1]]),length(FC)))
Fbar<-array(dim=c(1000,length(FC[[1]]),length(FC)))
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

years=2017:(2017+length(FC[[1]])-1)
t1=4 # year number where MSE starts for probability SSB<Blim plot


#### Plot results in pdf ####

setwd("~/DTU/HAWG/Forecast/MSE")

pdf(file = paste0("MSE_forecast_Fmsy=",Fmsy,".pdf"), width = 12, height = 12 )

  plot(x=c(0,MSYBtrig,MSYBtrig*2), y=c(0,Fmsy,Fmsy), type="l", xlab="SSB", ylab="F", ylim=c(0,Fmsy*1.1), main="F scenarios",lwd=2)
  segments(x0=MSYBtrig, y0 = 0, x1=MSYBtrig, y1=Fmsy, lty=2, col="grey")
  segments(x0=Blim, y0 = 0, x1=Blim, y1=(Fmsy*Blim/MSYBtrig), lty=2, col="grey")
  
  segments(x0=0, y0 = Flow, x1=Blim, y1=Flow, lty=1, col="blue",lwd=2)
  segments(x0=0, y0 = 0, x1=Blim, y1=0, lty=1, col="red",lwd=2)
  segments(x0=(Flow*MSYBtrig/Fmsy), y0 = Flow, x1=Blim, y1=(Fmsy*Blim/MSYBtrig), lty=2, col="green",lwd=2)
  segments(x0=0, y0 = Flow, x1=(Flow*MSYBtrig/Fmsy), y1=Flow, lty=2, col="green",lwd=2)
  segments(x0=Blim, y0 = Flow, x1=MSYBtrig, y1=Fmsy, lty=4, col="orange",lwd=2)
  segments(x0=0, y0 = Flow, x1=Blim, y1=Flow, lty=4, col="orange",lwd=2)
  text(x=Blim*2/3, y=Flow-0.01, labels = "3", col="blue")
  text(x=Blim/2, y=0.02, labels = "1", col="red")
  text(y=Flow/2, x=Blim*0.1, labels = "2")
  text(x=(Flow*MSYBtrig/Fmsy), y=Flow+0.03, labels = "4", col="green")
  text(x=(MSYBtrig+Blim)/2, y=Flow+0.05, labels = "5", col="orange")
  #points(x=Blim, y=(Fmsy*Blim/MSYBtrig), pch=16, col="grey")
  text(x=Blim*0.9, y=Fmsy*0.8, labels="Blim", col="grey")
  text(x=MSYBtrig, y=Fmsy*1.05, labels="MSYBtrigger", col="grey")
  

  par(mfrow=c(ceiling(length(FC)/2),2))
  for (k in 1:length(FC)){
    fbarplot(FC[[k]], main=paste0("Fscenario=",k))
    abline(h=Fmsy,col="red",lty=2)
    abline(h=Flow,col="blue",lty=2)
  }
  plot.new()
  for (k in 1:length(FC)){
    ssbplot(FC[[k]], main=paste0("Fscenario=",k))
    abline(h=MSYBtrig,col="red",lty=2)
    abline(h=Blim,col="blue",lty=2)
    
  }
  plot.new()
  for (k in 1:length(FC)){
    catchplot(FC[[k]], main=paste0("Fscenario=",k))
  }
  plot.new()
  for (k in 1:length(FC)){
    recplot(FC[[k]], main=paste0("Fscenario=",k))
  }
  
  plot.new()
  par(mfrow=c(2,ceiling((length(FC[[1]])-(t1-1))/2)),oma=c(4,4,2,1),mar=c(0,0,0,0))
  for (t in t1:length(FC[[1]])){
    plot(x=risk.val[1,,t],y=risk.val[2,,t],pch=1:5,xlim=c(0,1),ylim=c(0,max(risk.val[2,,])*1.1),xaxt="n",yaxt="n")
    text(x=0.5,y=max(risk.val[2,,]),labels = years[t],cex=2)
    if(t==t1) legend("topleft",legend=1:5,pch=1:5,bty="n")
    if(t==t1) axis(2)
    if(t==t1+ceiling((length(FC[[1]])-(t1-1))/2)) axis(2)
    if(t>=t1+ceiling((length(FC[[1]])-(t1-1))/2)) axis(1)
  }
  mtext("Probability SSB<Blim",side=1,line=3,outer=TRUE)
  mtext("Median catch",side=2,line=3,outer=TRUE)

  par(mfrow=c(1,1))
  for (k in 1:length(FC)){
    plot.new()
    text(x=0.5,y=1,labels=paste0("Catch by fleet Fscenario=",k), cex=2)
    # FC.table<-array(dim=c(length(FC[[1]]),12))
    # FC.table[]<-FC[[k]]
    # grid.table(FC.table)
    grid.table(attributes(FC[[k]])$catchby)
  }
  
  # col.scale <- c("#b2182b","#ef8a62","#fddbc7","#d1e5f0","#67a9cf","#2166ac")
  # par(mfrow=c(ceiling(length(FC)/2),2))
  # for (k in 1:length(FC)){
  #   plot(Fbar[,3,k]~SSB[,2,k], main=paste0("Fscenario=",k), ylim=c(0,max(Fbar[,3:(ncol(SSB)),k])), xlim=c(0,max(SSB[,2:(ncol(SSB)-1),k])),col=col.scale[1])
  #   for (t in 3:ncol(SSB)-1){
  #     points(Fbar[,t+1,k]~SSB[,t,k], main=paste0("Fscenario=",k),col=col.scale[t-1])
  #   }
  # }
  
  
dev.off()



# xtab<-function(x,caption='Table X.', file=stdout(), width='"100%"', cornername='', dec=rep(1,ncol(x))){
#   nc<-ncol(x)
#   lin<-paste('<table width=',width,'>', sep='')
#   lin<-c(lin,sub('$','</td></tr>',sub('\\. |\\.$','.</b> ',
#                                       sub('^', paste('<tr><td colspan=',nc+1,'><b>',sep=''), caption))))
#   hr<-paste('<tr><td colspan=',nc+1,'><hr noshade></td></tr>', sep='')
#   lin<-c(lin,hr)
#   cnames<-colnames(x)
#   cnames<-paste(sub('$','</b></td>',sub('^','<td align=right><b>',cnames)), collapse='\t')
#   lin<-c(lin,paste('<tr>',paste('<td align=left><b>',cornername,'</b></td>',sep=''),cnames,'</tr>'))
#   lin<-c(lin,hr)
#   rnames<-sub('$','</b></td>',sub('^','<tr> <td align=left><b>',rownames(x)))
#   xx<-sapply(1:ncol(x),function(i)sub('NA','  ',formatC(round(x[,i,drop=FALSE],dec[i]),digits=dec[i], format='f')))
#   x<-relist(xx, skeleton=x)
#   for(i in 1:nrow(x)){
#     thisline<-paste(rnames[i],paste(sub('$','</td>',sub('^','<td align=right>',x[i,])), collapse='\t'),'</tr>', sep='')
#     lin<-c(lin,thisline)
#   }
#   lin<-c(lin,hr)
#   lin<-c(lin,'</table><br>\n')
#   writeLines(lin,con=file)
# }
# stamp<-gsub('-[[:digit:]]{4}$','',gsub(':','.',gsub(' ','-',gsub('^[[:alpha:]]{3} ','',date()))))
# if(exists("FC")){
#   ii<-0
#   lapply(FC, function(f){
#     ii<<-ii+1;
#     tf<-attr(f,"tab");
#     dec<-c(3,3,3,rep(0,ncol(tf)-3));
#     xtab(tf, caption=paste0('Forecast table ',ii,'a. ', attr(f,"label"),'.'),
#          cornername='Year', file=paste(stamp,'_tabX',ii,'a.html',sep=''), dec=dec);
#     tf<-attr(f,"catchby");
#     tf<-tf[,seq(1, ncol(tf), by=3)]
#     dec<-rep(0,ncol(tf));
#     xtab(tf, caption=paste0('Forecast table of catch by fleet ',ii,'b. ', attr(f,"label"),'.'),
#          cornername='Year', file=paste(stamp,'_tabX',ii,'b.html',sep=''), dec=dec);
#   })
# }
# 
# 
