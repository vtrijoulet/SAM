
HS_fit <- function(name, pair.years=NULL){

  fit=fitfromweb(name,character.only=TRUE)
  if (missing(pair.years)) pair.years=1:fit$data$noYears else pair.years=which(fit$data$years%in%pair.years) # choose SR pairs
  lag <- fit$conf$minAge

  if (lag>0) {
    y=rectable(fit)[pair.years[-(1:lag)],1]
    x=ssbtable(fit)[pair.years[-((length(pair.years)-(lag-1)):length(pair.years))],1]
  } else {
    y=rectable(fit)[pair.years,1]
    x=ssbtable(fit)[pair.years,1]
  }

  fn <- function(par){
    i <- (x < exp(par[1]))
    slope <- exp(par[2]) / exp(par[1])
    sum( (log(y[i]) - log(slope * x[i]))^ 2 ) + sum( ( log(y[!i]) - par[2] )^ 2  )
  }
  n = 100
  gr <- expand.grid(x=seq(log(min(x)),log(max(x)),length=n), y=seq(log(min(y)),log(max(y)),length=n))
  m <- apply(gr,1,fn)
  # Get close to global optimum
  par = unlist(gr[which.min(m),]) # par are x,y coordinates for inflexion point
  # Find the exact global optimum
  opt <- optim(par, fn, hessian=TRUE)
  
  # inverse_hessian<-solve(opt$hessian)
  # sigma<-sqrt(diag(inverse_hessian))
  # natural_sigma <- exp(opt$par)*sigma
  # natural_cv <- round(natural_sigma/exp(opt$par),3)
  #upper<-opt$par+1.96*prop_sigma
  #lower<-opt$par-1.96*prop_sigma


  log.pred.y <- y
  i <- (x < exp(opt$par[1]))
  slope <- exp(opt$par[2]) / exp(opt$par[1])
  log.pred.y[i] <- log(slope * x[i])
  log.pred.y[!i] <-(opt$par[2])

  sd.log.y <- sd(log(y)-log.pred.y)
  # CI.low <- log.pred.y-1.96*sd.log.y
  # CI.up <- log.pred.y+1.96*sd.log.y
  # 
  # plot(y=log(y), x=log(x), pch=16, type="p")
  # lines(log.pred.y~log(x))
  # polygon(x=c(sort(log(x)),rev(sort(log(x)))), y=c(sort(CI.low),rev(sort(CI.up))), col=rgb(0,0,0,alpha=0.2), border = NA)
  # 
  # plot(y=y, x=x, pch=16, type="o", col="red")
  # lines(exp(log.pred.y)~x)
  # polygon(x=c(sort(x),rev(sort(x))), y=c(sort(exp(CI.low)),rev(sort(exp(CI.up)))), col=rgb(0,0,0,alpha=0.2), border = NA)

  return(c(as.vector(opt$par), sd.log.y))
  #save.image(file=paste0("~/DTU/Manuscripts/MSE_forecast_paper/results/HS_param_", name, ".Rdata"))
  
}



# fn1 <- function(par, ssb,rec){
#   log_pred_rec <- vector(length=length(ssb))
#   for (t in 1:length(ssb)){
#       if (ssb[t]>=(exp(par[1])/exp(par[2]))){
#         log_pred_rec[t] <- log(exp(par[1]))
#       } else {
#         log_pred_rec[t] <- log(exp(par[2])*ssb[t])
#       }
#   }
#   nll=sum((log(rec)-log_pred_rec)^2)
#   #nll=nll+plogis(log(exp(par[1]))-log(max(rec)))*1e10*(log(exp(par[1]))-log(max(rec)))^2
#   #if (log(exp(par[1]))>log(max(rec))) nll=nll+1e10*(log(exp(par[1]))-log(max(rec)))^2
#   #nll=nll+plogis(min(ssb)-(exp(par[1])/exp(par[2])))*1e10*(min(ssb)-(exp(par[1])/exp(par[2])))^2
#   #if ((exp(par[1])/exp(par[2]))<=min(ssb)) nll=nll+1e10*(min(ssb)-(exp(par[1])/exp(par[2])))^2
#   return(nll)
# }
# opt <- optim(par = c(log(max(rectable(fit)[pair.years[-1],1])), log(10))*1, ssb=ssbtable(fit)[pair.years[-length(pair.years)],1], rec=rectable(fit)[pair.years[-1],1], fn=fn1, control =list(reltol=1e-40, maxit=1000) )
# exp(opt$par)
# plot(y=rec, x=ssb, pch=16, type="o", xlim=c(0,max(ssbtable(fit)[-fit$data$noYears,1])))
# pred.rec=exp(opt$par[2])*seq(0,300000,100)
# lines(pred.rec~seq(0,300000,100))
# abline(h=exp(opt$par[1]))

