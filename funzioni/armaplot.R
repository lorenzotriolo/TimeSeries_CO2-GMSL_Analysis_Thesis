armaplot <- function(n.obs=500, phi1=0, phi2=0, theta1=0, theta2=0, lm=36, seme=3)
{
  library(tseries)
  set.seed(seme)
  ts.sim<-arima.sim(n=n.obs, model = list(ar=c(phi1,phi2), ma=c(theta1, theta2)))
  par(mfrow=c(3,2))
  ts.plot(ts.sim, ylab="", main="Grafico della serie");abline(h=mean(ts.sim))
  wn <- ts(rnorm(n.obs))
  ts.plot(wn,ylab="", main="Wn"); abline(h=mean(wn))
  
  acft <- ARMAacf(ar=c(phi1,phi2), ma=c(theta1,theta2), lag.max = lm)
  pacft <- ARMAacf(ar=c(phi1,phi2), ma=c(theta1,theta2), lag.max = lm, pacf = TRUE)
  
  barplot(acft[-1], col="red", ylim=c(-1,1), main="ACF teorica", names.arg = seq(1:lm))
  barplot(pacft, col="red", ylim=c(-1,1), main="PACF teorica", names.arg = seq(1:lm))
  
  acfs<-acf(ts.sim, lag.max = lm, plot = F)
  pacfs<-pacf(ts.sim, lag.max = lm, plot = F)
  
  barplot(as.matrix(acfs$acf[-1]), beside = T, col="yellow", ylim=c(-1,1), 
          main="ACF campionaria", names.arg = seq(1:lm))
  abline(h=c(-1.96/n.obs^(1/2),1.96/n.obs^(1/2)),lty=2)
  
  barplot(as.matrix(pacfs$acf), beside = T, col="yellow", ylim=c(-1,1), 
          main="PACF campionaria", names.arg = seq(1:lm))
  abline(h=c(-1.96/n.obs^(1/2),1.96/n.obs^(1/2)),lty=2)
}
