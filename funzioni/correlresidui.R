correlresidui<-function(residui,lm=36, npar=0)
{
  par(mfrow=c(1,3))
  n.obs<-length(residui)
  acfs<-acf(residui, lag.max = lm, plot = F)
  m.acfs <- max(abs(acfs$acf[-1])) + 0.1
  pacfs<-pacf(residui, lag.max = lm, plot = F)
  m.pacfs<-max(abs(pacfs$acf)) + 0.1
  
  #plot acf
  barplot(rev(as.matrix(acfs$acf[-1])), beside = T, col= "yellow", xlim = c(-m.acfs, m.acfs), horiz = T, 
          main = "Acf globale", ylab = "", cex.names = 0.9, names.arg = rev(seq(1:lm)))
  
  abline(v=0)
  abline(v=c(-1.96/n.obs^(1/2),1.96/n.obs^(1/2)), lty = 2)
  
  #plot pacf
  barplot(rev(as.matrix(pacfs$acf)), beside = T, col="yellow", xlim=c(-m.pacfs,m.pacfs), horiz = T,
          main= "Acf parziale", ylab = "", cex.names = 0.9, names.arg = rev(seq(1:lm)))
  
  abline(v=0)
  abline(v=c(-1.96/n.obs^(1/2),1.96/n.obs^(1/2)), lty = 2)
  
  #grafico statistica Ljung-Box
  LBv<-rep(0,lm)
  for (i in ((npar+1):lm)){
    LBv[i]<-Box.test(residui, lag=i, type = "Ljung-Box",fitdf = npar)$p.value
  }
  plot(LBv, rev(seq(1:lm)),main = "Ljung-Box stat", axes = F, ylab = "lag", xlab="p.value",
       xlim = c(0,1), type = "n")
  points(LBv[(npar+1):lm], rev(seq(1:(lm-npar))))
  abline(v=0.05, col="red")
  axis(1,seq(0,1,0.1))
  axis(2,at=seq(1:lm),labels = rev(seq(1:lm)))
  
  par(mfrow=c(1,1))
  
}