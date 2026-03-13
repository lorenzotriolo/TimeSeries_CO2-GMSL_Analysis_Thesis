correlogrammi<-function(dati, lm=36)
{
  n.obs <- length(dati)
  par(mfrow=c(1,2))
  acfs<-acf(dati,lag.max = lm, plot=F)
  m.acfs<-max(abs(acfs$acf[-1])) + 0.1
  pacfs<-pacf(dati,lag.max = lm, plot=F)
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
  
  par(mfrow=c(1,1))
}