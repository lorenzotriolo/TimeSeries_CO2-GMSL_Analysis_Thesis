testnorm<-function(residui, nclassi){
  res.stand<- (residui - mean(residui))/sd(residui) #residui standardizzati
  hist(res.stand, freq = F, main="", col="lightblue", ylim = c(0,0.6), nclass=nclassi) #istogramma
  curve(dnorm, add=T, col="red", lwd=2) # adattiamo curva normale per confronto
  library(tseries)
  jb<-jarque.bera.test(res.stand) #test di Jarque-Bera
  legend("topright", legend = paste("JB test p.value = ", round(jb$p.value,3)), bty = "n", cex=1.2)
  jb
}