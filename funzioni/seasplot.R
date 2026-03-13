seasplot<-function(dati, periodo)
{
  #dati: dati da plottare
  #periodo: 12 se serie mensile, 4 se serie trimestrale
  
  nomi<-if (periodo==12) list ("G", "F", "M", "A", "M", "G", "L", "A", "S", "O", "N", "D")
  else list ("I", "II", "III", "IV")
  
  n<-length(dati)
  li<-rep(0, periodo)
  mi<-rep(0, periodo)
  
  li[1]<-length(dati[seq(1, length(dati),by=periodo)]) #serie gennaio
  labx<-if (periodo==12) "month" else "quarter" #asse delle ascisse
  
  par(mfrow=c(1,1), cex=0.8)
  plot(seq(1:li[1]),dati[seq(1, length(dati), by=periodo)], ylab="", ylim = c(min(dati),max(dati)), xlim=c(1,n),
       xlab=labx, main="Serie stagionali",type = "l",col="blue", lwd=2, axes=F) #grafico spezzata gennaio
  mi[1]<-mean(dati[seq(1, length(dati),by=periodo)]) #media gennaio
  segments(1, mi[1], li[1], mi[1], col="red") #aggiungo media gennaio sul grafico
  
  for (i in 2:periodo){
    li[i]<-length(dati[seq(i, length(dati),by=periodo)]) #
    lines(cumsum(li)[i-1] + seq(1:li[i]), dati[seq(i, length(dati),by=periodo)], col="blue", lwd=2) #altri grafici
    mi[i]<-mean(dati[seq(i, length(dati),by=periodo)]) #medie altri mesi
    segments(1+cumsum(li)[i-1], mi[i],li[i] + cumsum(li)[i-1], mi[i], col="red") #aggiungo medie successive grafico
  }
  axis(1, at = (li[i])*seq(1:periodo) - li[1]/2, labels= nomi)
  axis(2)
}