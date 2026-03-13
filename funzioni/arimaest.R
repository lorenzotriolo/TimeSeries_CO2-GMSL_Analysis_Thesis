arimaest<-function(dati, nsorder= c(0,0,0), sorder= c(0,0,0), periodo=NA, vf=F, res=F)
{
  ris<-arima(dati, order=nsorder, seasonal= list(order=sorder, period=periodo), include.mean = vf)
  coef.t<-ris$coef/diag(ris$var.coef)^(1/2)
  out<- if (res==F) list (ris, coef.t) else ris$residuals
  names(out)<-list("stime", "tstatistics")
  out
}
