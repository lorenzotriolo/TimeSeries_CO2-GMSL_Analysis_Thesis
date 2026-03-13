forecastplot <- function(dati,stime,passi){
  #dati = dati originali
  #stime = stime con arimaest a partire da serie originale
  #passi = numero di passi da prevedere in avanti
  forecast<-predict(stime, passi)$pred
  forecast.se <- predict(stime, passi)$se
  plot(c(dati, forecast), type='l')
  lines(forecast, col='blue')
  lines(forecast + 1.96 * forecast.se, col='blue', lty=2)
  lines(forecast - 1.96 * forecast.se, col='blue', lty=2)
  list(forecast, forecast.se)
}
