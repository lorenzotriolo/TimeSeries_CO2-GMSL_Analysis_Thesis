forecastplott <- function(dati, stime, passi) {
  # dati = serie originale (oggetto ts con start e frequency)
  # stime = modello ARIMA stimato (da arimaest)
  # passi = numero di previsioni future
  
  forecast_vals <- predict(stime, passi)$pred
  forecast_se   <- predict(stime, passi)$se
  
  # Ottieni info temporali della serie originale
  start_pred <- time(dati)[length(dati)] + 1/frequency(dati)
  forecast_ts <- ts(forecast_vals, start = start_pred, frequency = frequency(dati))
  upper_ts <- forecast_ts + 1.96 * forecast_se
  lower_ts <- forecast_ts - 1.96 * forecast_se
  
  # Plot correttamente con tempo sull'asse X
  ts.plot(dati, forecast_ts, upper_ts, lower_ts,
          col = c("black", "blue", "blue", "blue"),
          lty = c(1,1,2,2),
          ylab = "Valori previsti",
          xlab = "Tempo")
  
  # Restituisce valori utili
  list(forecast = forecast_ts, se = forecast_se)
}
