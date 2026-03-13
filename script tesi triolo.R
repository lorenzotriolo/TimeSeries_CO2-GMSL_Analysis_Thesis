### LIBRERIE & FUNZIONI ###
library(readr)
library(tseries)
library(dplyr)
library(lubridate)
library(vars)
library(ggplot2)
library(lmtest)

setwd('C:\\Users\\HP\\Desktop\\terzo anno sse\\serie storiche ed economiche\\lab\\funzioni')
source('correlogrammi.R')
source('seasplot.R')
source('arimaest.R')
source('correlresidui.R')
source('forecastplott.R')
source('testnorm.R')


#################################
#### ANALISI SERIE CO2 ##########
#################################

file_path_co2 <- "C:\\Users\\HP\\Desktop\\tesi\\dataset + script\\co2.txt"
co2_data <- read.table(file_path_co2, header = FALSE, skip = 1, sep = "", stringsAsFactors = FALSE)

# Assegno i nomi corretti alle colonne (come da documentazione)
names(co2_data) <- c("year", "month", "decimal_date", "CO2_monthly_avg",
                     "CO2_deseasonalized", "num_days", "st_dev", "unc_of")

# Verifica presenza di NA
cat("Ci sono NA nella colonna 'CO2_monthly_avg'?", any(is.na(co2_data$CO2_monthly_avg)), "\n")

# Serie mensile
data <- ts(co2_data$CO2_monthly_avg, start = c(co2_data$year[1], co2_data$month[1]), frequency = 12)
ts.plot(data)
correlogrammi(data, 80); seasplot(data,12)

# Differenze per stazionarietà
ddata <- diff(data); ts.plot(ddata); abline(h=mean(ddata))
d12 <- diff(ddata,12); ts.plot(d12); abline(h=mean(d12))
seasplot(ddata,12)
correlogrammi(ddata, 80)
seasplot(d12,12)
correlogrammi(d12,80)

# Test di Dickey-Fuller aumentato
adf.test(d12)

### Stima modelli ARIMA ###
arimaest(d12, nsorder = c(1,0,0), sorder=c(0,0,1), periodo =12)
arimaest(d12, nsorder = c(0,0,1), sorder=c(0,0,1), periodo =12)
arimaest(d12, nsorder = c(0,0,2), sorder=c(0,0,1), periodo =12) #AIC = 440.61
arimaest(d12, nsorder = c(0,0,1), sorder=c(1,0,1), periodo =12)
arimaest(d12, nsorder = c(1,0,1), sorder=c(0,0,1), periodo =12) #AIC = 439.45
arimaest(d12, nsorder = c(1,0,1), sorder=c(0,0,1), periodo =12, vf=T) #AIC = 432.27
arimaest(d12, nsorder = c(0,0,2), sorder=c(0,0,2), periodo =12)
arimaest(d12, nsorder = c(0,0,2), sorder=c(0,0,1), periodo =12, vf=T)  # AIC = 434.45

# Verifica del modello scelto: (1,1,1)  x (0,1,1)
res1 <- arimaest(d12, nsorder = c(1,0,1), sorder=c(0,0,1), periodo =12, vf=T, res=T)
testnorm(res1,30)
correlresidui(res1, 80, 3)
#Verifica del modello (0,1,2) x (0,1,1)
res2 <- arimaest(d12, nsorder = c(0,0,2), sorder=c(0,0,1), periodo =12, vf=T, res=T)
testnorm(res2,30)
correlresidui(res2, 80, 3)

### Previsioni ###
# forecastplott: versione modificata per includere date
co2_pred <- arimaest(data, nsorder = c(1,1,1), sorder=c(0,1,1), periodo =12, vf=T)$stime
forecastplott(data, co2_pred, 60)
forecastplott(data, co2_pred, 312)
forecastplott(data, co2_pred, 600)
pred1 <- arimaest(data, nsorder = c(0,1,2), sorder=c(0,1,1), periodo =12, vf=T)$stime
forecastplott(data,pred1, 60)

par(mfrow=c(2,1))
forecastplott(data, pred, 60)
forecastplott(data,pred1, 60)
par(mfrow=c(1,1))

### Decomposizione ###
decomp_co2 <- decompose(data, type = "additive")  # Additiva: ampiezza costante
plot(decomp_co2)


#################################
#### ANALISI SERIE GMSL #########
#################################

file_path_gmsl <- "C:\\Users\\HP\\Desktop\\tesi\\dataset + script\\gmsl.txt"
gmsl <- read.table(file_path_gmsl, skip = 23, header = FALSE)

colnames(gmsl) <- c(
  "altimeter_type", "cycle", "year_frac", "n_obs", "n_obs_weighted",
  "gmsl_no_gia", "std_no_gia", "gmsl_no_gia_smoothed",
  "gmsl_gia", "std_gia", "gmsl_gia_smoothed",
  "gmsl_gia_smoothed_deseasoned", "gmsl_no_gia_smoothed_deseasoned"
)

# Aggiungo variabile data e aggrego per mese
gmsl <- gmsl %>%
  mutate(
    date = as.Date((year_frac - 1970) * 365.25, origin = "1970-01-01"),
    year_month = format(date, "%Y-%m")
  ) %>%
  group_by(year_month) %>%
  summarise(gmsl_gia = mean(gmsl_gia, na.rm = TRUE)) %>%
  arrange(year_month)

# Creo la serie temporale
gmsl_ts <- ts(gmsl$gmsl_gia,
              start = c(as.numeric(substr(gmsl$year_month[1], 1, 4)),
                        as.numeric(substr(gmsl$year_month[1], 6, 7))),
              frequency = 12)
ts.plot(gmsl_ts)

# Differenze per stazionarietà
dsl <- diff(gmsl_ts); ts.plot(dsl); abline(h=mean(dsl))
seasplot(gmsl_ts,12)
seasplot(dsl, 12)
correlogrammi(dsl,80)

d12dsl <- diff(dsl,12)
ts.plot(d12dsl); abline(h=mean(d12dsl))
seasplot(d12dsl,12)
correlogrammi(d12dsl, 80)

adf.test(d12dsl)

### Modelli ARIMA ###
arimaest(d12dsl, nsorder =c(1,0,1), sorder = c(1,0,1), periodo =12)
arimaest(d12dsl, nsorder = c(1,0,0), sorder=c(0,0,1), periodo =12) #aic =1606
arimaest(d12dsl, nsorder = c(1,0,0), sorder=c(1,0,0), periodo =12) #aic =1745
arimaest(d12dsl, nsorder = c(0,0,1), sorder=c(0,0,1), periodo =12) #aic =1603
arimaest(d12dsl, nsorder = c(0,0,1), sorder=c(0,0,2), periodo =12)
arimaest(d12dsl, nsorder = c(2,0,0), sorder=c(0,0,1), periodo =12)
arimaest(d12dsl, nsorder = c(1,0,2), sorder=c(2,0,0), periodo =12) #1685
arimaest(d12dsl, nsorder = c(1,0,0), sorder=c(2,0,0), periodo =12) 
arimaest(d12dsl, nsorder = c(1,0,0), sorder=c(0,0,1), periodo =12, vf=T)  # Intercetta non significativa

# Diagnostica modello
res_sl <- arimaest(d12dsl, nsorder = c(1,0,0), sorder = c(0,0,1), periodo =12, res=T)
testnorm(res_sl, 30)
correlresidui(res_sl, lm= 80, npar=2)

# Altro tentativo
res_sl2 <- arimaest(d12dsl, nsorder = c(0,0,1), sorder = c(0,0,1), periodo =12, res=T)
testnorm(res_sl2, 30)
correlresidui(res_sl2, lm= 80, npar=2)

### Previsioni ###
pred <- arimaest(gmsl_ts, nsorder = c(1,1,0), sorder = c(0,1,1), periodo =12)$stime
forecastplott(gmsl_ts, pred, 60)

#################################
#### GESTIONE OUTLIER GMSL #########
#################################
# Seleziona solo le osservazioni dal 1997 in poi
gmsl_sub <- subset(gmsl, as.numeric(substr(year_month, 1, 4)) >= 1997)

# Crea la serie temporale a partire da 1997
ts_fix <- ts(gmsl_sub$gmsl_gia,
             start = c(as.numeric(substr(gmsl_sub$year_month[1], 1, 4)),
                       as.numeric(substr(gmsl_sub$year_month[1], 6, 7))),
             frequency = 12)
ts.plot(ts_fix)
gmsl_fix_seas <- diff(diff(ts_fix),12); seasplot(gmsl_fix_seas,12); correlogrammi(gmsl_fix_seas, 80)
### Modelli ARIMA ###
arimaest(gmsl_fix_seas, nsorder =c(1,0,1), sorder = c(1,0,1), periodo =12)
arimaest(gmsl_fix_seas, nsorder = c(1,0,0), sorder=c(0,0,1), periodo =12) #aic =1372
arimaest(gmsl_fix_seas, nsorder = c(1,0,0), sorder=c(1,0,0), periodo =12) #aic =1482
arimaest(gmsl_fix_seas, nsorder = c(0,0,1), sorder=c(0,0,1), periodo =12) #aic =1376
arimaest(gmsl_fix_seas, nsorder = c(0,0,1), sorder=c(0,0,2), periodo =12)
arimaest(gmsl_fix_seas, nsorder = c(2,0,0), sorder=c(0,0,1), periodo =12)
arimaest(gmsl_fix_seas, nsorder = c(1,0,2), sorder=c(2,0,0), periodo =12) #1432
arimaest(gmsl_fix_seas, nsorder = c(1,0,0), sorder=c(2,0,0), periodo =12) 
arimaest(gmsl_fix_seas, nsorder = c(1,0,0), sorder=c(0,0,1), periodo =12, vf=T) #intercetta

res_sl2 <- arimaest(gmsl_fix_seas, nsorder = c(1,0,0), sorder = c(0,0,1), periodo =12, res=T)
testnorm(res_sl2, 30)
correlresidui(res_sl2, lm= 80, npar=2)
pred <- arimaest(ts_fix, nsorder = c(1,1,0), sorder = c(0,1,1), periodo =12)$stime
forecastplott(ts_fix, pred, 60)
forecastplott(ts_fix, pred, 312)

### Decomposizione ###
decomp_gmsl <- decompose(gmsl_ts, type = "additive")
plot(decomp_gmsl)

#################################
#### ANALISI SERIE CONGIUNTE #########
#################################

# GMSL mensile
# GMSL mensile: solo la variabile gmsl_gia
gmsl_mensile <- gmsl %>%
  dplyr::select(year_month, gmsl_gia) %>%
  group_by(year_month) %>%
  summarise(gmsl_gia = mean(gmsl_gia, na.rm = TRUE)) %>%
  arrange(year_month)

# CO2 mensile
co2_data <- co2_data %>%
  mutate(
    year_month = sprintf("%04d-%02d", year, month)
  )
co2_mensile <- co2_data %>%
  dplyr::select(year_month, CO2_monthly_avg)

##### Preparazione dati congiunti #####

dati_comuni <- inner_join(co2_mensile, gmsl_mensile, by = "year_month") %>%
  dplyr::select(year_month, CO2_monthly_avg, gmsl_gia) %>%
  arrange(year_month)

start_year <- as.numeric(substr(dati_comuni$year_month[1], 1, 4))
start_month <- as.numeric(substr(dati_comuni$year_month[1], 6, 7))

co2_ts <- ts(dati_comuni$CO2_monthly_avg, start = c(start_year, start_month), frequency = 12)
gmsl_ts <- ts(dati_comuni$gmsl_gia, start = c(start_year, start_month), frequency = 12)

d_co2_ts <- diff(co2_ts); d12_co2_ts <- diff(d_co2_ts, 12)
d_gmsl_ts <- diff(gmsl_ts); d12_gmsl_ts <- diff(d_gmsl_ts, 12)

##### Modello VAR #####

# Scatter plot CO2 vs GMSL
library(ggplot2)
ggplot(dati_comuni, aes(x = CO2_monthly_avg, y = gmsl_gia)) +
  geom_point(color = "steelblue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "darkred", se = FALSE) +
  labs(
    title = "Relazione tra CO2 e GMSL (con GIA)",
    x = "CO2 (ppm)",
    y = "GMSL (mm, con GIA)"
  ) +
  theme_minimal()

# Selezione lag ottimale
VARselect(dati_comuni[, -1], lag.max = 6, type = "const")

# Stima del VAR con p =  lag migliore scelto sopra, scelgo di usare SC (schwarz, BIC)
m1 <- VAR(dati_comuni[, -1], p = 3, type = "const")
summary(m1)
m2 <- VAR(dati_comuni[, -1], p = 2, type = "const")
summary(m2)

predictions <- predict(m2, n.ahead = 30, ci = 0.95)
# Plot previsioni
plot(predictions)

# Costruzione dataframe con le serie destagionalizzate
time_diff <- time(d12_co2_ts)
dati_diff <- data.frame(
  time = as.yearmon(time_diff),
  d12_CO2 = as.numeric(d12_co2_ts),
  d12_GMSL = as.numeric(d12_gmsl_ts)
)

VARselect(dati_diff[, c("d12_CO2", "d12_GMSL")], lag.max = 6, type = "none")
m_var_destag <- VAR(dati_diff[, c("d12_CO2", "d12_GMSL")], p =3, type= 'none')
summary(m_var_destag)
predictions_staz <- predict(m_var_destag, n.ahead = 12, ci = 0.95)
plot(predictions_staz)

# Funzione per invertire la trasformazione d12 (differenziazione stagionale)
invert_seasonal_diff <- function(diff_values, original_series, n_ahead = 12, seasonal_period = 12) {
  # Prendi gli ultimi seasonal_period valori della serie originale
  last_seasonal_values <- tail(as.numeric(original_series), seasonal_period)
  
  # Inizializza il vettore delle previsioni in livelli
  pred_levels <- numeric(n_ahead)
  
  # Per ogni previsione, aggiungi la differenza al valore corrispondente di un anno fa
  for (i in 1:n_ahead) {
    if (i <= seasonal_period) {
      # Per i primi 12 valori, usa i valori storici dell'anno precedente
      pred_levels[i] <- last_seasonal_values[i] + diff_values[i]
    } else {
      # Per i valori successivi, usa i valori predetti dell'anno precedente
      pred_levels[i] <- pred_levels[i - seasonal_period] + diff_values[i]
    }
  }
  
  return(pred_levels)
}

# Estrai le previsioni dalla differenziazione stagionale
pred_d12_co2 <- predictions_staz$fcst$d12_CO2[, "fcst"]
pred_d12_gmsl <- predictions_staz$fcst$d12_GMSL[, "fcst"]

# Ricostruisci i livelli originali invertendo la trasformazione d12
pred_levels_co2 <- invert_seasonal_diff(pred_d12_co2, co2_ts, n_ahead = 12)
pred_levels_gmsl <- invert_seasonal_diff(pred_d12_gmsl, gmsl_ts, n_ahead = 12)

# Crea oggetti ts per le previsioni
start_pred_co2 <- time(co2_ts)[length(co2_ts)] + 1/12
start_pred_gmsl <- time(gmsl_ts)[length(gmsl_ts)] + 1/12

pred_ts_co2 <- ts(pred_levels_co2, start = start_pred_co2, frequency = 12)
pred_ts_gmsl <- ts(pred_levels_gmsl, start = start_pred_gmsl, frequency = 12)

# Plot delle serie storiche originali + previsioni ricostruite
par(mfrow=c(2,1))
ts.plot(co2_ts, pred_ts_co2, col = c("black", "red"), lty = c(1,2), 
        ylab = "CO2 (ppm)", main = "Previsioni CO2 in livelli (con trend e stagionalità)")
legend("topleft", legend = c("Serie storica", "Previsioni"), 
       col = c("black", "red"), lty = c(1,2))

ts.plot(gmsl_ts, pred_ts_gmsl, col = c("black", "blue"), lty = c(1,2), 
        ylab = "GMSL (mm)", main = "Previsioni GMSL in livelli (con trend e stagionalità)")
legend("topleft", legend = c("Serie storica", "Previsioni"), 
       col = c("black", "blue"), lty = c(1,2))
par(mfrow=c(1,1))



##### Test di Granger #####
# Serie originali
par(mfrow = c(2, 1))
plot.ts(dati_comuni$CO2_monthly_avg, main = "CO2 Monthly Average", ylab = "ppm", col = "darkgreen")
plot.ts(dati_comuni$gmsl_gia, main = "GMSL (gia)", ylab = "mm", col = "blue")
par(mfrow = c(1, 1))

# Granger test

cat("\nGranger causality test: GMSL → CO₂ (diff. stagionali)\n")
print(grangertest(d12_co2_ts ~ d12_gmsl_ts, order = 3))

cat("Granger causality test: CO₂ → GMSL (diff. stagionali)\n")
print(grangertest(d12_gmsl_ts ~ d12_co2_ts, order = 3))

##### Cointegrazione #####
# Regressione ai minimi quadrati: GMSL ~ CO2
eg_reg <- lm(gmsl_gia ~ CO2_monthly_avg, data = dati_comuni)

# Residui
residui <- eg_reg$residuals

# Test ADF sui residui
adf.test(residui)

# Residui = error correction term
ECT <- resid(eg_reg)
ecm <- data.frame(
  D_gmsl = c(diff(dati_comuni$gmsl_gia)),
  D_co2  = c(diff(dati_comuni$CO2_monthly_avg)),
  ECT    = ECT[-1]
)
#modello
ecm_model <- lm(D_gmsl ~ D_co2 + ECT, data = ecm)
summary(ecm_model)


# --- PREVISIONI CO2 (da tuo SARIMA) ---
# pred contiene le previsioni da arimaest(), stime future della CO2+
n_ahead <- 24
plott <- forecastplott(data, co2_pred, n_ahead)
co2_future <- plott$forecast 

# Estendo le serie
gmsl_hist <- dati_comuni$gmsl_gia
co2_hist  <- dati_comuni$CO2_monthly_avg
ECT_hist  <- resid(eg_reg)

# Vettori estesi
gmsl_ext <- c(gmsl_hist, rep(NA, n_ahead))
co2_ext  <- c(co2_hist,  co2_future)
ECT_ext  <- c(ECT_hist, rep(NA, n_ahead))

# Previsioni dinamiche ECM
for (t in (length(gmsl_hist) + 1):(length(gmsl_hist) + n_ahead)) {
  # differenza CO2
  d_co2_t <- co2_ext[t] - co2_ext[t - 1]
  # error correction term
  ect_t   <- gmsl_ext[t - 1] - coef(eg_reg)[1] - coef(eg_reg)[2] * co2_ext[t - 1]
  # previsione differenza GMSL
  d_gmsl_t <- coef(ecm_model)["(Intercept)"] +
    coef(ecm_model)["D_co2"] * d_co2_t +
    coef(ecm_model)["ECT"]   * ect_t
  # ricostruzione livello
  gmsl_ext[t] <- gmsl_ext[t - 1] + d_gmsl_t
}

gmsl_ecm_forecast <- tail(gmsl_ext, n_ahead)
# Dati storici
df_storico <- data.frame(
  Periodo = 1:length(gmsl_hist),
  GMSL = gmsl_hist,
  Tipo = "Storico"
)

# Previsioni ECM
df_ecm <- data.frame(
  Periodo = (length(gmsl_hist) + 1):(length(gmsl_hist) + n_ahead),
  GMSL = gmsl_ecm_forecast,
  Tipo = "ECM"
)

# Unione
df_plot <- rbind(df_storico, df_ecm)

# Plot
ggplot(df_plot, aes(x = Periodo, y = GMSL, color = Tipo, linetype = Tipo)) +
  geom_line(size = 1, linetype = 'solid') +
  scale_color_manual(values = c("Storico" = "black", "ECM" = "blue")) +
  labs(
    title = "Previsioni GMSL con modello ECM",
    x = "Periodo",
    y = "GMSL",
    color = "Serie",
    linetype = "Serie"
  ) +
  theme_minimal(base_size = 14)


##########################
# ADL su serie COINTEGRATE
##########################
install.packages("dynlm")
install.packages('ARDL')
library(dynlm)
library(ARDL)
library(forecast)
# Numero massimo di lag da includere
p <- 2  # lag di CO2
q <- 2  # lag di GMSL

gmsl_ts <- ts(gmsl$gmsl_gia,
              start = c(as.numeric(substr(gmsl$year_month[1], 1, 4)),
                              as.numeric(substr(gmsl$year_month[1], 6, 7))),
                            frequency = 12)
co2_ts <- ts(dati_comuni$CO2_monthly_avg, start = c(as.numeric(substr(dati_comuni$year_month[1], 1, 4)),
                                                    as.numeric(substr(dati_comuni$year_month[1], 6, 7))),
             frequency = 12)
# Stima modello ARDL: GMSL in funzione dei lag di GMSL e CO2
ardl_model <- dynlm(
  gmsl_ts ~ L(gmsl_ts, 1:q) + L(co2_ts, 0:p)
)

summary(ardl_model)

# Coefficienti
cf <- coef(ardl_model)

# Lunghezze e lag
q <- 2  # lag su Y
p <- 2  # lag su X

# Serie estese
y_ext <- c(as.numeric(gmsl_ts), rep(NA, 30))
x_ext <- c(as.numeric(co2_ts), co2_future)

# Funzione one-step forecast
predict_one_step <- function(t, cf, q, p, y_ext, x_ext){
  y_part <- sum(sapply(1:q, function(j) cf[paste0("L(gmsl_ts, 1:q)", j)] * y_ext[t - j]))
  x_part <- sum(sapply(0:p, function(k) cf[paste0("L(co2_ts, 0:p)", k)] * x_ext[t - k]))
  return(cf["(Intercept)"] + y_part + x_part)
}

# Calcolo previsioni 30-step
for(t in (length(gmsl_ts)+1):(length(gmsl_ts)+30)){
  y_ext[t] <- predict_one_step(t, cf, q, p, y_ext, x_ext)
}

gmsl_ardl_forecast <- y_ext[(length(gmsl_ts)+1):(length(gmsl_ts)+30)]

# Creo un unico dataframe compatto
df_plott <- data.frame(
  Periodo = c(1:length(gmsl_ts), (length(gmsl_ts)+1):(length(gmsl_ts)+30)),
  GMSL = c(as.numeric(gmsl_ts), gmsl_ardl_forecast),
  Tipo = rep(c("Storico","ARDL"), c(length(gmsl_ts), 30))
)

# Plot compatto
ggplot(df_plott, aes(x = Periodo, y = GMSL, color = Tipo)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("Storico" = "black", "ARDL" = "red")) +
  labs(title = "Previsioni GMSL con modello ARDL", x = "Periodo", y = "GMSL") +
  theme_minimal(base_size = 14)


#############################
### Prev 300 lag forward ####
#############################
# Coefficienti
cf <- coef(ardl_model)

# Lunghezze e lag
q <- 2  # lag su Y
p <- 2  # lag su X

# Numero di step di previsione
n_forecast <- 300
plott <- forecastplott(data, co2_pred, n_forecast)
co2_future <- plott$forecast 

# Serie estese con spazio per le previsioni future
y_ext <- c(as.numeric(gmsl_ts), rep(NA, n_forecast))
x_ext <- c(as.numeric(co2_ts), co2_future)  # co2_future deve avere almeno n_forecast valori

# Funzione one-step forecast
predict_one_step <- function(t, cf, q, p, y_ext, x_ext){
  y_part <- sum(sapply(1:q, function(j) cf[paste0("L(gmsl_ts, 1:q)", j)] * y_ext[t - j]))
  x_part <- sum(sapply(0:p, function(k) cf[paste0("L(co2_ts, 0:p)", k)] * x_ext[t - k]))
  return(cf["(Intercept)"] + y_part + x_part)
}

# Calcolo previsioni n_forecast-step
for(t in (length(gmsl_ts)+1):(length(gmsl_ts)+n_forecast)){
  y_ext[t] <- predict_one_step(t, cf, q, p, y_ext, x_ext)
}

gmsl_ardl_forecast <- y_ext[(length(gmsl_ts)+1):(length(gmsl_ts)+n_forecast)]

# Creo un unico dataframe compatto per storico + previsione
df_plott <- data.frame(
  Periodo = c(1:length(gmsl_ts), (length(gmsl_ts)+1):(length(gmsl_ts)+n_forecast)),
  GMSL = c(as.numeric(gmsl_ts), gmsl_ardl_forecast),
  Tipo = rep(c("Storico","ARDL"), c(length(gmsl_ts), n_forecast))
)

# Plot compatto
ggplot(df_plott, aes(x = Periodo, y = GMSL, color = Tipo)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("Storico" = "black", "ARDL" = "red")) +
  labs(title = paste("Previsioni GMSL con modello ARDL a", n_forecast, "step"), x = "Periodo", y = "GMSL") +
  theme_minimal(base_size = 14)