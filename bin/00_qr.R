# Week 1: Forecast DAX returns using a Quantile Regression model

rm(list = ls())
set.seed(1103)

# packages
library(tidyverse) # collection of packages
library(zoo) # for time series
library(quantreg) # modeling framework for quantile regression
library(rugarch) # for ARMA-GARCH model
library(xts) # for time series
library(tidyquant) # import financial data
library(lubridate) # for dates and times
library(forecast) # for forecasting
library(fitdistrplus) # for distributions
library(here) # for directory
library(readxl)

# useful functions
source("seminar/dax/dax_procs.R")
source("seminar/dax/functions.R")

# relevant quantiles
tau <- c(.025,.25,.5,.75,.975)

# settings
week = 1
start_date <- "2015-01-01" 
forecast_date <- as.Date("2023-11-15") 
end_date <- as.Date(forecast_date) + days(8) 

# last_day <- as.Date("2024-02-17") # jeweiliger Samstag



##################################################################
##                      Initial Data Frame                      ##
##################################################################

power_hourly_raw <- as_tibble(read.csv2("seminar/power/qr/Realisierter_Stromverbrauch_Stunde.csv", sep = ";", dec = ","))
power_hourly  <-  power_hourly_raw %>% 
    dplyr::select(1,2,3,4) %>% 
    rename("gesamt" = 4)
power_hourly$gesamt <- gsub("\\.", "", power_hourly$gesamt)
power_hourly$gesamt <-  gsub("\\,", ".", power_hourly$gesamt)
power_hourly <- power_hourly %>% 
    mutate(gesamt = as.numeric(gesamt))


# create date_time column
df_raw <- power_hourly %>%
    mutate(gesamt = gesamt / 1000, 
            date = as.Date(Datum, format = "%d.%m.%Y"),
           date_time =  as.POSIXct(paste(date, paste(Anfang, ":00", sep = ""), sep = " "), format = "%Y-%m-%d %H:%M:%S"),
           weekday = weekdays(date)) %>%
    filter(!is.na(gesamt))

df_raw <- df_raw %>%
    dplyr::select(date_time,gesamt,weekday,Anfang)

View(df_raw)



#################################################################
##                             EDA                             ##
#################################################################

#### Uhrzeit und Wochentagcluster

df <- df_raw %>%
    mutate(night = if_else(Anfang %in% c("00:00", "01:00", "02:00", "03:00", "04:00"), 1, 0),
             earlyMorning = if_else(Anfang %in% c("05:00", "06:00"), 1, 0), 
            lateEvening = if_else(Anfang %in% c("21:00", "22:00", "23:00"), 1, 0), 
            workingHours = if_else(night == 0 & earlyMorning == 0 & lateEvening == 0, 1, 0)    
)

# df <- df_raw %>%
#     mutate(
#         day_time = if_else(Anfang %in% c("00:00", "01:00", "02:00", "03:00", "04:00"), "night", NA) ,
#         day_time = if_else(Anfang %in% c("05:00", "06:00"), "earlyMorning", NA),
#         day_time = if_else(Anfang %in% c("21:00", "22:00", "23:00"), "lateEvening", NA),
#         day_time = if_else(day_time != "night" &  day_time != "earlyMorning" & day_time != "lateEvening", "workingHours", NA)
#     )

# weekday_mapping = c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag")
# df$weekday_numeric <- match(df$weekday, weekday_mapping)

# Create dummy variables for each day of the week
df <- df %>%
  mutate(isMonday = as.numeric(weekday == "Montag"),
         isTuesday = as.numeric(weekday == "Dienstag"),
         isWednesday = as.numeric(weekday == "Mittwoch"),
         isThursday = as.numeric(weekday == "Donnerstag"),
         isFriday = as.numeric(weekday == "Freitag"),
         isSaturday = as.numeric(weekday == "Samstag"),
         isSunday = as.numeric(weekday == "Sonntag"))

View(df)

# Assuming lm is the linear regression model
model <- lm(gesamt ~ weekday + Anfang, data = df)
step_model <- step(model)
summary(step_model)

# Assuming lm is the linear regression model
model <- lm(gesamt ~ isMonday + isTuesday + isWednesday + isThursday + isFriday + isSaturday + isSunday + Anfang, data = df)
step_model <- step(model)
summary(step_model) 


# Assuming lm is the linear regression model
model <- lm(gesamt ~ weekday + night + earlyMorning + lateEvening + workingHours, data = df)
step_model <- step(model)
summary(step_model) 

cor(df[, c(2, 5:15) ]) %>% View()

## Wochentag und Tageszeit sehr relevant, Clustern verschlechter R^2 etwas
## Kein Unterschied ob Wochentag als Faktor oder binary variablen


#### Monat/Jahreszeit
# Monat als Zahl oder Faktor machen?

df <- df %>%
    mutate(month = months(date_time))

model <- lm(gesamt ~ month + weekday + Anfang, data = df)
step_model <- step(model)
summary(step_model) 



#################################################################
##                            Trend                            ##
#################################################################


# Durchschnittliche tägliche demand im monat berechnen. mit decomp trend holen, für wochenende forecasten und als regressor verwenden
library(fpp3)
df %>% mutate(date = as.Date(date_time)) %>% dplyr::select(date)


df_ts <- df %>%
    dplyr::select(date_time,gesamt) %>%
    mutate(
        date = as.Date(date_time),
        ym = yearmonth(date_time)
    ) 


    
df_ts <- df_ts %>%
    group_by(date) %>%
    summarise(gesamt = sum(gesamt)) %>%  # daily demand 
    ungroup() 

# df_ts <- df_ts[-1, ] # 
df_ts <- df_ts[-nrow(df_ts),] # nimm laufenden TAg raus


# df_ts <- df_ts%>%
#     group_by(ym) %>%
#     summarise(gesamt = mean(gesamt))    # avg daily demand per month



df_ts <- tsibble(df_ts, index = date)
# df_monthly_ts <- df_monthly_ts[-1,] # nimm ersten monat raus, wenn nicht vollständig
# df_monthly_ts <- df_monthly_ts[-nrow(df_monthly_ts),] # nimm laufenden monat raus

# Decompose the time series
decomp <- df_ts %>%
    model(stl = STL(gesamt))

decomp %>%
    components() %>%
    autoplot()


comp_ts <- decomp %>%
    components() %>%
    dplyr::select(date,trend) #,remainder)


# make forecast for trend and remainder, feed them into qr later
comp_model_trend <- comp_ts %>%
    model(
        trend_ets = ETS(trend)
    ) 

trend_fcst <- comp_model_trend %>% 
    forecast(h= 4 )


trend_fcst  %>% View()

trend_fcst_df <- data.frame( date = trend_fcst$date, trend = trend_fcst$.mean)

trend_df <- bind_rows(comp_ts, trend_fcst_df)

## mach später auch mit remainder

# Wetterdaten: Temperatur, Wind, Niederschlag, ...



# Futureprices, usw


#################################################################
##                         Modify Data                         ##
#################################################################
# The three hours starting on Friday, 12:00, 16:00 and 20:00.
# (36, 40, and 44 hours after submission deadline)
# The three hours starting on Saturday, 12:00, 16:00 and 20:00. (60, 64, and 68 hours after submission
# deadline)

max_datetime <- max(df$date_time) + hours(1)

end_datetime <- as.POSIXct(paste(last_day, paste("20:00:00", sep = ""), sep = " "), format = "%Y-%m-%d %H:%M:%S")


future_datetimes <- seq(from = max_datetime, to = end_datetime, by = "hour")

future <- data.frame(date_time = future_datetimes, gesamt = NA) %>%
    mutate(weekday = weekdays(date_time), 
            Anfang = format(date_time, "%H:%M"),
            month = months(date_time))
    


## Combine 
df_withFuture <- rbind(df %>% dplyr::select(date_time, gesamt,weekday,Anfang,month), future) 

df_withFuture <- df_withFuture %>% 
    mutate(date = as.Date(date_time)) %>%
    left_join(trend_df, by = "date", keep = FALSE)

# df_withFuture <- df_withFuture[-1,]

#################################################################
##                       test predictors                       ##
#################################################################

train <- df_withFuture %>%
    filter(!is.na(gesamt))

fcst <-  df_withFuture %>%
    filter(is.na(gesamt))

model <- lm(gesamt ~ trend + month + weekday + Anfang, data = train)
step_model <- step(model)
summary(step_model) 




#################################################################
##                     Quantile Regression                     ##
#################################################################

# Specify quantile levels
tau <- c(.025,.25,.5,.75,.975)

rq_fit <- rq(gesamt ~ trend + month + weekday + Anfang, data = train, tau = tau)

summary(rq_fit)

fitted(rq_fit)
plot(residuals(rq_fit))
head(residuals(rq_fit))
head(fitted(rq_fit)[,1])
head(fitted(rq_fit))

plot(x = df_train$date_time, y = rq_fit$y, col = "gray", ylim = c(10,100))
lines(x = df_train$date_time, y = fitted(rq_fit)[,1], type = "l", col = "#f4f805")
lines(x = df_train$date_time, y = fitted(rq_fit)[,2])
lines(x = df_train$date_time, y = fitted(rq_fit)[,3])
lines(x = df_train$date_time, y = fitted(rq_fit)[,4])
lines(x = df_train$date_time, y = fitted(rq_fit)[,5], type = "l", col = "#f4f805")





##################################################################
##                           Forecast                           ##
##################################################################

fcst <- fcst %>%
    mutate(
        gesamt_fc = predict(rq_fit, newdata = ., interval = "confidence")
    )


plot(x = fcst$date_time, y = fcst$gesamt_fc[,1], type = "l", ylim = c(0, 100))
for(i in 2:5) {
    lines(x = fcst$date_time, y = fcst$gesamt_fc[,i], type = "l")
}



##################################################################
##                            Abgabe                            ##
##################################################################



relevant <- fcst %>%
    filter(weekday %in% c("Freitag", "Samstag"),
            Anfang %in% c("12:00", "16:00", "20:00")) %>%
            dplyr::select(gesamt_fc)

relevant[[1]][6,5]

abgabe_energy <- matrix(nrow = 6, ncol = 5)
for(i in 1:6) {
    for(j in 1:5) {
        abgabe_energy[i,j] <- relevant[[1]][i,j]
    }
}



pred_df_energy <- data.frame(forecast_date = forecast_date, 
                      target = "energy", horizon = paste(c(36, 40, 44, 60, 64, 68), "hour"),
                      q0.025 = NA, q0.25 = NA, q0.5 = NA, q0.75 = NA, 
                      q0.975 = NA)

pred_df_energy[,4:8] <- abgabe_energy

pred_df_energy

saveRDS(pred_df_energy, here("energy", "abgabe_energy.RDS"))