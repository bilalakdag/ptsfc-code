# Week 7: Forecast DAX returns using a Quantile Regression model

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
library(fpp3)

# useful functions
source("seminar/dax/dax_procs.R")
source("seminar/dax/functions.R")

# relevant quantiles
tau <- c(.025,.25,.5,.75,.975)
start_date <- "2019-01-01" 

# settings
week = 7
forecast_date <- as.Date("2024-01-10") 

end_date <- as.Date(forecast_date) + days(3) 
forecast_dt <- paste(forecast_date, "23:00:00")
end_dt <- paste(end_date, "23:00:00")



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
    dplyr::select(date_time, gesamt,weekday,Anfang, date)

# View(df_raw)




#################################################################
##                       Create Features                       ##
#################################################################
# Filter data frame
df <- df_raw %>%
    filter(date_time <= end_dt, date >= start_date) 

# Create month
df <- df %>%
    mutate(month = months(date_time))

# daylight hours: take from python
daylight_df <- as_tibble(read.csv("seminar/power/daylight_hours.csv")) %>%
    rename(date = X) %>%
    mutate(date = as.Date(date))

df <- df %>%
    left_join(daylight_df, by = "date", keep = FALSE )

# holidays
holidays <- readRDS("seminar/holidays.RDS")
df <- df %>% mutate(isHoliday = ifelse(date %in% holidays, 1, 0))
# View(df)

df_train <- df %>%
    filter(date_time <= forecast_dt)

#################################################################
##                            Trend                            ##
#################################################################


# Durchschnittliche tägliche demand im monat berechnen. mit decomp trend holen, für wochenende forecasten und als regressor verwenden
# df %>% mutate(date = as.Date(date_time)) %>% dplyr::select(date)

df_ts <- df_train %>%
    dplyr::select(date_time, date, gesamt) %>%
    mutate(
        # date = as.Date(date_time),
        ym = yearmonth(date_time)
    ) 


    
df_ts <- df_ts %>%
    group_by(date) %>%
    summarise(gesamt = sum(gesamt)) %>%  # daily demand 
    ungroup() 


# df_ts <- df_ts[-nrow(df_ts),] # drop current date; not relevant ex post

df_ts <- tsibble(df_ts, index = date)

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
    forecast(h= 3 )  # forecast 4 days (Thursday, Friday, Saturday, Sunday)


# trend_fcst  %>% View()

trend_fcst_df <- data.frame( date = trend_fcst$date, trend = trend_fcst$.mean)

trend_df <- bind_rows(comp_ts, trend_fcst_df)



#################################################################
##                         Modify Data                         ##
#################################################################
# The three hours starting on Friday, 12:00, 16:00 and 20:00.
# (36, 40, and 44 hours after submission deadline)
# The three hours starting on Saturday, 12:00, 16:00 and 20:00. (60, 64, and 68 hours after submission
# deadline)

# max_datetime <- max(df$date_time) + hours(1)

# end_datetime <- as.POSIXct(paste(end_date, paste("23:00:00", sep = ""), sep = " "), format = "%Y-%m-%d %H:%M:%S")


# future_datetimes <- seq(from = max_datetime, to = end_datetime, by = "hour")

# future <- data.frame(date_time = future_datetimes, gesamt = NA) %>%
#     mutate(weekday = weekdays(date_time), 
#             Anfang = format(date_time, "%H:%M"),
#             month = months(date_time),
#             isHoliday = ifelse(date %in% holidays, 1, 0))
    
## Combine 
# df_withFuture <- rbind(df %>% dplyr::select(date_time, gesamt,weekday,Anfang,month), future) 

# df_withFuture <- df_withFuture %>% 
#     mutate(date = as.Date(date_time)) %>%
#     left_join(trend_df, by = "date", keep = FALSE)

# df_withFuture <- df_withFuture[-1,]

df_withFuture <- df %>%
    left_join(trend_df, by = "date", keep = FALSE)

# View(df_withFuture)
#################################################################
##                       test predictors                       ##
#################################################################

train <- df_withFuture %>%
    filter(date_time <= forecast_dt)

test <-  df_withFuture %>%
    filter(date_time > forecast_dt)

model <- lm(gesamt ~ trend + month + weekday + Anfang + isHoliday + daylight, data = train)
step_model <- step(model)
summary(step_model) 


#################################################################
##                     Quantile Regression                     ##
#################################################################

# Specify quantile levels
tau <- c(.025,.25,.5,.75,.975)

rq_fit <- rq(gesamt ~ trend + month + weekday + Anfang + isHoliday + daylight, data = train, tau = tau)

# summary(rq_fit)

# fitted(rq_fit)
# plot(residuals(rq_fit))
# head(residuals(rq_fit))
# head(fitted(rq_fit)[,1])
# head(fitted(rq_fit))

# plot(x = df_train$date_time, y = rq_fit$y, col = "gray", ylim = c(10,100))
# lines(x = df_train$date_time, y = fitted(rq_fit)[,1], type = "l", col = "#f4f805")
# lines(x = df_train$date_time, y = fitted(rq_fit)[,2])
# lines(x = df_train$date_time, y = fitted(rq_fit)[,3])
# lines(x = df_train$date_time, y = fitted(rq_fit)[,4])
# lines(x = df_train$date_time, y = fitted(rq_fit)[,5], type = "l", col = "#f4f805")



##################################################################
##                           Forecast                           ##
##################################################################

test_fc <- test %>%
    mutate(
        gesamt_fc = predict(rq_fit, newdata = ., interval = "confidence")
    )


plot(x = test_fc$date_time, y = test_fc$gesamt, type = "l", ylim = c(0, 100), col = "orange")
for(i in 1:5) {
    lines(x = test_fc$date_time, y = test_fc$gesamt_fc[,i], type = "l", col = "gray")
}



##################################################################
##               Create table and submission file               ##
##################################################################

relevant <- test_fc %>%
    filter(weekday %in% c("Freitag", "Samstag"),
            Anfang %in% c("12:00", "16:00", "20:00")) %>%
            dplyr::select(gesamt_fc)

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

# Save quantiles
filename <- paste0("powerQR_subQuantiles_week", week, ".RDS")
saveRDS(pred_df_energy, here("seminar/power/qr/results", filename)) 


#################################################################
##         Save predictions and real values with dates         ##
#################################################################
# Save all predictions, with corresponding y and date-time value

predictions <- test_fc %>%
            dplyr::select(gesamt_fc)

pred_matrix <- matrix(nrow = nrow(test_fc), ncol = 5)
for(i in 1:nrow(test_fc)) {
    for(j in 1:5) {
        pred_matrix[i,j] <- predictions[[1]][i,j]
    }
}

pred_matrix

pred_df <- data.frame(forecast_date = forecast_date, 
                      target = "energy",
                      date_time = test_fc$date_time, y = test_fc$gesamt,
                      q0.025 = NA, q0.25 = NA, q0.5 = NA, q0.75 = NA, 
                      q0.975 = NA )

pred_df[,5:9] <- pred_matrix

# View(pred_df)


filename_data <- paste0("powerQR_quantiles_week", week, ".RDS")
saveRDS(data, here("seminar/power/qr/results", filename_data))

#################################################################
##                      Evaluate forecast                      ##
#################################################################

y_true <- pred_df %>% dplyr::select(date_time,y)
y_pred <- pred_df %>% dplyr::select(q0.025:q0.975)

qs <- qs_5days(y_true = y_true, y_pred = y_pred, tau = tau)

qs <- qs %>%
    rename(date_time = date) %>%
    rename(mean_qs = mean) %>%
    mutate(sum_qs = sum(across(qs0.025:qs0.975)))


# Save quantile scores
filename_qs <- paste0("powerQR_qs_week", week, ".RDS")
saveRDS(qs, here("seminar/power/qr/results" , filename_qs)) 


# Quantile scores for relevant hours
relevant_hours <- test_fc %>%
    filter(weekday %in% c("Freitag", "Samstag"),
            Anfang %in% c("12:00", "16:00", "20:00")) %>%
            dplyr::select(date_time) %>%
            pull(date_time) 

qs_sub <- qs %>%
    filter(date_time %in% relevant_hours) 
    
qs_sub$forecast_date <- forecast_date
qs_sub$horizon <- paste(c(36, 40, 44, 60, 64, 68), "hour")
qs_sub$target = "energy"

qs_sub <- qs_sub %>%
    dplyr::select(forecast_date,target,date_time,horizon,qs0.025:qs0.975,mean_qs,sum_qs)

filename_qsSub <- paste0("powerQR_subQS_week", week, ".RDS")
saveRDS(qs_sub, here("seminar/power/qr/results" , filename_qsSub)) 