# Week 4: Forecast DAX returns using a Quantile Regression model

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

# useful functions
source("seminar/dax/dax_procs.R")
source("seminar/dax/functions.R")

# relevant quantiles
tau <- c(.025,.25,.5,.75,.975)
start_date <- "2015-01-01" 

# settings
week = 4
forecast_date <- as.Date("2023-12-06") 
end_date <- as.Date(forecast_date) + days(8)  

##################################################################
##                      Initial data frame                      ##
##################################################################

symbol <- "^GDAXI"
dax_raw <- tq_get(symbol, from = start_date, to = end_date, get = "stock.prices") %>%
  dplyr::select(date, adjusted) 
  
# drop NAs
any(is.na(dax_raw))
dax_raw %>% filter(is.na(adjusted))
dax_raw <- na.omit(dax_raw)


dax_raw <- tq_get(symbol, from = start_date, to = end_date, get = "stock.prices") %>%
  dplyr::select(date, adjusted) 


# Brauche das eigentlich nur noch für garch_fitted_fcst$date <- forecast_df$date 
weekdays <- c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag")
dax_tmp <- dax_raw %>% filter(date <= forecast_date)
forecast_dates <- seq(max(dax_tmp$date) + 1, by = "days", length.out = 7)
forecast_df <- data.frame(date = forecast_dates)
forecast_df <- forecast_df %>%
    mutate(weekday = weekdays(date),
        adjusted = NA) %>%
    filter(weekday %in% weekdays) %>%
    dplyr::select(date, adjusted)



# calculate log-returns
dax <- dax_raw %>%
    mutate(
         ret1 = compute_return(adjusted, h = 1), 
         ret2 = compute_return(adjusted, h = 2),
         ret3 = compute_return(adjusted, h = 3),
         ret4 = compute_return(adjusted, h = 4),
         ret5 = compute_return(adjusted, h = 5),
         ret1_sqrd = compute_return(adjusted, h = 1)^2,
         ret_sqrd_lag1 = lag(ret1_sqrd, 1),
         ret_sqrd_lag2 = lag(ret1_sqrd, 2),
         ret_sqrd_lag3 = lag(ret1_sqrd, 3),
         ret_sqrd_lag4 = lag(ret1_sqrd, 4),
         ret_sqrd_lag5 = lag(ret1_sqrd, 5)
    ) %>% 
    as_tibble() %>%
    dplyr::select(date, ret1:ret5, ret_sqrd_lag1:ret_sqrd_lag5) %>%
    filter(!is.na(ret5))

# View(dax)

#################################################################
##                            GARCH                            ##
#################################################################

##### Mit For-Schleife ######     
dax_forts <- dax %>%
        dplyr::select(date, ret1:ret5) %>%
        filter(date <= forecast_date,
               if_all(.cols = all_of(2:6), ~ !is.na(.)))  # keine NAs

length <- nrow(dax_forts)

garch_fitted_data <- data.frame(
  date = dax_forts$date,
  garch_fit1 = numeric(length),
  garch_fit2 = numeric(length),
  garch_fit3 = numeric(length),
  garch_fit4 = numeric(length),
  garch_fit5 = numeric(length)
)


garch_sigma_data <-  data.frame(
  date = dax_forts$date,
  garch_sigma1 = numeric(length),
  garch_sigma2 = numeric(length),
  garch_sigma3 = numeric(length),
  garch_sigma4 = numeric(length),
  garch_sigma5 = numeric(length)
)

length_fcst <- 5
garch_fitted_fcst <- data.frame(
  garch_fit1 = numeric(length_fcst),
  garch_fit2 = numeric(length_fcst),
  garch_fit3 = numeric(length_fcst),
  garch_fit4 = numeric(length_fcst),
  garch_fit5 = numeric(length_fcst)
)

garch_sigma_fcst <-  data.frame(
  garch_sigma1 = numeric(length_fcst),
  garch_sigma2 = numeric(length_fcst),
  garch_sigma3 = numeric(length_fcst),
  garch_sigma4 = numeric(length_fcst),
  garch_sigma5 = numeric(length_fcst)
)

# get values from garch model for returns of respective lags 
for(i in 1:5) {
tmp <- dax_forts

dax_ts <- xts(tmp[, i+1], order.by = tmp$date)

# Just use ARMA(1,1)-GARCH(1,1) model
spec <- ugarchspec(mean.model=list(armaOrder=c(1,1)), 
                  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)))

fit <- ugarchfit(spec, data = dax_ts)

garch_forecast <- ugarchforecast(fit, n.ahead = 5)

garch_fitted_data[i+1] <- fitted(fit)
garch_sigma_data[i+1] <- sigma(fit)

garch_fitted_fcst[i] <- fitted(garch_forecast)
garch_sigma_fcst[i] <- sigma(garch_forecast)
}

garch_fitted_fcst$date <- forecast_df$date 
garch_fitted_fcst <- garch_fitted_fcst %>% relocate(date)
# colnames(garch_fitted_fcst) <- colnames(garch_fitted_data) 
garch_fitted <- rbind(garch_fitted_data, garch_fitted_fcst)

garch_sigma_fcst$date <- forecast_df$date 
garch_sigma_fcst <- garch_sigma_fcst %>% relocate(date)
# colnames(garch_fitted_fcst) <- colnames(garch_fitted_data) 
garch_sigma <- rbind(garch_sigma_data, garch_sigma_fcst)

garch_df <- garch_fitted %>% 
    left_join(garch_sigma, by = "date", keep = FALSE)



##################################################################
##                           Holidays                           ##
##################################################################

# holidays
holidays <- readRDS("seminar/holidays_since2015.RDS")
dax <- dax %>% mutate(isHoliday = ifelse(date %in% holidays, 1, 0))



#################################################################
##                      Create data frame                      ##
#################################################################

df_forfcst <- dax %>%      #dax_forts %>%
  right_join(garch_df, by = "date", keep = FALSE) %>%
  # left_join(dax %>% select(date, ret_sqrd_lag1:ret_sqrd_lag5), by = "date", keep = FALSE) %>%
  mutate(weekday = weekdays(date)) #%>%
  #filter(date >= "2018-01-25") # Am Donnerstag starten, damit alles korrekt?

# View(df_forfcst)


#################################################################
##                     Quantile Regression                     ##
#################################################################
# Split the data into training and testing sets
dax_train <- df_forfcst %>%
    filter(date <= forecast_date)

dax_fcst <- df_forfcst %>%
    filter(date > forecast_date)


### Alle manuell

rqfit_ret1 <- rq(ret1 ~ ret_sqrd_lag1 + weekday + isHoliday + garch_sigma1,
                 tau = tau, data = dax_train)

rqfit_ret2 <- rq(ret2 ~ret_sqrd_lag2 + weekday + isHoliday + garch_sigma2,
                 tau = tau, data = dax_train)

rqfit_ret3 <- rq(ret3 ~ ret_sqrd_lag3  + weekday + isHoliday + garch_sigma3, 
                 tau = tau, data = dax_train)

rqfit_ret4 <- rq(ret4 ~ ret_sqrd_lag4 + weekday + isHoliday + garch_sigma4,
                 tau = tau, data = dax_train)

rqfit_ret5 <- rq(ret5 ~ ret_sqrd_lag5 + weekday + isHoliday + garch_sigma5,
                 tau = tau, data = dax_train)



##################################################################
##                        Evaluate Model                        ##
##################################################################

# Model fit
# summary(rqfit_ret1)
# summary(rqfit_ret2)
# summary(rqfit_ret3)
# summary(rqfit_ret4)
# summary(rqfit_ret5)




##################################################################
##                           Forecast                           ##
##################################################################

dax_fcst <- dax_fcst %>%
    mutate(
        ret1_fc = predict(rqfit_ret1, newdata = ., interval = "confidence"),
        ret2_fc = predict(rqfit_ret2, newdata = ., interval = "confidence"),
        ret3_fc = predict(rqfit_ret3, newdata = ., interval = "confidence"),
        ret4_fc = predict(rqfit_ret4, newdata = ., interval = "confidence"),
        ret5_fc = predict(rqfit_ret5, newdata = ., interval = "confidence")
    )

# dax_fcst %>%
#   dplyr::select(date,ret1:ret5, ret1_fc:ret5_fc) %>% 
#   View()

##################################################################
##               Create table and submission file               ##
##################################################################

abgabe = matrix(nrow = 5, ncol = 5)
start <- ncol(dax_fcst) - 5
for(i in 1:5) {
    for(j in 1:5) {
        abgabe[i,j] <- dax_fcst[[i, start+i]][j]
    }
}

# quantile_comparison_plot(list(abgabe), 
#                          model_names = c("Base"))
# abline(h = 0, lwd = .5, lty = 2)


pred_df_dax <- data.frame(forecast_date = forecast_date, 
                      target = "DAX", horizon = paste(c(1, 2, 5:7), "day"),
                      q0.025 = NA, q0.25 = NA, q0.5 = NA, q0.75 = NA, 
                      q0.975 = NA)
pred_df_dax[,4:8] <- abgabe
pred_df_dax <- pred_df_dax %>% replace(is.na(.),0)




# View(pred_df_dax)

# Save quantiles
filename <- paste0("daxQR_subQuantiles_week", week, ".csv")
write.csv(pred_df_dax, here("seminar/dax/qr/results", filename), row.names = FALSE) 



#################################################################
##         Save predictions and real values with dates         ##
#################################################################
pred <- pred_df_dax
pred$date <- dax_fcst$date
pred <- pred %>%
  dplyr::select(date, q0.025:q0.975)

y <- y_for_eval(data = dax, forecast_date = forecast_date)

data <- y %>% left_join(pred, by = "date", keep = FALSE)
data$horizon <- paste(c(1, 2, 5:7), "day")
data$forecast_date <- forecast_date
data$target <- "DAX"
data <- data %>%
  dplyr::select(forecast_date,target,date,horizon,ret,q0.025:q0.975)


filename_data <- paste0("daxQR_data_week", week, ".csv")
write.csv(data, here("seminar/dax/qr/results", filename_data), row.names = FALSE)


#################################################################
##                      Evaluate forecast                      ##
#################################################################

# daily quantile score, mean score, and summarized score
pred_for_qs <- pred %>% dplyr::select(q0.025:q0.975) # get date out to calculate qs

qs <- qs_5days(y_true = y, y_pred = pred_for_qs, tau = tau) %>%
  rowwise() %>%
  mutate(mean_qs = mean(c_across(2:6)),
        sum_qs = sum(across(qs0.025:qs0.975)))

qs$forecast_date <- forecast_date
qs$horizon <- paste(c(1, 2, 5:7), "day")

qs$target <- "DAX"

qs <- qs %>%
    dplyr::select(forecast_date,target,date,horizon,qs0.025:qs0.975,mean_qs,sum_qs)

# Save quantile scores
filename_qs <- paste0("daxQR_qs_week", week, ".csv")
write.csv(qs, here("seminar/dax/qr/results" , filename_qs), row.names = FALSE) 

