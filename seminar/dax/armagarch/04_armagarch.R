# Week 4: Forecast DAX returns using an ARMA-GARCH model
# Select optimal parameter combination for model

rm(list = ls())
set.seed(1103)

# packages
library(tidyverse) # collection of packages
library(zoo) # for time series
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

# future dataframe
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
         ret5 = compute_return(adjusted, h = 5)
    ) %>% 
    as_tibble() %>%
    dplyr::select(date, ret1:ret5) %>%
    filter(!is.na(ret5))


##################################################################
##                Find the relevant distribution                ##
##################################################################

# hist(dax$ret1, freq = FALSE)
# lines(density(dax$ret1))
# descdist(dax$ret1, discrete = FALSE)

# Initialize a list to store the results
fit_results <- list()

# ret 1
fit_results[["norm1"]] <- fitdist(dax$ret1, "norm")
fit_results[["cauchy1"]] <- fitdist(dax$ret1, "cauchy")
fit_results[["t1"]] <- fitdist(dax$ret1, "t", start = list(df= 2))

# ret 2
fit_results[["norm2"]] <- fitdist(dax$ret2, "norm")
fit_results[["cauchy2"]] <- fitdist(dax$ret2, "cauchy")
fit_results[["t2"]] <- fitdist(dax$ret2, "t", start = list(df= 2))

# ret 3
fit_results[["norm3"]] <- fitdist(dax$ret3, "norm")
fit_results[["cauchy3"]] <- fitdist(dax$ret3, "cauchy")
fit_results[["t3"]] <- fitdist(dax$ret3, "t", start = list(df= 2))

# ret 4
fit_results[["norm4"]] <- fitdist(dax$ret4, "norm")
fit_results[["cauchy4"]] <- fitdist(dax$ret4, "cauchy")
fit_results[["t4"]] <- fitdist(dax$ret4, "t", start = list(df= 2))

# ret 5
fit_results[["norm5"]] <- fitdist(dax$ret5, "norm")
fit_results[["cauchy5"]] <- fitdist(dax$ret5, "cauchy")
fit_results[["t5"]] <- fitdist(dax$ret5, "t", start = list(df= 2))

summary(fit_results[["norm1"]])
summary(fit_results[["cauchy1"]])
summary(fit_results[["t1"]])

summary(fit_results[["norm2"]])
summary(fit_results[["cauchy2"]])
summary(fit_results[["t2"]])

summary(fit_results[["norm3"]])
summary(fit_results[["cauchy3"]])
summary(fit_results[["t3"]])

summary(fit_results[["norm4"]])
summary(fit_results[["cauchy4"]])
summary(fit_results[["t4"]])

summary(fit_results[["norm5"]])
summary(fit_results[["cauchy5"]])
summary(fit_results[["t5"]])

# Use t-distribution


############################################################################
############################################################################
###                                                                      ###
###                              ARMA-GARCH                              ###
###                                                                      ###
############################################################################
############################################################################

# Split Data at forecast date
dax_train <- dax %>%
    filter(date <= forecast_date)

dax_test <- dax %>%
    filter(date > forecast_date )

# Calculate optimal parameter combinations 

arma_set_ret1  <- arma_params(dax_train$ret1,6,6)
arma_set_ret1[order(arma_set_ret1$BIC)[],]
arma_set_ret1 <-  arma_set_ret1[order(arma_set_ret1$BIC)[1],] 
arma_p_ret1  <- arma_set_ret1[,1]
arma_q_ret1  <- arma_set_ret1[,2]
param_set_ret1 <- arma_garch_params(dax_train$ret1, "std", arma_p_ret1, arma_q_ret1,6,6) 
param_set_ret1[order(param_set_ret1$BIC)[],] 
param_set_ret1 <-  param_set_ret1[order(param_set_ret1$BIC)[1],] 
param_ret1 <- param_set_ret1[1:4] 

arma_set_ret2  <- arma_params(dax_train$ret2,6,6)
arma_set_ret2[order(arma_set_ret2$BIC)[],]
arma_set_ret2 <-  arma_set_ret2[order(arma_set_ret2$BIC)[1],] 
arma_p_ret2  <- arma_set_ret2[,1]
arma_q_ret2  <- arma_set_ret2[,2]
param_set_ret2 <- arma_garch_params(dax_train$ret2, "std", arma_p_ret2, arma_q_ret2,6,6) 
param_set_ret2[order(param_set_ret2$BIC)[],] 
param_set_ret2 <-  param_set_ret2[order(param_set_ret2$BIC)[1],] 
param_ret2 <- param_set_ret2[1:4] 

arma_set_ret3  <- arma_params(dax_train$ret3,6,6)
arma_set_ret3[order(arma_set_ret3$BIC)[],]
arma_set_ret3 <-  arma_set_ret3[order(arma_set_ret3$BIC)[1],] 
arma_p_ret3  <- arma_set_ret3[,1]
arma_q_ret3  <- arma_set_ret3[,2]
param_set_ret3 <- arma_garch_params(dax_train$ret3, "std", arma_p_ret3, arma_q_ret3,6,6)      
param_set_ret3[order(param_set_ret3$BIC)[],] 
param_set_ret3 <-  param_set_ret3[order(param_set_ret3$BIC)[1],] 
param_ret3 <- param_set_ret3[1:4] 

arma_set_ret4  <- arma_params(dax_train$ret4,6,6)
arma_set_ret4[order(arma_set_ret4$BIC)[],]
arma_set_ret4 <-  arma_set_ret4[order(arma_set_ret4$BIC)[1],] 
arma_p_ret4  <- arma_set_ret4[,1]
arma_q_ret4  <- arma_set_ret4[,2]
param_set_ret4 <- arma_garch_params(dax_train$ret4, "std", arma_p_ret4, arma_q_ret4,6,6) 
param_set_ret4[order(param_set_ret4$BIC)[],] 
param_set_ret4 <-  param_set_ret4[order(param_set_ret4$BIC)[1],] 
param_ret4 <- param_set_ret4[1:4] 

arma_set_ret5  <- arma_params(dax_train$ret5,6,6)
arma_set_ret5[order(arma_set_ret5$BIC)[],]
arma_set_ret5 <-  arma_set_ret5[order(arma_set_ret5$BIC)[1],] 
arma_p_ret5  <- arma_set_ret5[,1]
arma_q_ret5  <- arma_set_ret5[,2]
param_set_ret5 <- arma_garch_params(dax_train$ret5, "std", arma_p_ret5, arma_q_ret5,6,6) 
param_set_ret5[order(param_set_ret5$BIC)[],] 
param_set_ret5 <-  param_set_ret5[order(param_set_ret5$BIC)[1],] 
param_ret5 <- param_set_ret5[1:4] 

param_sets <- list()
param_sets[["ret1"]] <- as.vector(as.numeric(unlist(param_ret1)))
param_sets[["ret2"]] <- as.vector(as.numeric(unlist(param_ret2)))
param_sets[["ret3"]] <- as.vector(as.numeric(unlist(param_ret3)))
param_sets[["ret4"]] <- as.vector(as.numeric(unlist(param_ret4)))
param_sets[["ret5"]] <- as.vector(as.numeric(unlist(param_ret5)))

filename_param_sets <- paste0("seminar/dax/armagarch/params/param_sets", week, ".csv")
write.csv(param_sets, filename_param_sets ,row.names = FALSE)


##################################################################
##                       Fit and Forecast                       ##
##################################################################
# Create Time Series
dax_forts <- dax_train %>%
        dplyr::select(date, ret1:ret5) %>%
        filter(date <= forecast_date,
               if_all(.cols = all_of(2:6), ~ !is.na(.)))  # keine NAs

# Create empty data frames for fitted values and sigmas
length <- nrow(dax_forts)

fitted_data <- data.frame(
  date = dax_forts$date,
  fit1 = numeric(length),
  fit2 = numeric(length),
  fit3 = numeric(length),
  fit4 = numeric(length),
  fit5 = numeric(length)
)

sigma_data <-  data.frame(
  date = dax_forts$date,
  sigma1 = numeric(length),
  sigma2 = numeric(length),
  sigma3 = numeric(length),
  sigma4 = numeric(length),
  sigma5 = numeric(length)
)

length_fcst <- 5
fitted_fcst <- data.frame(
  fit1 = numeric(length_fcst),
  fit2 = numeric(length_fcst),
  fit3 = numeric(length_fcst),
  fit4 = numeric(length_fcst),
  fit5 = numeric(length_fcst)
)

sigma_fcst <-  data.frame(
  sigma1 = numeric(length_fcst),
  sigma2 = numeric(length_fcst),
  sigma3 = numeric(length_fcst),
  sigma4 = numeric(length_fcst),
  sigma5 = numeric(length_fcst)
)


shape <- data.frame() # to store degree of freedom for t-distribution

# get values from garch model for returns of respective lags 
for(i in 1:5) {
tmp <- dax_forts

dax_ts <- xts(tmp[, i+1], order.by = tmp$date)

spec <- ugarchspec(mean.model=list(armaOrder= param_sets[[i]][1:2]),  #c(1,1)), 
                  variance.model = list(model = "sGARCH", garchOrder =  param_sets[[i]][3:4]),                #c(1, 1)),
                  distribution.model = "std")

fit <- ugarchfit(spec, data = dax_ts)

forecast <- ugarchforecast(fit, n.ahead = 5)

shape[i,1] <- coef(fit)[["shape"]] 

fitted_data[i+1] <- fitted(fit)
sigma_data[i+1] <- sigma(fit)

fitted_fcst[i] <- fitted(forecast)
sigma_fcst[i] <- sigma(forecast)
}

shape

# Create df with observed and predicted fitted values, and sigmas
fitted_fcst$date <- forecast_df$date 
fitted_fcst <- fitted_fcst %>% relocate(date)
# colnames(garch_fitted_fcst) <- colnames(garch_fitted_data) 
fitted <- rbind(fitted_data, fitted_fcst)

sigma_fcst$date <- forecast_df$date 
sigma_fcst <- sigma_fcst %>% relocate(date)
# colnames(garch_fitted_fcst) <- colnames(garch_fitted_data) 
sigma <- rbind(sigma_data, sigma_fcst)

df <- fitted %>% 
    left_join(sigma, by = "date", keep = FALSE)


#################################################################
##                     Calculate Quantiles                     ##
#################################################################

dax_quantiles <- data.frame()
for(i in 1:5) {
  dax_quantiles[i,1] <- fitted_fcst[[i,i+1]] + sigma_fcst[[i,i+1]] * qt(p = 0.025, df = shape[i,1])
  dax_quantiles[i,2] <- fitted_fcst[[i,i+1]] + sigma_fcst[[i,i+1]]  * qt(p = 0.25, df = shape[i,1])
  dax_quantiles[i,3] <- fitted_fcst[[i,i+1]] + sigma_fcst[[i,i+1]]  * qt(p = 0.5, df = shape[i,1])
  dax_quantiles[i,4] <- fitted_fcst[[i,i+1]] + sigma_fcst[[i,i+1]]  * qt(p = 0.75, df = shape[i,1])
  dax_quantiles[i,5] <- fitted_fcst[[i,i+1]] + sigma_fcst[[i,i+1]]  * qt(p = 0.975, df = shape[i,1])
}
dax_quantiles

# Submission dataframe
pred_df_dax <- data.frame(forecast_date = forecast_date, 
                      target = "DAX", horizon = paste(c(1, 2, 5:7), "day"),
                      q0.025 = NA, q0.25 = NA, q0.5 = NA, q0.75 = NA, 
                      q0.975 = NA)
pred_df_dax[,4:8] <- dax_quantiles
pred_df_dax <- pred_df_dax %>% replace(is.na(.),0)

# View(pred_df_dax)

# Save quantiles
filename <- paste0("daxAG_subQuantiles_week", week, ".csv")
write.csv(pred_df_dax, here("seminar/dax/armagarch/results", filename), row.names = FALSE)

# alt
rugarch::quantile(fitted_fcst[1,2], sigma_fcst[1,2], 0.5)


#################################################################
##         Save predictions and real values with dates         ##
#################################################################
# daily quantile score, mean score, and summarized score
pred <- dax_quantiles
colnames(pred) <- c("q0.025", "q0.25", "q0.5", "q0.75", "q0.975")
pred$date <- fitted_fcst$date
pred <- pred %>%
  dplyr::select(date, q0.025:q0.975)

y <- y_for_eval(data = dax, forecast_date = forecast_date)

data <- y %>% left_join(pred, by = "date", keep = FALSE)
data$horizon <- paste(c(1, 2, 5:7), "day")
data$forecast_date <- forecast_date
data$target <- "DAX"
data <- data %>%
  dplyr::select(forecast_date,target,date,horizon,ret,q0.025:q0.975)

filename_data <- paste0("daxAG_data_week", week, ".csv")
write.csv(data, here("seminar/dax/armagarch/results", filename_data), row.names = FALSE)


############################################################################
############################################################################
###                                                                      ###
###                              EVALUATION                              ###
###                                                                      ###
############################################################################
############################################################################


# daily quantile score, mean score, and summarized score
pred_for_qs <- pred %>% dplyr::select(q0.025:q0.975) # get date out to calculate qs

qs <- qs_5days(y_true = y, y_pred = pred_for_qs, tau = tau) %>%
  rowwise() %>%
  mutate(mean_qs = mean(c_across(2:6)),
        sum_qs = sum(across(qs0.025:qs0.975)))
qs

qs$forecast_date <- forecast_date
qs$horizon <- paste(c(1, 2, 5:7), "day")

qs$target <- "DAX"

qs <- qs %>%
    dplyr::select(forecast_date,target,date,horizon,qs0.025:qs0.975,mean_qs,sum_qs)

# Save quantile scores
filename_qs <- paste0("daxAG_qs_week", week, ".csv")
write.csv(qs, here("seminar/dax/armagarch/results" , filename_qs), row.names = FALSE)

