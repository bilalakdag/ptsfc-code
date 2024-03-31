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

# useful functions
source("seminar/dax/dax_procs.R")
source("seminar/dax/functions.R")


start_date <- "2015-01-01" 



##################################################################
##                      Initial data frame                      ##
##################################################################

symbol <- "^GDAXI"
dax_raw <- tq_get(symbol, from = start_date, get = "stock.prices") %>%
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
    dplyr::select(date, adjusted, ret1:ret5, ret_sqrd_lag1:ret_sqrd_lag5) %>%
    filter(!is.na(ret5))



##################################################################
##                         Stationarity                         ##
##################################################################





#################################################################
##                          Visualize                          ##
#################################################################

pdf("seminar/dax/dax_stock_price.pdf")
plot(x = dax$date, y = dax$adjusted, type = "l",
     xlab = "Date", ylab = "Stock Price", 
     main = "DAX Stock Price",
     lwd = 2, # increase line width
     lty = 1, # solid line
     ) 
dev.off()

pdf("seminar/dax/dax_return1.pdf")
plot(x = dax$date, y = dax$ret1, type = "l",
     xlab = "Date", ylab = "Return", 
     main = "DAX Daily Log Returns Over Time",
     lwd = 2, # increase line width
     lty = 1, # solid line
     ylim = c(-20, 20)
     ) 
dev.off()

plot(x = dax$date, y = dax$ret2, type = "l",
     xlab = "Date", ylab = "Return", 
     main = "DAX Daily Log Returns",
     lwd = 2, # increase line width
     lty = 1, # solid line
     ylim = c(-20, 20)
     ) 

plot(x = dax$date, y = dax$ret3, type = "l",
     xlab = "Date", ylab = "Return", 
     main = "DAX Daily Log Returns Over Time",
     lwd = 2, # increase line width
     lty = 1, # solid line
     ylim = c(-20, 20)
     ) 



hist(dax$ret1)
