
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


start_date <- "2019-01-01" 
end_date <- "2023-11-15"
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
df <- df_raw %>% filter(date >= start_date, date <= end_date)
View(df)

# df$date_time <- as.POSIXct(df$date_time)

# Open PDF device with specified width and height
pdf("seminar/power/power_data.pdf", width = 20, height = 5)
# Your plotting code here
plot(x = df$date_time, y = df$gesamt, type = "l", lwd = 2,
    xlab = "Date", ylab = "Electricity Consumption [1000 MWh]",
    main = "Hourly Electricity Consumption in Germany")
# Close PDF device
dev.off()




#### show weekly seasonality
df <- df %>%
    mutate(weekday = weekdays(date))

df_weekday <- df %>%
    group_by(weekday) %>%
    summarise(mean = mean(gesamt))

german_weekdays <- c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag")
english_weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

df$weekday <- factor(df$weekday, levels = german_weekdays)


pdf("seminar/power/power_weekday.pdf", width = 10, height = 5)
plot(df$weekday, df$gesamt, type = "l", 
     xlab = "Weekday", ylab = "Energy Consumption [1000 MWh]", 
     main = "Average Electricity Consumption in Germany by Weekday", 
     xaxt = "n")  # suppress x-axis labels

# Add x-axis labels
axis(1, at = 1:length(german_weekdays), labels = english_weekdays)

# Add gridlines
grid()
dev.off()



#### show daily seasonality
df$Anfang <- factor(df$Anfang)

pdf("seminar/power/power_hourly.pdf", width = 10, height = 5)
plot(df$Anfang, df$gesamt, type = "l", 
     xlab = "Hour", ylab = "Energy Consumption [1000 MWh]", 
     main = "Average Electricity Consumption in Germany by Hour")  # suppress x-axis labels

grid()
dev.off()



#### show annual seasonality
df <- df %>%
    mutate(day = yday(date_time))

df$day <- factor(df$day)

df_day <- df %>%
    group_by(day) %>%
    summarise(mean = mean(gesamt))

pdf("seminar/power/power_day.pdf", width = 10, height = 5)
plot(df_day$day, df_day$mean, type = "l", lwd = 2,
     xlab = "Day of Year", ylab = "Energy Consumption [1000 MWh]", 
     main = "Average Electricity Consumption in Germany by Day of Year",
          xaxt = "n")  
axis(1, at = seq(1, 365, by = 30))  # Adjust the sequence as needed
dev.off()

