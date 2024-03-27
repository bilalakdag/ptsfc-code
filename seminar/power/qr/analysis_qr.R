
library(tidyverse)


#################################################################
##                         Import Data                         ##
#################################################################
# relevant quantiles
subQuantiles <- data.frame()
for (i in 1:12) {
    # name <- paste0("week", i)
    path <- paste0("seminar//power//qr//results//powerQR_subQuantiles_week", i, ".RDS")
    tmp <- readRDS(path)
    subQuantiles <- rbind(subQuantiles, tmp)
}


# subQS <- list()
# for (i in 1:12) {
#     # name <- paste0("week", i)
#     path <- paste0("seminar//power//qr//results//powerQR_subQS_week", i, ".RDS")
#     subQS[[i]] <- readRDS(path)
# }

# relevant quantile scores
subQS <- data.frame()
for (i in 1:12) {
    # name <- paste0("week", i)
    path <- paste0("seminar//power//qr//results//powerQR_subQS_week", i, ".RDS")
    tmp <- readRDS(path)
    subQS <- rbind(subQS, tmp)
}

# observed values for outcome variable
y <- as_tibble(read.csv("seminar/power/y_energy.csv")) 
dates <- subQS$date_time
y <- y %>% filter(date_time %in% dates) 
y <- y %>% left_join(subQS %>% dplyr::select(date_time,horizon) %>% mutate(date_time = as.character(date_time)), by = "date_time", keep = FALSE) %>%
    dplyr::select(date_time, horizon, gesamt)

#################################################################
##                   Analyze Quantile Scores                   ##
#################################################################

# quantile scores of a week
mean_qs_forecast_date <- subQS %>%
    group_by(forecast_date) %>%
        summarise(mean_qs = mean(mean_qs))

subQS %>%
    group_by(forecast_date) %>%
    ggplot(aes(x = horizon, y = mean_qs, group = forecast_date, col = forecast_date)) + 
    geom_line()

# quantile scores for a horizon across weeks
mean_qs_horizon <- subQS %>%
    group_by(horizon) %>%
        summarise(mean_qs = mean(mean_qs))


subQS %>%
    ggplot(aes(x = horizon, y = mean_qs)) + 
    geom_boxplot() + 
    ggtitle("Quantile Scores across horizons")


# Running average of scores (like in shiny app)
score_horizon <- subQS %>%
    group_by(horizon) %>%
    summarise(scores = mean(sum_qs))

score <- score_horizon %>% ungroup() %>% summarise(score = mean(scores))

# Prediction intervals


# Interval Score



# Calibration


# Coverage probability
interval_coverage_0.5 <- (y$gesamt >= subQuantiles$q0.25 & y$gesamt <= subQuantiles$q0.75) 
interval_coverage_0.95 <- (y$gesamt >= subQuantiles$q0.025 & y$gesamt <= subQuantiles$q0.975)

coverage_probability_0.5 <- sum(interval_coverage_0.5) / length(interval_coverage_0.5)
coverage_probability_0.95 <- sum(interval_coverage_0.95) / length(interval_coverage_0.95)



#################################################################
##                Analyze/Visualize Predictions                ##
#################################################################


