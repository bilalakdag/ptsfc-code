library(tidyverse)
library(xtable)
#################################################################
##                         Import Data                         ##
#################################################################
# relevant quantiles
# subQuantiles <- data.frame()
# for (i in 1:12) {
#     # name <- paste0("week", i)
#     path <- paste0("seminar//dax//qr//results//daxQR_subQuantiles_week", i, ".csv")
#     tmp <- read.csv(path)
#     subQuantiles <- rbind(subQuantiles, tmp)
# }
# subQuantiles


# relevant quantiles and observed values
quantiles <- data.frame()
for (i in 1:12) {
    # name <- paste0("week", i)
    path <- paste0("seminar//dax//qr//results//daxQR_data_week", i, ".csv")
    tmp <- read.csv(path)
    quantiles <- rbind(quantiles, tmp)
}
quantiles

# quantile scores
quantile_scores <- data.frame()
for (i in 1:12) {
    # name <- paste0("week", i)
    path <- paste0("seminar//dax//qr//results//daxQR_qs_week", i, ".csv")
    tmp <- read.csv(path)
    quantile_scores <- rbind(quantile_scores, tmp)
}
quantile_scores 


#################################################################
##                   Analyze Quantile Scores                   ##
#################################################################

# quantile scores of a week
mean_qs_forecast_date <- quantile_scores %>%
    group_by(forecast_date) %>%
        summarise(mean_qs = mean(mean_qs))

quantile_scores %>%
    group_by(forecast_date) %>%
    ggplot(aes(x = horizon, y = mean_qs, group = forecast_date, col = forecast_date)) + 
    geom_line()

# mean quantile scores: quantiles
quantile_means <- colMeans(quantile_scores[, 5:11 ])
quantile_means
mean_qs_quantile <- data.frame(horizon = "average across quantile",
           qs0.025 = mean(quantile_scores$qs0.025),
           qs0.25 = mean(quantile_scores$qs0.25),
           qs0.50 = mean(quantile_scores$qs0.50),
           qs0.75 = mean(quantile_scores$qs0.75),
           qs0.975 = mean(quantile_scores$qs0.975),
           mean_qs = NA,
           sum_qs = NA)
mean_qs_quantile
# mean quantile scores: horizon

mean_qs_horizon <- quantile_scores %>%
    group_by(horizon) %>%
    summarise(across(qs0.025:sum_qs, ~mean(.))) %>%
    ungroup()

# combine
scores <- rbind(mean_qs_horizon, mean_qs_quantile)


# create table
print(
    xtable(x = scores,
            type = "latex", 
            digits = 2, 
            label = "dax_qr_qs",
            caption = "Quantile scores for predicted DAX returns using quantile regression. 
                       Averaged over the 12 weeks relevant for the challenge."),
    include.rownames = FALSE,
    file = "seminar/dax/qr/qrmean_qs_horizon.latex")

quantile_scores %>%
    ggplot(aes(x = horizon, y = mean_qs)) + 
    geom_boxplot() + 
    ggtitle("Quantile Scores across horizons")


# Running average of scores (like in shiny app)
score_horizon <- quantile_scores %>%
    group_by(horizon) %>%
    summarise(scores = mean(sum_qs))

score <- score_horizon %>% ungroup() %>% summarise(score = mean(scores))

# Prediction intervals


# Interval Score



# Calibration


# Coverage probability
interval_coverage_0.5 <- (quantiles$ret >= quantiles$q0.25 & quantiles$ret <= quantiles$q0.75) 
interval_coverage_0.95 <- (quantiles$ret >= quantiles$q0.025 & quantiles$ret <= quantiles$q0.975)

coverage_probability_0.5 <- sum(interval_coverage_0.5) / length(interval_coverage_0.5)
coverage_probability_0.95 <- sum(interval_coverage_0.95) / length(interval_coverage_0.95)



#################################################################
##                Analyze/Visualize Predictions                ##
#################################################################