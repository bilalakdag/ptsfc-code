rm(list = ls())
library(tidyverse)
library(xtable)

#################################################################
##                         Import Data                         ##
#################################################################
# quantile scores qr
quantile_scores_qr <- data.frame()
for (i in 1:12) {
    # name <- paste0("week", i)
    path <- paste0("seminar//dax//qr//results//daxQR_qs_week", i, ".csv")
    tmp <- read.csv(path)
    quantile_scores_qr <- rbind(quantile_scores_qr, tmp)
}

# quantiles qr
quantiles_qr <- data.frame()
for (i in 1:12) {
    # name <- paste0("week", i)
    path <- paste0("seminar//dax//qr//results//daxQR_data_week", i, ".csv")
    tmp <- read.csv(path)
    quantiles_qr <- rbind(quantiles_qr, tmp)
}
quantiles_qr


# quantile  scores arma garch
quantile_scores_ag <- data.frame()
for (i in 1:12) {
    # name <- paste0("week", i)
    path <- paste0("seminar//dax//armagarch//results//daxAG_qs_week", i, ".csv")
    tmp <- read.csv(path)
    quantile_scores_ag <- rbind(quantile_scores_ag, tmp)
}

# quantiles arma garch
quantiles_ag <- data.frame()
for (i in 1:12) {
    # name <- paste0("week", i)
    path <- paste0("seminar//dax//armagarch//results//daxAG_data_week", i, ".csv")
    tmp <- read.csv(path)
    quantiles_ag <- rbind(quantiles_ag, tmp)
}
quantiles_ag



#################################################################
##                   Analyze Quantile Scores                   ##
#################################################################

#################### Performance each week ##########################
# quantile scores of a week
weeks <- c("Week 1", "Week 2", "Week 3", "Week 4", "Week 5", "Week 6", "Break", "Break", "Week 7", "Week 8", "Week 9", "Week 10", "Week 11", "Week 12")

mean_qs_qr_forecast_date <- quantile_scores_qr %>%
    group_by(forecast_date) %>%
        summarise(mean_qs_qr = mean(mean_qs)) %>%
    mutate(forecast_date = as.Date(forecast_date)) 


mean_qs_ag_forecast_date <- quantile_scores_ag %>%
    group_by(forecast_date) %>%
        summarise(mean_qs_ag = mean(mean_qs)) %>%
    mutate(forecast_date = as.Date(forecast_date))


mean_qs_forecast_date_plot <- mean_qs_qr_forecast_date %>%
    ggplot(aes(x=forecast_date, y = mean_qs_qr)) +
    geom_point(aes(col = "Quantile Regression"), size = 4) +
    geom_point(aes(x= forecast_date, y = mean_qs_ag_forecast_date$mean_qs_ag, col = "ARMA-GARCH"), size = 4) +   
    scale_x_date(breaks = seq(as.Date("2023-11-15"), max(mean_qs_qr_forecast_date$forecast_date), by = "1 week"), labels = weeks) +
    scale_y_continuous(limits = c(0,1.1),   breaks = seq(0, 1, by = 0.25)) +  # Adjust y-axis breaks as needed
    xlab("Week of Challenge") + 
    ylab("Average Quantile Score") +
    labs(colour = "Model"
        # caption = "Average quantile scores for all forecasting weeks using quantile regression and ARMA-GARCH models"
        ) +
    theme_classic() +
    theme(axis.text.x = element_text(size = 12),  # Adjust size as needed
          axis.text.y = element_text(size = 12),  # Adjust size of axis tick labels as needed
          axis.title.x = element_text(size = 14), # Adjust size of x-axis label
          axis.title.y = element_text(size = 14), # Adjust size of y-axis label
          panel.border = element_rect(color = "black", fill = NA, size = 1)) # Add panel border

# Save the plot as a PDF
ggsave("seminar/dax/figures/mean_qs_forecast_date_dax.pdf", plot = mean_qs_forecast_date_plot)

qs_forecast_date <- mean_qs_qr_forecast_date %>%
    left_join(mean_qs_ag_forecast_date, by = "forecast_date", keep = FALSE) %>%
    mutate(forecast_date = as.character(forecast_date))

print(
    xtable(x = qs_forecast_date,
            type = "latex", 
            digits = 2, 
            label = "dax_mean_qs_forecast_date",
            caption = "Average quantile scores per forecasting week for quantile regression and ARMA-GARCH"),
    include.rownames = FALSE,
    file = "seminar/dax/mean_qs_forecast_date_dax.latex")


############################################ Overall QS and coverage, per quantile, per horizon, 

###### QR
quantile_means_qr <- colMeans(quantile_scores_qr[, 5:10 ])
quantile_means_qr

interval_coverage_0.5_qr <- (quantiles_qr$ret >= quantiles_qr$q0.25 & quantiles_qr$ret <= quantiles_qr$q0.75) 
interval_coverage_0.95_qr <- (quantiles_qr$ret >= quantiles_qr$q0.025 & quantiles_qr$ret <= quantiles_qr$q0.975)

coverage_probability_0.5_qr <- sum(interval_coverage_0.5_qr) / length(interval_coverage_0.5_qr)
coverage_probability_0.95_qr <- sum(interval_coverage_0.95_qr) / length(interval_coverage_0.95_qr)

results_qr <- c(quantile_means_qr, coverage_probability_0.95_qr, coverage_probability_0.5_qr) 



###### AG
quantile_means_ag <- colMeans(quantile_scores_ag[, 5:10 ])
quantile_means_ag

interval_coverage_0.5_ag <- (quantiles_ag$ret >= quantiles_ag$q0.25 & quantiles_ag$ret <= quantiles_ag$q0.75) 
interval_coverage_0.95_ag <- (quantiles_ag$ret >= quantiles_ag$q0.025 & quantiles_ag$ret <= quantiles_ag$q0.975)

coverage_probability_0.5_ag <- sum(interval_coverage_0.5_ag) / length(interval_coverage_0.5_ag)
coverage_probability_0.95_ag <- sum(interval_coverage_0.95_ag) / length(interval_coverage_0.95_ag)

results_ag <- c(quantile_means_ag, coverage_probability_0.95_ag, coverage_probability_0.5_ag) 

###### Combine
results <- rbind(results_qr, results_ag)
results <- t(results)

print(
    xtable(x = results,
            type = "latex", 
            digits = 2, 
            label = "dax_eval",
            caption = "Forecast evaluation for DAX"),
    file = "seminar/dax/eval.latex")

