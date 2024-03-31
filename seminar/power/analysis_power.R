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
    path <- paste0("seminar//power//qr//results//powerQR_subQS_week", i, ".RDS")
    tmp <- readRDS(path)
    quantile_scores_qr <- rbind(quantile_scores_qr, tmp)
}
quantile_scores_qr

# quantiles qr
quantiles_qr <- data.frame()
for (i in 1:12) {
    # name <- paste0("week", i)
    path <- paste0("seminar//power//qr//results//powerQR_subQuantiles_week", i, ".RDS")
    tmp <- readRDS(path)
    quantiles_qr <- rbind(quantiles_qr, tmp)
}
quantiles_qr


# quantile  scores arma garch
quantile_scores_gb <- data.frame()
for (i in 1:12) {
    # name <- paste0("week", i)
    path <- paste0("seminar//power//gradboost//results//powerGB_subQS_week", i, ".csv")
    tmp <- read.csv(path)
    quantile_scores_gb <- rbind(quantile_scores_gb, tmp)
}
quantile_scores_gb
quantile_scores_gb$forecast_date <- quantile_scores_qr$forecast_date

# quantiles arma garch
quantiles_gb <- data.frame()
for (i in 1:12) {
    # name <- paste0("week", i)
    path <- paste0("seminar//power//gradboost//results//powerGB_subQuantiles_week", i, ".csv")
    tmp <- read.csv(path)
    quantiles_gb <- rbind(quantiles_gb, tmp)
}
quantiles_gb



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


mean_qs_gb_forecast_date <- quantile_scores_gb %>%
    group_by(forecast_date) %>%
        summarise(mean_qs_gb = mean(mean_qs)) %>%
    mutate(forecast_date = as.Date(forecast_date))

mean_qs_forecast_date_plot <- mean_qs_qr_forecast_date %>%
    ggplot(aes(x=forecast_date, y = mean_qs_qr)) +
    geom_point(aes(col = "Quantile Regression"), size = 4) +
    geom_point(aes(x= forecast_date, y = mean_qs_gb_forecast_date$mean_qs_gb, col = "Gradient Boosting"), size = 4) +   
    scale_x_date(breaks = seq(as.Date("2023-11-15"), max(mean_qs_qr_forecast_date$forecast_date), by = "1 week"), labels = weeks) +
    scale_y_continuous(limits = c(0,3),   breaks = seq(0, 3, by = 0.5)) +  # Adjust y-axis breaks as needed
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
ggsave("seminar/power/mean_qs_forecast_date_power.pdf", plot = mean_qs_forecast_date_plot)


qs_forecast_date <- mean_qs_qr_forecast_date %>%
    left_join(mean_qs_gb_forecast_date, by = "forecast_date", keep = FALSE) %>%
    mutate(forecast_date = as.character(forecast_date))

print(
    xtable(x = qs_forecast_date,
            type = "latex", 
            digits = 2, 
            label = "power_mean_qs_forecast_date",
            caption = "Average quantile scores per forecasting week for quantile regression and gradient boosting"),
    include.rownames = FALSE,
    file = "seminar/power/mean_qs_forecast_date_power.latex")
