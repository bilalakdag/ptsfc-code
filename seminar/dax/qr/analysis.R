rm(list = ls())
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



# mean quantile scores: quantiles
quantile_means <- colMeans(quantile_scores[, 5:10 ])
quantile_means
mean_qs_quantile <- data.frame(horizon = "average across quantile",
           qs0.025 = mean(quantile_scores$qs0.025),
           qs0.25 = mean(quantile_scores$qs0.25),
           qs0.50 = mean(quantile_scores$qs0.50),
           qs0.75 = mean(quantile_scores$qs0.75),
           qs0.975 = mean(quantile_scores$qs0.975),
           mean_qs = mean(quantile_scores$mean_qs))
mean_qs_quantile
# mean quantile scores: horizon

mean_qs_horizon <- quantile_scores %>%
    group_by(horizon) %>%
    summarise(across(qs0.025:mean_qs, ~mean(.))) %>%
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
    file = "seminar/dax/qr/qr_mean_qs.latex")

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

coverage <- data.frame(
    coverage_probability_0.5,
    coverage_probability_0.95
)
colnames(coverage) <- c("50% Interval", "95% Interval")
coverage <- t(coverage)
colnames(coverage) <- "Coverage Probability"

print(
    xtable(x = coverage,
            type = "latex", 
            digits = 2, 
            label = "dax_qr_coverage",
            caption = "Coverage probability for predicted DAX returns using quantile regression. "),
    # include.rownames = FALSE,
    file = "seminar/dax/qr/qr_coverage.latex")



#################### Performance each week ##########################
# quantile scores of a week
mean_qs_forecast_date <- quantile_scores %>%
    group_by(forecast_date) %>%
        summarise(mean_qs = mean(mean_qs))

quantile_scores %>%
    group_by(forecast_date) %>%
    ggplot(aes(x = horizon, y = mean_qs, group = forecast_date, col = forecast_date)) + 
    geom_line()

#################################################################
##                Analyze/Visualize Predictions                ##
#################################################################

weeks <- c("Week 1", "Week 2", "Week 3", "Week 4", "Week 5", "Week 6", "Break", "Break", "Week 7", "Week 8", "Week 9", "Week 10", "Week 11", "Week 12")
quantiles_forplot <- quantiles %>%
    mutate(date = as.Date(date)) %>%
    mutate(forecast_date = as.Date(forecast_date))%>%
    as_tibble()

cumLogReturnQR <- quantiles_forplot %>%
  pivot_longer(cols = c(ret, q0.025:q0.975), names_to = "Legend", values_to = "Return") %>%
  ggplot(aes(x = date, y = Return, group = Legend, col = Legend)) + 
  geom_line() +
  geom_vline(xintercept = as.Date("2023-11-30"), linetype = "dashed", color = "red") + 
  geom_vline(xintercept = as.Date("2023-12-06"), linetype = "dashed", color = "red") + 
  geom_vline(xintercept = as.Date("2024-01-11"), linetype = "dashed", color = "pink") + 
  geom_vline(xintercept = as.Date("2024-01-17"), linetype = "dashed", color = "pink") + 
  geom_vline(xintercept = as.Date("2024-01-18"), linetype = "dashed", color = "violet") + 
  geom_vline(xintercept = as.Date("2024-01-24"), linetype = "dashed", color = "violet") + 
    scale_x_date(breaks = seq(as.Date("2023-11-15"), max(quantiles_forplot$forecast_date), by = "1 week"), labels = weeks) +
    # scale_y_continuous(limits = c(-4,4),   breaks = seq(0, 1, by = 0.25)) +  # Adjust y-axis breaks as needed
    xlab("Week of Challenge") + 
  scale_color_manual(values = c("ret" = "black", "q0.025" = "lightgray", "q0.25" = "lightgray", "q0.5" = "lightgray", "q0.75" = "lightgray", "q0.975" = "lightgray")) + 
  theme_classic() +  # Minimal theme
#   labs(title = "Cumulative Log-Returns (Black) and Prediction Quantiles (Grey)",
#         subtitle = "Vertical lines contain forecast weeks in which the ARMA-GARCH model predicted poorly") + 
  xlab("Week") + 
    theme_classic() +
    theme(axis.text.x = element_text(size = 12),  # Adjust size as needed
          axis.text.y = element_text(size = 12),  # Adjust size of axis tick labels as needed
          axis.title.x = element_text(size = 14), # Adjust size of x-axis label
          axis.title.y = element_text(size = 14), # Adjust size of y-axis label
          panel.border = element_rect(color = "black", fill = NA, size = 1)) + # Add panel border 
          theme(legend.position = "none") 

# Save the plot as a PDF
ggsave("seminar/dax/qr/cumulative_log_returns_qr.pdf", plot = cumLogReturnQR)
