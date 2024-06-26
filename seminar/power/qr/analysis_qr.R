
rm(list = ls())
library(tidyverse)
library(xtable)


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

# mean quantile scores: quantiles
quantile_means <- colMeans(subQS[, 5:10 ])
quantile_means
mean_qs_quantile <- data.frame(horizon = "average across quantile",
           qs0.025 = mean(subQS$qs0.025),
           qs0.25 = mean(subQS$qs0.25),
           qs0.50 = mean(subQS$qs0.50),
           qs0.75 = mean(subQS$qs0.75),
           qs0.975 = mean(subQS$qs0.975),
           mean_qs = mean(subQS$mean_qs))
mean_qs_quantile
# mean quantile scores: horizon

mean_qs_horizon <- subQS %>%
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
            label = "power_qr_qs",
            caption = "Quantile scores for predicted electricity consumption using quantile regression. 
                       Averaged over the 12 weeks relevant for the challenge."),
    include.rownames = FALSE,
    file = "seminar/power/qr/qr_mean_qs.latex")












subQS %>%
    ggplot(aes(x = horizon, y = mean_qs)) + 
    geom_boxplot() + 
    ggtitle("Quantile Scores across horizons")


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
            label = "power_qr_coverage",
            caption = "Coverage probability for predicted electricity consumption using quantile regression. "),
    # include.rownames = FALSE,
    file = "seminar/power/qr/qr_coverage.latex")









#################################################################
##                Analyze/Visualize Predictions                ##
#################################################################


#################################################################
##                          Visualize                          ##
#################################################################


weeks <- c("Week 1", "Week 2", "Week 3", "Week 4", "Week 5", "Week 6", "Break", "Break", "Week 7", "Week 8", "Week 9", "Week 10", "Week 11", "Week 12")

subQuantiles$date_time <- y$date_time

quantiles_forplot <- subQuantiles %>%
    # mutate(date = as.Date(date)) %>%
    mutate(forecast_date = as.Date(as.character(forecast_date), format="%Y%m%d")) %>%
    as_tibble()

quantiles_forplot <- quantiles_forplot %>%
    left_join(y %>% dplyr::select(date_time, gesamt), by = "date_time" , keep = FALSE)

# quantiles_forplot <- quantiles_forplot %>%
#     mutate(date = as.Date(date_time)) %>%
#     pivot_wider(names_from = horizon, values_from = c(gesamt, q0.025:q0.975))

quantiles_forplot <- quantiles_forplot %>%
    mutate(date_time = as.POSIXct(date_time))

quantiles_forplot %>%
    ggplot(aes(x=date_time, y = gesamt, group = horizon, col = horizon)) +
    geom_point() + 
    scale_x_datetime()


cumLogReturnQR <- quantiles_forplot %>%
  pivot_longer(cols = c(gesamt, q0.025,q0.975), names_to = "Legend", values_to = "Demand") %>% 
  ggplot(aes(x = date_time, y = Demand, group = Legend, color = Legend)) + 
  geom_point() +
#   geom_segment(aes(xend = date_time, yend = lag(Demand)), linetype = "solid") +
    scale_x_datetime(breaks = seq(as.POSIXct("2023-11-16 00:00:00"), as.POSIXct("2024-02-18 00:00:00"), by = "1 week"), labels = weeks,
                        minor_breaks = seq(as.POSIXct("2023-11-16 00:00:00"), as.POSIXct("2024-02-18 00:00:00"), by = "12 hours")
                        ) +
    # scale_y_continuous(limits = c(-4,4),   breaks = seq(0, 1, by = 0.25)) +  # Adjust y-axis breaks as needed
    xlab("Week of Challenge") + 
  scale_color_manual(values = c("gesamt" = "black", "q0.025" = "red", "q0.975" = "green")) + 
#   theme_classic() +  # Minimal themek
#   labs(title = "Cumulative Log-Returns (Black) and Prediction Quantiles (Grey)",
#         subtitle = "Vertical lines contain forecast weeks in which the ARMA-GARCH model predicted poorly") + 
  xlab("Week") + 
    # theme_classic() +
    theme(axis.text.x = element_text(size = 12),  # Adjust size as needed
          axis.text.y = element_text(size = 12),  # Adjust size of axis tick labels as needed
          axis.title.x = element_text(size = 14), # Adjust size of x-axis label
          axis.title.y = element_text(size = 14), # Adjust size of y-axis label
          panel.border = element_rect(color = "black", fill = NA, size = 1)) + # Add panel border 
          theme(legend.position = "none") 


quantiles_forplot %>%
    ggplot(aes(x=date_time, y = gesamt))+
    geom_point(color = "blue") +
    geom_line(aes(x = date_time, y = q0.025), color = "red") +
    geom_line(aes(x = date_time, y = q0.975), color = "red") +
    scale_x_datetime(breaks = seq(as.POSIXct("2023-11-16 00:00:00"), as.POSIXct("2024-02-18 00:00:00"), by = "1 week"), labels = weeks,
                        minor_breaks = seq(as.POSIXct("2023-11-16 00:00:00"), as.POSIXct("2024-02-18 00:00:00"), by = "12 hours")
                        ) 


# Save the plot as a PDF
ggsave("seminar/power/qr/cumulative_log_returns_qr.pdf", plot = cumLogReturnQR)


y %>%
  ggplot(aes(x = date_time, y = gesamt)) +
  geom_point() 



################################ per horizon

quantiles_forplot %>%
    filter(horizon == "36 hour") %>%
    ggplot(aes(x=date_time, y = q0.025)) +
    geom_point()

h36_gesamt <- quantiles_forplot %>%
    filter(horizon == "36 hour") %>%
    dplyr::select(gesamt)
h36_gesamt$week <- c("Week 1", "Week 2", "Week 3", "Week 4", "Week 5", "Week 6",  "Week 7", "Week 8", "Week 9", "Week 10", "Week 11", "Week 12")


plot(x = h36_gesamt$week, y = h36_gesamt$gesamt)
