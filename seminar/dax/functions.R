# calculate y_true: 
# von einem forecast_date aus für die nächsten 5-Tage die relevanten cum-returns berechnen
y_for_eval <- function(data, forecast_date) {
        df <- data %>%
          filter(date > forecast_date, date <= as.Date(forecast_date) + days(7)) %>%
          mutate(ret = cumsum(ret1)) %>%
          dplyr::select(date, ret)
        return(df)
}



# y_true:
# real <- dax %>% filter(date > "2023-11-08", date <= "2023-11-15") %>%
#   mutate(ret = cumsum(ret1)) %>%
#   dplyr::select(date, ret)
# y_pred: abgabe dataframe - columns are quantiles, rows are days

qs_5days <- function(y_true, y_pred, tau) {
  loss_observation <- data.frame(date = y_true[,1])
  for(i in 1:nrow(y_true)) {

    for(j in 1:length(tau)) {
      q <- tau[j]
      loss_observation[i,j+1] <- 2 * max(q * (y_true[i, 2] - y_pred[i,j]), (q - 1) * (y_true[i, 2] - y_pred[i,j])) # checke ob funktion richtig
    }
  } 

  loss_observation <- loss_observation  %>%         # calc mean for each row
      rowwise() %>%
      mutate(mean = mean(c_across(2:6)))

  colnames(loss_observation) <- c("date", "qs0.025", "qs0.25", "qs0.50", "qs0.75", "qs0.975", "mean")
  return(loss_observation)
}



# Function to fit ARMA models and return AIC and BIC
fit_arma_model <- function(p, q, data) {
  order <- c(p, q)
  arma_model <- arima(x = data, order = c(p, 0,  q))
  aic <- AIC(arma_model)
  bic <- BIC(arma_model)
  return(c(order, aic, bic))
}

### Für einen bestimmten return automatisch die passenden ARMA-GARCH Parameter bekommen
# df ist ist data.frame mit date, ret1, ..., ret5
arma_params <- function(df, p, q) {
p_values <- 1:p
q_values <- 1:q
results <- data.frame()
# Loop through different p and q values
    for(p in p_values) {
        for(q in q_values) {
            model_result <- fit_arma_model(p, q, df)
            results <- rbind(results, model_result)
        }
    }
# Add column names to the results data frame
colnames(results) <- c("p", "q", "AIC", "BIC")
return(results)
}





# Function to fit GARCH models and return AIC and BIC
fit_garch_model <- function(p, q, P, Q, df, distribution) {
  spec <- ugarchspec(mean.model = list(armaOrder = c(p, q)),
                    variance.model = list(garchOrder = c(P,Q)),
                    distribution.model = distribution)
  fit <- ugarchfit(spec, data = df, solver = "hybrid")
  aic <- infocriteria(fit)[1]
  bic <- infocriteria(fit)[2]
  return(c(p, q, P, Q, distribution, aic, bic))
}

### Für einen bestimmten return automatisch die passenden ARMA-GARCH Parameter bekommen
# df ist ist data.frame mit date, ret1, ..., ret5
arma_garch_params <- function(df, distribution, p, q, P, Q) {
P_values <- 1:P
Q_values <- 1:Q
results <- data.frame()
# Loop through different p and q values
    for(P in P_values) {
        for(Q in Q_values) {
            model_result <- fit_garch_model(p, q, P, Q, df, distribution)
            results <- rbind(results, model_result)
        }
    }
# Add column names to the results data frame
colnames(results) <- c("p", "q", "P", "Q", "distribution", "AIC", "BIC")
return(results)
}