get_confint <- function(est, se, level = 0.95,
                        conf.type = c('plain', 'log', 'log-log', 'logit', 'arcsin')) {
  conf.type <- match.arg(conf.type)

  # Z-score for the given confidence level
  alpha <- 1 - level
  z <- qnorm(1 - alpha / 2)

  # est[est <= 0] <- 0
  # est[est >= 1] <- 1

  lower <- numeric(length(est))
  upper <- numeric(length(est))

  for (i in seq_along(est)) {
    if (conf.type == 'log') {
      # Log transformation
      log_est <- log(est[i])
      lower[i] <- exp(log_est - z * se[i] / est[i])
      upper[i] <- exp(log_est + z * se[i] / est[i])
    } else if (conf.type == 'log-log') {
      # Log-log transformation
      log_log_est <- log(-log(est[i]))
      lower[i] <- exp(-exp(log_log_est - z * se[i] / (est[i] * log(est[i]))))
      upper[i] <- exp(-exp(log_log_est + z * se[i] / (est[i] * log(est[i]))))
    } else if (conf.type == 'plain') {
      # Plain (no transformation)
      lower[i] <- est[i] - z * se[i]
      upper[i] <- est[i] + z * se[i]
    } else if (conf.type == 'logit') {
      # Logit transformation
      logit_est <- log(est[i] / (1 - est[i]))
      lower[i] <- 1 / (1 + exp(-(logit_est - z * se[i] / (est[i] * (1 - est[i])))))
      upper[i] <- 1 / (1 + exp(-(logit_est + z * se[i] / (est[i] * (1 - est[i])))))
    } else if (conf.type == 'arcsin') {
      # Arcsine square root transformation
      arcsin_est <- asin(sqrt(est[i]))
      lower[i] <- (sin(arcsin_est - z * se[i] / (2 * sqrt(est[i] * (1 - est[i])))))^2
      upper[i] <- (sin(arcsin_est + z * se[i] / (2 * sqrt(est[i] * (1 - est[i])))))^2
    }

    # Ensure bounds are within [0, 1]
    lower[i] <- max(0, min(lower[i], 1))
    upper[i] <- max(0, min(upper[i], 1))
  }

  return(data.frame(lower = lower, upper = upper))
}
