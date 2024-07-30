fit_treatment_model <- function(data, lambda_init) {
  model_misspecified <- glm(a ~ z + xt1 + xt2, data = data, family = binomial)

  log_likelihood <- function(lambda, a, z, u) {
    p_complier <- exp(-lambda * u^2)
    p_always_taker <- (1 - p_complier) * (u <= 0)

    p <- p_always_taker + z * p_complier
    included <- p > 0 & p < 1
    # print(sum(included))
    pp <- p[included]

    log_likelihood <- -sum(a[included] * log(pp) + (1 - a[included]) * log(1 - pp))
    log_likelihood
  }

  model_correct <- optim(lambda_init, log_likelihood, a = data$a, z = data$z, u = data$u,
                         lower = 1e-3, method = "L-BFGS-B")

  lambda_hat <- model_correct$par

  predict_correct <- function(z, u) {
    p_complier <- exp(-lambda_hat * u^2)
    p_always_taker <- (1 - p_complier) * (u <= 0)
    p_always_taker + p_complier * z
  }

  predict_misspecified <- function(z, xt1, xt2) {
    newdata <- data.frame(z = z, xt1 = xt1, xt2 = xt2)
    predict(model_misspecified, newdata = newdata, type = "response")
  }

  list(
    model_misspecified = model_misspecified,
    model_correct = model_correct,
    predict_correct = predict_correct,
    predict_misspecified = predict_misspecified
  )
}
