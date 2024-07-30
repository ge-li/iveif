fit_outcome_model <- function(data) {
  model_correct <- lm(y ~ a + x1 + x2 + c, data = data)
  model_misspecified <- lm(y ~ a + z + xt1 + xt2, data = data)

  predict_outcome_correct <- function(a, z, x1, x2, c) {
    newdata <- data.frame(a = a, x1 = x1, x2 = x2, c = c)
    predict(model_correct, newdata = newdata)
  }

  predict_outcome_misspecified <- function(a, z, xt1, xt2) {
    newdata <- data.frame(a = a, z = z, xt1 = xt1, xt2 = xt2)
    predict(model_misspecified, newdata = newdata)
  }

  sigma_correct <- sigma(model_correct)
  prob_Y_leq_y_correct <- function(y, a, z, x1, x2, c) {
    mu <- predict_outcome_correct(a, z, x1, x2, c)
    pnorm((y - mu) / sigma_correct)
  }

  sigma_misspecified <- sigma(model_misspecified)
  prob_Y_leq_y_misspecified <- function(y, a, z, xt1, xt2) {
    mu <- predict_outcome_misspecified(a, z, xt1, xt2)
    pnorm((y - mu) / sigma_misspecified)
  }

  list(
    model_correct = model_correct,
    model_misspecified = model_misspecified,
    predict_outcome_correct = predict_outcome_correct,
    predict_outcome_misspecified = predict_outcome_misspecified,
    prob_Y_leq_y_correct = prob_Y_leq_y_correct,
    prob_Y_leq_y_misspecified = prob_Y_leq_y_misspecified
  )
}
