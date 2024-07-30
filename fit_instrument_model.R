fit_instrument_model <- function(data) {
  model_correct <- glm(z ~ x1 + x2, data = data, family = binomial)
  model_misspecified <- glm(z ~ xt1 + xt2, data = data, family = binomial)

  predict_correct <- function(x1, x2) {
    newdata <- data.frame(x1 = x1, x2 = x2)
    predict(model_correct, newdata = newdata, type = "response")
  }

  predict_misspecified <- function(xt1, xt2) {
    newdata <- data.frame(xt1 = xt1, xt2 = xt2)
    predict(model_misspecified, newdata = newdata, type = "response")
  }

  list(
    model_correct = model_correct,
    model_misspecified = model_misspecified,
    predict_correct = predict_correct,
    predict_misspecified = predict_misspecified
  )
}
