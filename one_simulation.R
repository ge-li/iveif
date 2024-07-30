one_simulation <- function(n, alpha, lambda, beta_1, beta_2) {

  data <- generate_data_ge(n, alpha, lambda, beta_1, beta_2)

  sigma <- sqrt(beta_1^2 + beta_2^2 + 1)
  f_y1c_true <- function(y) {
    pnorm(y, mean = alpha, sd = sigma)
  }
  f_y0c_true <- function(y) {
    pnorm(y, mean = 0, sd = sigma)
  }

  instrument_model <- fit_instrument_model(data)
  pz_correct <- instrument_model$predict_correct(data$x1, data$x2)
  pz_misspecified <- instrument_model$predict_misspecified(data$xt1, data$xt2)

  treatment_model <- fit_treatment_model(data, lambda)
  pa_1_correct <- treatment_model$predict_correct(z = 1, data$u)
  pa_1_misspecified <- treatment_model$predict_misspecified(z = 1, data$xt1, data$xt2)
  pa_0_correct <- treatment_model$predict_correct(z = 0, data$u)
  pa_0_misspecified <- treatment_model$predict_misspecified(z = 0, data$xt1, data$xt2)

  outcome_model <- fit_outcome_model(data)
  prob_Y_leq_y_11_correct <- function(y) {
    outcome_model$prob_Y_leq_y_correct(y, a = 1, z = 1, data$x1, data$x2, data$c)
  }
  prob_Y_leq_y_11_misspecified <- function(y) {
    outcome_model$prob_Y_leq_y_misspecified(y, a = 1, z = 1, data$xt1, data$xt2)
  }
  prob_Y_leq_y_01_correct <- function(y) {
    outcome_model$prob_Y_leq_y_correct(y, a = 0, z = 1, data$x1, data$x2, data$c)
  }
  prob_Y_leq_y_01_misspecified <- function(y) {
    outcome_model$prob_Y_leq_y_misspecified(y, a = 0, z = 1, data$xt1, data$xt2)
  }
  prob_Y_leq_y_10_correct <- function(y) {
    outcome_model$prob_Y_leq_y_correct(y, a = 1, z = 0, data$x1, data$x2, data$c)
  }
  prob_Y_leq_y_10_misspecified <- function(y) {
    outcome_model$prob_Y_leq_y_misspecified(y, a = 1, z = 0, data$xt1, data$xt2)
  }
  prob_Y_leq_y_00_correct <- function(y) {
    outcome_model$prob_Y_leq_y_correct(y, a = 0, z = 0, data$x1, data$x2, data$c)
  }
  prob_Y_leq_y_00_misspecified <- function(y) {
    outcome_model$prob_Y_leq_y_misspecified(y, a = 0, z = 0, data$xt1, data$xt2)
  }

  add_confint <- function(model) {
    tmp <- get_confint(model$f0, model$f0_se, level = 0.95)
    model$f0_lower <- tmp$lower
    model$f0_upper <- tmp$upper
    tmp <- get_confint(model$f1, model$f1_se, level = 0.95)
    model$f1_lower <- tmp$lower
    model$f1_upper <- tmp$upper
    model
  }

  model_all_correct <-
    fit_ccdf(data$z, data$a, data$y, pz_correct,
             pa_1_correct, pa_0_correct,
             prob_Y_leq_y_11_correct, prob_Y_leq_y_10_correct,
             prob_Y_leq_y_01_correct, prob_Y_leq_y_00_correct) |>
    add_confint()

  model_instrument_correct <-
    fit_ccdf(data$z, data$a, data$y, pz_correct,
             pa_1_misspecified, pa_0_misspecified,
             prob_Y_leq_y_11_misspecified, prob_Y_leq_y_10_misspecified,
             prob_Y_leq_y_01_misspecified, prob_Y_leq_y_00_misspecified) |>
    add_confint()

  model_instrument_misspecified <-
    fit_ccdf(data$z, data$a, data$y, pz_misspecified,
             pa_1_correct, pa_0_correct,
             prob_Y_leq_y_11_correct, prob_Y_leq_y_10_correct,
             prob_Y_leq_y_01_correct, prob_Y_leq_y_00_correct) |>
    add_confint()

  model_all_misspecified <-
    fit_ccdf(data$z, data$a, data$y, pz_misspecified,
             pa_1_misspecified, pa_0_misspecified,
             prob_Y_leq_y_11_misspecified, prob_Y_leq_y_10_misspecified,
             prob_Y_leq_y_01_misspecified, prob_Y_leq_y_00_misspecified) |>
    add_confint()

  # metrics <- rbind(
  #   get_metrics(f_y0c_true, f_y1c_true, model_all_correct),
  #   get_metrics(f_y0c_true, f_y1c_true, model_instrument_correct),
  #   get_metrics(f_y0c_true, f_y1c_true, model_instrument_misspecified),
  #   get_metrics(f_y0c_true, f_y1c_true, model_all_misspecified)
  # )
  # metrics <- cbind(
  #   data.frame(model = c("All Correct", "Instrument Correct",
  #                        "Instrument Misspecified", "All Misspecified")),
  #   metrics
  # )
  # metrics$n_obs <- n

  list(
    # metrics = metrics,
    data = data,
    model_all_correct = model_all_correct,
    model_instrument_correct = model_instrument_correct,
    model_instrument_misspecified = model_instrument_misspecified,
    model_all_misspecified = model_all_misspecified
  )
}
