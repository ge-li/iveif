get_cMWTE_SE <- function(data, lambda) {

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

  cMWTE_all_correct <-
    fit_cMWTE(data$z, data$a, data$y, pz_correct,
              pa_1_correct, pa_0_correct,
              prob_Y_leq_y_11_correct, prob_Y_leq_y_10_correct,
              prob_Y_leq_y_01_correct, prob_Y_leq_y_00_correct)

  cMWTE_instrument_correct <-
    fit_cMWTE(data$z, data$a, data$y, pz_correct,
              pa_1_misspecified, pa_0_misspecified,
              prob_Y_leq_y_11_misspecified, prob_Y_leq_y_10_misspecified,
              prob_Y_leq_y_01_misspecified, prob_Y_leq_y_00_misspecified)

  cMWTE_instrument_misspecified <-
    fit_cMWTE(data$z, data$a, data$y, pz_misspecified,
             pa_1_correct, pa_0_correct,
             prob_Y_leq_y_11_correct, prob_Y_leq_y_10_correct,
             prob_Y_leq_y_01_correct, prob_Y_leq_y_00_correct)

  cMWTE_all_misspecified <-
    fit_cMWTE(data$z, data$a, data$y, pz_misspecified,
             pa_1_misspecified, pa_0_misspecified,
             prob_Y_leq_y_11_misspecified, prob_Y_leq_y_10_misspecified,
             prob_Y_leq_y_01_misspecified, prob_Y_leq_y_00_misspecified)

  cMWTE <- rbind(
    cMWTE_all_correct,
    cMWTE_instrument_correct,
    cMWTE_instrument_misspecified,
    cMWTE_all_misspecified
  ) |> as.data.frame()

  row.names(cMWTE) <- NULL
  cMWTE$model <- c("EIF: All Correct",
                   "EIF: Instrument Correct",
                   "EIF: Instrument Misspecified",
                   "EIF: All Misspecified")

  cMWTE

}
