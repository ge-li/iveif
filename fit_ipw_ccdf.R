fit_ipw_ccdf <- function(data) {
  n <- nrow(data)

  instrument_model <- fit_instrument_model(data)
  pz_correct <- instrument_model$predict_correct(data$x1, data$x2)
  pz_misspecified <- instrument_model$predict_misspecified(data$xt1, data$xt2)

  get_ipw_ccdf <- function(y, pz, type = c("treated", "control")) {
    w <- data$z / pz - (1 - data$z) / (1 - pz)
    if (type == "treated") {
      nums <- w * data$a * (data$y <= y)
      dens <- w * data$a
    } else if (type == "control") {
      nums <- w * (1 - data$a) * (data$y <= y)
      dens <- w * (1 - data$a)
    }
    est <- mean(nums) / mean(dens)
    est
  }

  monotonize_cdf <- function(f_val) {
    f_val[1] <- max(0, f_val[1])
    f_val <- cummax(f_val)
    f_val / f_val[length(f_val)]
  }

  sorted_y <- sort(data$y)
  f0_correct <- sapply(sorted_y, get_ipw_ccdf, pz = pz_correct, type = "control") |> monotonize_cdf()
  f0_misspecified <- sapply(sorted_y, get_ipw_ccdf, pz = pz_misspecified, type = "control") |> monotonize_cdf()
  f1_correct <- sapply(sorted_y, get_ipw_ccdf, pz = pz_correct, type = "treated") |> monotonize_cdf()
  f1_misspecified <- sapply(sorted_y, get_ipw_ccdf, pz = pz_misspecified, type = "treated") |> monotonize_cdf()

  get_ipw_ccdf_se <- function(y, f, pz, type = c("treated", "control")) {
    w <- data$z / pz - (1 - data$z) / (1 - pz)
    if (type == "treated") {
      nums <- w * data$a * (data$y <= y)
      dens <- w * data$a
    } else if (type == "control") {
      nums <- w * (1 - data$a) * (data$y <= y)
      dens <- w * (1 - data$a)
    }
    se <- sqrt(mean(((nums - f * dens) / mean(dens))^2) / n)
    se
  }

  f0_se_correct <- purrr::map2_dbl(sorted_y,
                               f0_correct,
                               \(y, f) get_ipw_ccdf_se (y, f, pz = pz_correct, type = "control"))
  f0_se_misspecified <- purrr::map2_dbl(
    sorted_y,
    f0_misspecified,
    \(y, f) get_ipw_ccdf_se (y, f, pz = pz_misspecified, type = "control")
  )
  f1_se_correct <- purrr::map2_dbl(sorted_y,
                               f1_correct,
                               \(y, f) get_ipw_ccdf_se (y, f, pz = pz_correct, type = "treated"))
  f1_se_misspecified <- purrr::map2_dbl(
    sorted_y,
    f1_misspecified,
    \(y, f) get_ipw_ccdf_se (y, f, pz = pz_misspecified, type = "treated")
  )


  model_ipw_correct <- data.frame(
    sorted_y = sorted_y,
    f0 = f0_correct,
    f0_se = f0_se_correct,
    f1 = f1_correct,
    f1_se = f1_se_correct
  )

  model_ipw_misspecified <- data.frame(
    sorted_y = sorted_y,
    f0 = f0_misspecified,
    f0_se = f0_se_misspecified,
    f1 = f1_misspecified,
    f1_se = f1_se_misspecified
  )

  list(
    model_ipw_correct = model_ipw_correct,
    model_ipw_misspecified = model_ipw_misspecified
  )
}


add_ipw_fit_to_res <- function(res) {
  fitted <- fit_ipw_ccdf(res$data)
  res$model_ipw_correct <- fitted$model_ipw_correct |> add_confint()
  res$model_ipw_misspecified <- fitted$model_ipw_misspecified |> add_confint()
  res
}

newres_100_lambda.5 <- pbapply::pblapply(
  res_100_lambda.5,
  add_ipw_fit_to_res,
  cl = 32
)

newres_400_lambda.5 <- pbapply::pblapply(
  res_400_lambda.5,
  add_ipw_fit_to_res,
  cl = 32
)

newres_1600_lambda.5 <- pbapply::pblapply(
  res_1600_lambda.5,
  add_ipw_fit_to_res,
  cl = 32
)

newres_100_lambda2 <- pbapply::pblapply(
  res_100_lambda2,
  add_ipw_fit_to_res,
  cl = 32
)

newres_400_lambda2 <- pbapply::pblapply(
  res_400_lambda2,
  add_ipw_fit_to_res,
  cl = 32
)

newres_1600_lambda2 <- pbapply::pblapply(
  res_1600_lambda2,
  add_ipw_fit_to_res,
  cl = 32
)

library(purrr)
library(furrr)

plan(multisession, workers = 32)

newres_100_lambda.5 <- future_map(
  res_100_lambda.5,
  add_ipw_fit_to_res,
  .progress = TRUE
)

newres_400_lambda.5 <- future_map(
  res_400_lambda.5,
  add_ipw_fit_to_res,
  .progress = TRUE
)

newres_1600_lambda.5 <- future_map(
  res_1600_lambda.5,
  add_ipw_fit_to_res,
  .progress = TRUE
)

newres_100_lambda2 <- future_map(
  res_100_lambda2,
  add_ipw_fit_to_res,
  .progress = TRUE
)

newres_400_lambda2 <- future_map(
  res_400_lambda2,
  add_ipw_fit_to_res,
  .progress = TRUE
)

newres_1600_lambda2 <- future_map(
  res_1600_lambda2,
  add_ipw_fit_to_res,
  .progress = TRUE
)

saveRDS(newres_100_lambda.5, "newres_100_lambda.5.rds")
saveRDS(newres_400_lambda.5, "newres_400_lambda.5.rds")
saveRDS(newres_1600_lambda.5, "newres_1600_lambda.5.rds")
saveRDS(newres_100_lambda2, "newres_100_lambda2.rds")
saveRDS(newres_400_lambda2, "newres_400_lambda2.rds")
saveRDS(newres_1600_lambda2, "newres_1600_lambda2.rds")

