fit_ccdf <- function(Z, A, Y, pz, pa_1, pa_0,
                     prob_Y_leq_y_11, prob_Y_leq_y_10,
                     prob_Y_leq_y_01, prob_Y_leq_y_00) {
  n <- length(Z)

  W <- (Z - pz) / (1 - pz)

  eif_numerator_1 <- function(y) {
    W * (((Y <= y) * A - pa_1 * prob_Y_leq_y_11(y)) / pz +
           pa_1 * prob_Y_leq_y_11(y) - pa_0 * prob_Y_leq_y_10(y)
    )
  }

  eif_numerator_0 <- function(y) {
    W * (((Y <= y) * (1 - A) - (1 - pa_1) * prob_Y_leq_y_01(y)) / pz +
           (1 - pa_1) * prob_Y_leq_y_01(y) - (1 - pa_0) * prob_Y_leq_y_00(y)
    )
  }

  eif_denominator_1 <- W * ((A - pa_1) / pz + pa_1 - pa_0)

  eif_denominator_0 <- -eif_denominator_1

  p_complier <- mean(eif_denominator_1)

  get_ccdf_1 <- function(y) {
    nums <- eif_numerator_1(y)
    est <- mean(nums) / p_complier
    se <- sqrt(mean(((nums - est * eif_denominator_1) / p_complier)^2) / n)
    c(est, se)
  }

  get_ccdf_0 <- function(y) {
    nums <- eif_numerator_0(y)
    est <- mean(nums) / (-p_complier)
    se <- sqrt(mean(((nums - est * eif_denominator_0) / p_complier)^2) / n)
    c(est, se)
  }

  sorted_y <- sort(Y)
  ccdf_1 <- sapply(sorted_y, get_ccdf_1)
  ccdf_0 <- sapply(sorted_y, get_ccdf_0)

  monotonize_cdf <- function(f_val) {
    f_val[1] <- max(0, f_val[1])
    f_val <- cummax(f_val)
    f_val / f_val[length(f_val)]
  }

  data.frame(
    sorted_y = sorted_y,
    f0 = monotonize_cdf(ccdf_0[1, ]),
    f0_se = ccdf_0[2, ],
    f1 = monotonize_cdf(ccdf_1[1, ]),
    f1_se = ccdf_1[2, ]
  )
}
