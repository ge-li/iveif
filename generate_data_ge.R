generate_data_ge <- function(n, alpha, lambda, beta_1, beta_2) {
  x1 <- rnorm(n)
  x2 <- rnorm(n)

  xt1 <- exp(x1) / 3
  xt2 <- x2 / (1 + exp(x1)) + 6

  pz <- plogis(0.3 * x1 - 0.5 * x2)
  z <- rbinom(n, 1, pz)

  u <- rnorm(n)
  p_complier <- exp(-lambda * u^2)
  p_always_taker <- (1 - p_complier) * (u <= 0)
  p_never_taker <- (1 - p_complier) * (u > 0)

  is_complyer <- as.logical(rbinom(n, 1, p_complier))
  is_always_taker <- (!is_complyer) & (u <= 0)

  c <- rep(2, n)
  c[is_complyer] <- 1
  c[is_always_taker] <- 0

  a <- (c == 0) + z * (c == 1) # actual treatment

  ey0 <- beta_1 * x1 + beta_2 * x2 + c - 1
  y0 <- ey0 + rnorm(n)
  y1 <- y0 + alpha # complier

  y <- a * y1 + (1 - a) * y0

  data <- data.frame(x1, x2, xt1, xt2,
                     pz, z, u, a, c, y0, y1, y)

  data
}
