for (f in list.files(pattern = "*.R$")) {
  source(f)
}

library(pbapply)
# Test the function one_simulation
alpha = 1
beta_1 = 0.4
beta_2 = -0.8
sigma <- sqrt(beta_1^2 + beta_2^2 + 1)
f_y1c_true <- function(y) {
  pnorm(y, mean = alpha, sd = sigma)
}
f_y0c_true <- function(y) {
  pnorm(y, mean = 0, sd = sigma)
}

set.seed(100)
res_100_lambda.5 <- pbreplicate(2000, expr = {
  one_sim <- try(one_simulation(100, alpha, lambda = 0.5, beta_1, beta_2), silent = TRUE)
  one_sim
}, cl = 8, simplify = FALSE)
saveRDS(res_100_lambda.5, "res_100_lambda.5.rds")

set.seed(400)
res_400_lambda.5 <- pbreplicate(2000, expr = {
  one_sim <- try(one_simulation(400, alpha, lambda = 0.5, beta_1, beta_2), silent = TRUE)
  one_sim
}, cl = 8, simplify = FALSE)
saveRDS(res_400_lambda.5, "res_400_lambda.5.rds")

set.seed(1600)
res_1600_lambda.5 <- pbreplicate(2000, expr = {
  one_sim <- try(one_simulation(1600, alpha, lambda = 0.5, beta_1, beta_2), silent = TRUE)
  one_sim
}, cl = 8, simplify = FALSE)
saveRDS(res_1600_lambda.5, "res_1600_lambda.5.rds")

set.seed(100)
res_100_lambda2 <- pbreplicate(2000, expr = {
  one_sim <- try(one_simulation(100, alpha, lambda = 2, beta_1, beta_2), silent = TRUE)
  one_sim
}, cl = 8, simplify = FALSE)
saveRDS(res_100_lambda2, "res_100_lambda2.rds")

set.seed(400)
res_400_lambda2 <- pbreplicate(2000, expr = {
  one_sim <- try(one_simulation(400, alpha, lambda = 2, beta_1, beta_2), silent = TRUE)
  one_sim
}, cl = 8, simplify = FALSE)
saveRDS(res_400_lambda2, "res_400_lambda2.rds")

set.seed(1600)
res_1600_lambda2 <- pbreplicate(2000, expr = {
  one_sim <- try(one_simulation(1600, alpha, lambda = 2, beta_1, beta_2), silent = TRUE)
  one_sim
}, cl = 8, simplify = FALSE)
saveRDS(res_1600_lambda2, "res_1600_lambda2.rds")
