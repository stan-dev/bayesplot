set.seed(8420)
y <- rnorm(100)
yrep <- matrix(rnorm(2500), ncol = 100)
group <- gl(4, 25, labels = LETTERS[1:4])
status_y <- rep_len(0:1, length.out = length(y))
left_truncation_y <- y - 10

y2 <- rpois(30, 1)
yrep2 <- matrix(rpois(30, 1), ncol = 30)
group2 <- rep(1, 30)
status_y2 <- rep_len(0:1, length.out = length(y2))


# for vdiffr visual tests
set.seed(11172017)
vdiff_y <- rnorm(100)
vdiff_yrep <- matrix(rnorm(2500), ncol = 100)
vdiff_group <- gl(4, 25, labels = LETTERS[1:4])
vdiff_status_y <- rep_len(0:1, length.out = length(vdiff_y))

vdiff_y2 <- rpois(30, 1)
vdiff_yrep2 <- matrix(rpois(30 * 10, 1), ncol = 30, nrow = 10)
vdiff_group2 <- rep_len(c(1,2), length.out = 30)
vdiff_status_y2 <- rep_len(0:1, length.out = length(vdiff_y2))

vdiff_loo_y <- rnorm(100, 30, 5)
vdiff_loo_yrep <- matrix(rnorm(100 * 400, 30, 5), nrow = 400)
vdiff_loo_lw <- vdiff_loo_yrep
vdiff_loo_lw[] <- rnorm(100 * 400, -8, 2)


vdiff_y3 <- rexp(50, rate = 0.2)
vdiff_status_y3 <- rep_len(0:1, length.out = length(vdiff_y3))
vdiff_group3 <- rep_len(c(1,2), length.out = 50)
vdiff_left_truncation_y3 <- runif(length(vdiff_y3), min = 0, max = 0.6) * vdiff_y3

simulate_truncated_exp <- function(n, rate, trunc_point) {
  u <- runif(n)
  return(trunc_point - log(u) / rate)
}

rate <- 0.2
vdiff_yrep3 <- matrix(NA, nrow = 10, ncol = 50)
for (i in 1:50) {
  vdiff_yrep3[, i] <- simulate_truncated_exp(10, rate, vdiff_left_truncation_y3[i])
}

set.seed(seed = NULL)
