set.seed(8420)

# Prepare input objects
arr <- array(rnorm(4000), dim = c(100, 4, 10))
arr1chain <- arr[, 1, , drop = FALSE]
drawsarr <- posterior::example_draws()
drawsarr1chain <- drawsarr[, 1, , drop = FALSE]
mat <- matrix(rnorm(1000), nrow = 100, ncol = 10)
dframe <- as.data.frame(mat)
chainlist <- list(matrix(rnorm(1000), nrow = 100, ncol = 10),
                  matrix(rnorm(1000), nrow = 100, ncol = 10))
mixed_names <- c("(Intercept)", "beta[1]", "beta[2]", "sigma",
                 paste0("b[(Intercept) x:", 1:6, "]"))
colnames(mat) <- colnames(dframe) <-
  colnames(chainlist[[1]]) <- colnames(chainlist[[2]]) <- mixed_names
dimnames(arr) <- dimnames(arr1chain) <- list(NULL, NULL, mixed_names)
chainlist1chain <- chainlist[1]

# one parameter
arr1 <- arr[, , 1, drop = FALSE]
drawsarr1 <- drawsarr[, , 1, drop = FALSE]
mat1 <- mat[, 1, drop = FALSE]
dframe1 <- dframe[, 1, drop = FALSE]
chainlist1 <- list(chainlist[[1]][, 1, drop=FALSE],
                  chainlist[[2]][, 1, drop=FALSE])

# data.frame with chain column
dframe_multiple_chains <- dframe
dframe_multiple_chains$chain <- rep(1:4, 25)


# for vdiffr visual tests
set.seed(11172017)
vdiff_dframe <- as.data.frame(matrix(rnorm(500), nrow = 100, ncol = 5))
vdiff_dframe_chains <- as.data.frame(
  matrix(rnorm(4000), nrow = 2000, ncol = 2)
)
vdiff_dframe_chains$chain <- rep(1:4, each = 500)

vdiff_dframe_chains_divergences <- data.frame(
  Iteration = rep(1:500, each = 4),
  Parameter = "divergent__",
  Value = rbinom(2000, size = 1, prob = .02),
  Chain = vdiff_dframe_chains$chain,
  stringsAsFactors = FALSE
)

vdiff_dframe_chains_treedepth <- vdiff_dframe_chains_divergences
vdiff_dframe_chains_treedepth$Parameter <- "treedepth__"
vdiff_dframe_chains_treedepth$Value <- sample(1:10, size = 2000, replace = TRUE)

vdiff_dframe_chains_acceptance <- vdiff_dframe_chains_divergences
vdiff_dframe_chains_acceptance$Parameter <- "accept_stat__"
vdiff_dframe_chains_acceptance$Value <- runif(2000, 0.7, 1)

vdiff_dframe_chains_stepsize <- vdiff_dframe_chains_divergences
vdiff_dframe_chains_stepsize$Parameter <- "stepsize__"
vdiff_dframe_chains_stepsize$Value <- rep(c(0.37, 0.42, 0.33, 0.47), each = 500)

vdiff_dframe_chains_energy <- vdiff_dframe_chains_divergences
vdiff_dframe_chains_energy$Parameter <- "energy__"
vdiff_dframe_chains_energy$Value <- rexp(2000, rate = 0.1)

vdiff_dframe_chains_np <- rbind(
  vdiff_dframe_chains_divergences,
  vdiff_dframe_chains_treedepth,
  vdiff_dframe_chains_energy,
  vdiff_dframe_chains_acceptance,
  vdiff_dframe_chains_stepsize
)

vdiff_dframe_chains_lp <- vdiff_dframe_chains_divergences
vdiff_dframe_chains_lp$Parameter <- NULL
vdiff_dframe_chains_lp$Value <- runif(2000, -100, -50)

vdiff_dframe_rank_overlay_bins_test <- posterior::as_draws_df(
  list(
    list(theta = -2 + 0.003 * 1:1000 + stats::arima.sim(list(ar = 0.7), n = 1000, sd = 0.5)),
    list(theta = 1 + -0.01 * 1:1000 + stats::arima.sim(list(ar = 0.7), n = 1000, sd = 0.5))
  )
)

set.seed(seed = NULL)
