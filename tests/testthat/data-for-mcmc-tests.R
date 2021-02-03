set.seed(8420)

# Prepare input objects
arr <- array(rnorm(4000), dim = c(100, 4, 10))
arr1chain <- arr[, 1, , drop = FALSE]
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
mat1 <- mat[, 1, drop = FALSE]
dframe1 <- dframe[, 1, drop = FALSE]
chainlist1 <- list(chainlist[[1]][, 1, drop=FALSE],
                  chainlist[[2]][, 1, drop=FALSE])

# data.frame with chain column
dframe_multiple_chains <- dframe
dframe_multiple_chains$chain <- rep(1:4, 25)


# for vdiffr visual tests
set.seed(11172017)
vdiff_dframe <- as.data.frame(matrix(rnorm(1000), nrow = 100, ncol = 5))
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
set.seed(seed = NULL)
