# Prepare acceptable input objects

arr <- array(rnorm(4000), dim = c(100, 4, 10))
arr1chain <- arr[, 1, , drop = FALSE]
mat <- matrix(rnorm(1000), nrow = 100, ncol = 10)
dframe <- as.data.frame(mat)

mixed_names <- c("(Intercept)", "beta[1]", "beta[2]", "sigma",
                 paste0("b[(Intercept) x:", 1:6, "]"))
colnames(mat) <- colnames(dframe) <- mixed_names
dimnames(arr) <- dimnames(arr1chain) <- list(NULL, NULL, mixed_names)

# one parameter
arr1 <- arr[, , 1, drop = FALSE]
mat1 <- mat[, 1, drop = FALSE]
dframe1 <- dframe[, 1, drop = FALSE]

# data.frame with chain column
dframe_multiple_chains <- dframe
dframe_multiple_chains$chain <- rep(1:4, 25)

