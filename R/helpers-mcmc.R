# Prepare 3-D array for MCMC plots
#
# @param x User's 'x' input to one of the mcmc_* functions.
# @return A 3-D (Iterations x Chains x Parameters) array.
#
prepare_mcmc_array <- function(x) {
  x <- validate_mcmc_x(x)

  parnames <- if (is.matrix(x)) {
    colnames(x)
  } else if (is.array(x)) {
    dimnames(x)[[3]]
  } else {
    stop("No parameter names found.")
  }

  if (is.matrix(x))
    x <- array(x, dim = c(nrow(x), 1, ncol(x)))

  structure(x,
            dimnames = list(
              Iteration = NULL,
              Chain = seq_len(ncol(x)),
              Parameter = parnames
            ))
}

# Perform some checks on user's 'x' input for MCMC plots
#
# @param x User's 'x' input to one of the mcmc_* functions.
# @return x, unless an error is thrown.
#
validate_mcmc_x <- function(x) {
  if (is.data.frame(x))
    x <- as.matrix(x)

  stopifnot(is.matrix(x) || is.array(x))
  if (anyNA(x))
    stop("NAs not allowed in 'x'.")

  x
}

# Apply transformations to parameter draws
#
# @param x 3-D array of draws
# @param transformation User's 'transformations' argument to one of the mcmc_*
#   functions.
# @return x, with tranformations having been applied to some parameters.
#
transform_draws <- function(x, transformations = list()) {
  x_transforms <- lapply(transformations, match.fun)
  pars <- dimnames(x)[[3]]

  if (!all(names(x_transforms) %in% pars)) {
    not_found <- which(!names(x_transforms) %in% pars)
    stop(
      "Some names(transformations) don't match parameter names: ",
      paste(names(t_x)[not_found], collapse = ", ")
    )
  }

  for (p in names(x_transforms))
      x[, , p] <- x_transforms[[p]](x[, , p])

  x
}
