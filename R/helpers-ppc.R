# input validation and type checking ----------------------------------------

# Check if an object is a vector (but not list) or a 1-D array
is_vector_or_1Darray <- function(x) {
  if (is.vector(x) && !is.list(x)) {
    return(TRUE)
  }

  isTRUE(is.array(x) && length(dim(x)) == 1)
}

# Check if x consists of whole numbers (very close to integers)
# Implementation here follows example ?integer
is_whole_number <- function(x, tol = .Machine$double.eps) {
  if (!is.numeric(x)) {
    FALSE
  } else {
    abs(x - round(x)) < tol
  }
}

# Check if all values in x are whole numbers or counts (non-negative whole
# numbers)
all_whole_number <- function(x, ...) {
  all(is_whole_number(x, ...))
}
all_counts <- function(x, ...) {
  all_whole_number(x, ...) && min(x) >= 0
}


#' Validate y
#'
#' Checks that `y` is numeric, doesn't have any NAs, and is either a vector, 1-D
#' array, or univariate time series object of class `ts`.
#'
#' @param y The `y` object from the user.
#' @return Either throws an error or returns a numeric vector.
#' @noRd
validate_y <- function(y) {
  stopifnot(is.numeric(y))

  if (!(inherits(y, "ts") && is.null(dim(y)))) {
    if (!is_vector_or_1Darray(y)) {
      abort("'y' must be a vector or 1D array.")
    }
    y <- as.vector(y)
  }

  if (anyNA(y)) {
    abort("NAs not allowed in 'y'.")
  }

  unname(y)
}


#' Validate predictions (`yrep` or `ypred`)
#'
#' Checks that `predictions` is a numeric matrix, doesn't have any NAs, and has
#' the correct number of columns.
#'
#' @param predictions The user's `yrep` or `ypred` object (SxN matrix).
#' @param `n_obs` The number of observations (columns) that `predictions` should
#'   have, if applicable.
#' @return Either throws an error or returns a numeric matrix.
#' @noRd
validate_predictions <- function(predictions, n_obs = NULL) {
  # sanity checks
  stopifnot(is.matrix(predictions), is.numeric(predictions))
  if (!is.null(n_obs)) {
    stopifnot(length(n_obs) == 1, n_obs == as.integer(n_obs))
  }

  if (is.integer(predictions)) {
    if (nrow(predictions) == 1) {
      predictions[1, ] <- as.numeric(predictions[1,, drop = FALSE])
    }
    else {
      predictions <- apply(predictions, 2, as.numeric)
    }
  }

  if (anyNA(predictions)) {
    abort("NAs not allowed in predictions.")
  }

  if (!is.null(n_obs) && (ncol(predictions) != n_obs)) {
    abort("ncol(yrep) must be equal to length(y).")
  }

  # get rid of names but keep them as an attribute in case we want them
  obs_names <- colnames(predictions)
  predictions <- unclass(unname(predictions))
  attr(predictions, "obs_names") <- obs_names

  predictions
}


#' Validate PIT
#'
#' Checks that `pit` is numeric, doesn't have any NAs, and is either a vector,
#' or a 1-D array with values in `[0,1]`.
#'
#' @param pit The 'pit' object from the user.
#' @return Either throws an error or returns a numeric vector.
#' @noRd
validate_pit <- function(pit) {
  if (anyNA(pit)) {
    abort("NAs not allowed in 'pit'.")
  }

  stopifnot(is.numeric(pit))

  if (!is_vector_or_1Darray(pit)) {
    abort("'pit' must be a vector or 1D array.")
  }

  if (any(pit > 1) || any(pit < 0)) {
    abort("'pit' must only contain values between 0 and 1.")
  }

  unname(pit)
}

#' Validate group
#'
#' Checks that grouping variable has correct number of observations and is
#' either a factor variable or vector (which is coerced to factor).
#'
#' @param group The user's `group` argument.
#' @param n_obs The number of observations that `group` should contain (e.g.,
#'   `length(y)`, `ncol(yrepd)`, etc.). Unlike for `validate_predictions()`,
#'   this is always required for `validate_group()`.
#' @return Either throws an error or returns `group` (coerced to a factor).
#' @noRd
validate_group <- function(group, n_obs) {
  # sanity checks
  stopifnot(is.vector(group) || is.factor(group),
            length(n_obs) == 1, n_obs == as.integer(n_obs))

  if (!is.factor(group)) {
    group <- as.factor(group)
  }

  if (anyNA(group)) {
    abort("NAs not allowed in 'group'.")
  }

  if (length(group) != n_obs) {
    abort("length(group) must be equal to the number of observations.")
  }

  unname(group)
}


#' Validate x
#'
#' Checks that x is a numeric vector, doesn't have any NAs, and has the
#' same length as y.
#'
#' @param x,y The user's `x` vector and the `y` object returned by `validate_y()`.
#' @param unique_x `TRUE` or `FALSE` indicating whether to require all unique
#'   values in `x`.
#' @return Either throws an error or returns a numeric vector.
#' @noRd
validate_x <- function(x = NULL, y, unique_x = FALSE) {
  if (is.null(x)) {
    if (inherits(y, "ts") && is.null(dim(y))) {
      x <- stats::time(y)
    } else {
      x <- seq_along(y)
    }
  }

  stopifnot(is.numeric(x))

  if (!is_vector_or_1Darray(x)) {
    abort("'x' must be a vector or 1D array.")
  }

  x <- as.vector(x)
  if (length(x) != length(y)) {
    abort("length(x) must be equal to length(y).")
  }

  if (anyNA(x)) {
    abort("NAs not allowed in 'x'.")
  }

  if (unique_x) {
    stopifnot(identical(length(x), length(unique(x))))
  }

  unname(x)
}


# Internals for grouped plots ---------------------------------------------

#' Modify a call to a `_grouped` function to a call to the ungrouped version
#' @param fn The new function to call (a string).
#' @param call The original call (from `match.call(expand.dots = FALSE)`).
#' @return The new unevaluated call, with additional argument
#'   `called_from_internal=TRUE` which can be detected by the function to be
#'   called so it knows not to warn about the `group` and `facet_args` arguments.
#' @noRd
ungroup_call <- function(fn, call) {
  args <- rlang::call_args(call)
  args$called_from_internal <- TRUE
  args[["..."]] <- NULL
  rlang::call2(.fn = fn, !!!args, .ns = "bayesplot")
}

#' Check if the `...` to a plotting function was passed from it's `_grouped` version
#' @param dots The `...` arguments already in a list, i.e., `list(...)`.
#' @return `TRUE` or `FALSE`
#' @noRd
from_grouped <- function(dots) {
  isTRUE(dots[["called_from_internal"]]) && !is.null(dots[["group"]])
}



# reshaping ---------------------------------------------------

#' Convert matrix of predictions into a molten data frame
#'
#' @param predictions A matrix (`yrep` or `ypred`), already validated using
#'   `validate_predictions()`.
#' @return A data frame with columns:
#'   * `y_id`: integer indicating the observation number (`predictions` column).
#'   * `rep_id`: integer indicating the simulation number (`predictions` row).
#'   * `rep_label`: factor with S levels, where S is `nrow(predictions)`, i.e.
#'     the number of simulations included in `predictions`.
#'   * `value`: the simulation values.
#' @noRd
melt_predictions <- function(predictions) {
  obs_names <- attr(predictions, "obs_names")
  out <- predictions %>%
    reshape2::melt(varnames = c("rep_id", "y_id")) %>%
    tibble::as_tibble()

  rep_labels <- create_rep_ids(out$rep_id)
  y_names <- obs_names[out$y_id] %||% out$y_id
  out$rep_label <- factor(rep_labels, levels = unique(rep_labels))
  out$y_name <- factor(y_names, levels = unique(y_names))
  out[c("y_id", "y_name", "rep_id", "rep_label", "value")]
}


#' Stack `y` below melted `yrep` data
#'
#' @param y Validated `y` input.
#' @param yrep Validated `yrep` input.
#' @return A data frame with the all the columns as the one returned by
#'   `melt_predictions()`, plus additional columns:
#'   * `is_y`: logical indicating whether the values are observations (`TRUE`)
#'      or simulations (`FALSE`).
#'   * `is_y_label`: factor with levels `italic(y)` for observations and
#'      `italic(y)[rep]` for simulations.
#' @noRd
melt_and_stack <- function(y, yrep) {
  y_text <- as.character(y_label())
  yrep_text <- as.character(yrep_label())

  molten_preds <- melt_predictions(yrep)

  # Add a level in the labels for the observed y values
  levels(molten_preds$rep_label) <- c(levels(molten_preds$rep_label), y_text)

  y_names <-  attr(yrep, "obs_names") %||% seq_along(y)

  ydat <- tibble::tibble(
    rep_label = factor(y_text, levels = levels(molten_preds$rep_label)),
    rep_id = NA_integer_,
    y_id = seq_along(y),
    y_name = factor(y_names, levels = unique(y_names)),
    value = y)

  data <- dplyr::bind_rows(molten_preds, ydat) %>%
    mutate(
      rep_label = relevel(.data$rep_label, y_text),
      is_y = is.na(.data$rep_id),
      is_y_label = ifelse(.data$is_y, y_text, yrep_text) %>%
        factor(levels = c(y_text, yrep_text)))

  cols <- c("y_id", "y_name", "rep_id", "rep_label",
            "is_y", "is_y_label", "value")
  data[cols]
}


#' Obtain the coverage parameter for simultaneous confidence bands for ECDFs
#'
#' @param N Length of sample.
#' @param L Number of chains. Used for MCMC, defaults to 1 for ppc.
#' @param K Number of equally spaced evaluation points (1:K / K). Defaults to N.
#' @param prob Desired simultaneous coverage (0,1).
#' @param M number of simulations to run, if simulation method is used.
#' @param interpolate_adj Boolean defining whether to interpolate the confidence
#' bands from precomputed values. Interpolation provides a faster plot with the
#' trade-off of possible loss of accuracy.
#' @return The adjusted coverage parameter yielding the desired simultaneous
#'  coverage of the ECDF traces.
#' @noRd
adjust_gamma <- function(N,
                         L = 1,
                         K = N,
                         prob = 0.99,
                         M = 1000,
                         interpolate_adj = FALSE) {
  if (! all_counts(c(K, N, L))) {
    abort("Parameters 'N', 'L' and 'K' must be positive integers.")
  }
  if (prob >= 1 || prob <= 0) {
    abort("Value of 'prob' must be in (0,1).")
  }
  if (is.null(interpolate_adj)) {
    if (K <= 200 || N < 100) {
      interpolate_adj <- FALSE
    } else {
      interpolate_adj <- TRUE
    }
  }
  if (interpolate_adj == TRUE) {
    gamma <- interpolate_gamma(N = N, K = K, prob = prob, L = L)
  } else if (L == 1) {
    gamma <- adjust_gamma_optimize(N = N, K = K, prob = prob)
  } else {
    gamma <- adjust_gamma_simulate(N = N, L = L, K = K, prob = prob, M = M)
  }
  gamma
}

#' Adjust coverage parameter for single sample using the optimization method.
#' @param N Length of sample.
#' @param K Number of equally spaced evaluation points (1:K / K). Defaults to N.
#' @param prob Desired simultaneous coverage (0,1).
#' @return The adjusted coverage parameter yielding the desired simultaneous
#'  coverage of the ECDF traces.
#' @noRd
adjust_gamma_optimize <- function(N, K, prob) {
  target <- function(gamma, prob, N, K) {
    z <- 1:(K - 1) / K
    z1 <- c(0, z)
    z2 <- c(z, 1)

    # pre-compute quantiles and use symmetry for increased efficiency.
    x2_lower <- qbinom(gamma / 2, N, z2)
    x2_upper <- c(N - rev(x2_lower)[2:K], 1)

    # Compute the total probability of trajectories inside the confidence
    # intervals. Initialize the set and corresponding probabilities known
    # to be 0 and 1 for the starting value z1 = 0.
    x1 <- 0
    p_int <- 1
    for (i in seq_along(z1)) {
      p_int <- p_interior(
        p_int = p_int,
        x1 = x1,
        x2 = x2_lower[i]: x2_upper[i],
        z1 = z1[i],
        z2 = z2[i],
        N = N
      )
      x1 <- x2_lower[i]:x2_upper[i]
    }
    return(abs(prob - sum(p_int)))
  }
  optimize(target, c(0, 1 - prob), prob = prob, N = N, K = K)$minimum
}

#' Adjust coverage parameter for multiple chains using the simulation method.
#' In short, 'M' simulations of 'L' standard uniform chains are run and the
#' confidence bands are set to cover 100 * 'prob' % of these simulations.
#' @param N Length of sample.
#' @param L Number of chains. Used for MCMC, defaults to 1 for ppc.
#' @param K Number of equally spaced evaluation points (1:K / K). Defaults to N.
#' @param prob Desired simultaneous coverage (0,1).
#' @param M number of simulations to run.
#' @return The adjusted coverage parameter yielding the desired simultaneous
#'  coverage of the ECDF traces.
#' @noRd
adjust_gamma_simulate <- function(N, L, K, prob, M) {
  gamma <- numeric(M)
  z <- (1:(K - 1)) / K # Rank ECDF evaluation points.
  n <- N * (L - 1)
  k <- floor(z * N * L)
  for (m in seq_len(M)) {
    u <- u_scale(replicate(L, runif(N))) # Fractional ranks of sample chains
    scaled_ecdfs <- apply(outer(u, z, "<="), c(2, 3), sum)
    # Find the smalles marginal probability of the simulation run
    gamma[m] <- 2 * min(
      apply(
        scaled_ecdfs, 1, phyper, m = N, n = n, k = k
      ),
      apply(
        scaled_ecdfs - 1, 1, phyper, m = N, n = n, k = k, lower.tail = FALSE
      )
    )
  }
  alpha_quantile(gamma, 1 - prob)
}

#' Approximate the required adjustement to obtain simultaneous confidence bands
#' of an ECDF plot with interpolation with regards to N and K from precomputed
#' values for a fixed set of prob and L values.
#' @param N Length of sample.
#' @param L Number of chains. Used for MCMC, defaults to 1 for ppc.
#' @param prob Desired simultaneous coverage (0,1).
#' @param K Number of equally spaced evaluation points (1:K / K). Defaults to N.
#' @return The approximated adjusted coverage parameter yielding the desired
#' simultaneous coverage of the ECDF traces.
#' @noRd
interpolate_gamma <- function(N, K, prob, L) {
  # Find the precomputed values useful for the interpolation task.
  vals <- get_interpolation_values(N, K, L, prob)
  # Largest lower bound and smalles upper bound for N among precomputed values.
  N_lb <- max(vals[vals$N <= N, ]$N)
  N_ub <- min(vals[vals$N >= N, ]$N)
  # Approximate largest lower bound and smallest upper bound for gamma.
  log_gamma_lb <- approx(
    x = log(vals[vals$N == N_lb, ]$K),
    y = log(vals[vals$N == N_lb, ]$val),
    xout = log(K)
  )$y
  log_gamma_ub <- approx(
    x = log(vals[vals$N == N_ub, ]$K),
    y = log(vals[vals$N == N_ub, ]$val),
    xout = log(K)
  )$y
  if (N_ub == N_lb) {
    log_gamma_approx <- log_gamma_lb
  } else {
    # Approximate log_gamma for the desired value of N.
    log_gamma_approx <- approx(
      x = log(c(N_lb, N_ub)),
      y = c(log_gamma_lb, log_gamma_ub),
      xout = log(N)
    )$y
  }
  exp(log_gamma_approx)
}

#' Filter the precomputed values useful for the interpolation task given to
#' interpolate_gamma. Check, if the task is possible with the availabel data.
#' @param N Length of sample.
#' @param K Number of equally spaced evaluation points (1:K / K). Defaults to N.
#' @param L Number of chains. Used for MCMC, defaults to 1 for ppc.
#' @param prob Desired simultaneous coverage (0,1).
#' @return A data.frame containing the relevant precomputed values.
#' @noRd
get_interpolation_values <- function(N, K, L, prob) {
  for (dim in c("L", "prob")) {
    if (all(get(dim) != .gamma_adj[, dim])) {
      stop(paste(
        "No precomputed values to interpolate from for '", dim, "' = ",
        get(dim),
        ".\n",
        "Values of '", dim, "' available for interpolation: ",
        paste(unique(.gamma_adj[, dim]), collapse = ", "),
        ".",
        sep = ""
      ))
    }
  }
  vals <- .gamma_adj[.gamma_adj$L == L & .gamma_adj$prob == prob, ]
  if (N > max(vals$N)) {
    stop(paste(
      "No precomputed values to interpolate from for sample length of ",
      N,
      ".\n",
      "Please use a subsample of length ",
      max(vals$N),
      " or smaller, or consider setting 'interpolate_adj' = FALSE.",
      sep = ""
    ))
  }
  if (N < min(vals$N)) {
    stop(paste(
      "No precomputed values to interpolate from for sample length of ",
      N,
      ".\n",
      "Please use a subsample of length ",
      min(vals$N),
      " or larger, or consider setting 'interpolate_adj' = FALSE.",
      sep = ""
    ))
  }
  if (K > max(vals[vals$N <= N, ]$K)) {
    stop(paste(
      "No precomputed values available for interpolation for 'K' = ",
      K,
      ".\n",
      "Try either setting a value of 'K' <= ",
      max(vals[vals$N <= N, ]$K),
      "or 'interpolate_adj' = FALSE.",
      sep = ""
    ))
  }
  if (K < min(vals[vals$N <= N, ]$K)) {
    stop(paste(
      "No precomputed values available for interpolation for 'K' = ",
      K,
      ".\n",
      "Try either setting a value of 'K' >= ",
      min(vals[vals$N <= N, ]$K),
      " or 'interpolate_adj' = FALSE.",
      sep = ""
    ))
  }
  vals
}

#' A helper function for 'adjust_gamma_optimize' defining the probability that
#' a scaled ECDF stays within the supplied bounds between two evaluation points.
#' @param p_int For each value in x1, the probability that the ECDF has stayed
#' within the bounds until z1 and takes the value in x1 at z1.
#' @param x1 Vector of scaled ECDF values at the left end of the interval, z1.
#' @param x2 Vector of scaled ECDF values at the right end of the interval, z2.
#' @param z1 Left evaluation point in `[0,1]`
#' @param z2 Right evaluation point in `[0,1]` with z2 > z1.
#' @param N Total number of values in the sample.
#' @return A vector containing the probability to transitioning from the values
#' in x1 to each of the values in x2 weighted by the probabilities in p_int.
#' @noRd
p_interior <- function(p_int, x1, x2, z1, z2, N) {
  # Ratio between the length of the evaluation interval and the total length of
  # the interval left to cover by ECDF.
  z_tilde <- (z2 - z1) / (1 - z1)
  # Number of samples left to cover by ECDF.
  N_tilde <- rep(N - x1, each = length(x2))

  p_int <- rep(p_int, each = length(x2))
  x_diff <- outer(x2, x1, "-")
  # Pobability of each transition from a value in x1 to a value in x2.
  p_x2_int <- p_int * dbinom(x_diff, N_tilde, z_tilde)
  rowSums(p_x2_int)
}

#' A helper function for 'adjust_alpha_simulate'
#' 100 * `alpha` percent of the trials in 'gamma' are allowed to be rejected.
#' In case of ties, return the largest value dominating at most
#' 100 * (alpha + tol) percent of the values.
#' @noRd
alpha_quantile <- function(gamma, alpha, tol = 0.001) {
  a <- unname(quantile(gamma, probs = alpha))
  a_tol <- unname(quantile(gamma, probs = alpha + tol))
  if (a == a_tol) {
    if (min(gamma) < a) {
      # take the largest value that doesn't exceed the tolerance.
      a <- max(gamma[gamma < a])
    }
  }
  a
}

#' Compute simultaneous confidence intervals with the given adjusted coverage
#'  parameter gamma.
#' @param gamma Adjusted coverage parameter for the marginal distribution
#'  (binomial for PIT values and hypergeometric for rank transformed chains).
#' @param N Sample length.
#' @param K Number of uniformly spaced evaluation points.
#' @param L Number of MCMC-chains. (1 for ppc)
#' @return A list with upper and lower confidence interval values at the
#' evaluation points.
#' @noRd
ecdf_intervals <- function(gamma, N, K, L = 1) {
  lims <- list()
  z <- seq(0, 1, length.out = K + 1)
  if (L == 1) {
    lims$lower <- qbinom(gamma / 2, N, z)
    lims$upper <- qbinom(1 - gamma / 2, N, z)
  } else {
    n <- N * (L - 1)
    k <- floor(z * L * N)
    lims$lower <- qhyper(gamma / 2, N, n, k)
    lims$upper <- qhyper(1 - gamma / 2, N, n, k)
  }
  lims
}

#' Helper for 'adjust_gamma_simulate`
#' Transforms observations in 'x' into their corresponding fractional ranks.
#' @noRd
u_scale <- function(x) {
  array(rank(x) / length(x), dim = dim(x), dimnames = dimnames(x))
}

# labels ----------------------------------------------------------------
create_rep_ids <- function(ids) paste('italic(y)[rep] (', ids, ")")
y_label <- function() expression(italic(y))
yrep_label <- function() expression(italic(y)[rep])
ypred_label <- function() expression(italic(y)[pred])

# Dependence-aware uniformity tests -------------------------------------

#' Compute Shapley values
#'
#' Calculates the average marginal contribution of players across 
#' all random arrival orders in a cooperative game.
#' Used to provide a principled approach for quantifying 
#' point-specific influences in a way that reflects local miscalibration.
#'
#' @param x Numeric vector of Cauchy-transformed PIT values.
#' @return Numeric vector of Shapley values with the same length as `x`.
#' @noRd
.compute_shapley_values <- function(x) {
  n <- length(x)
  if (n == 0) {
    return(numeric(0))
  }
  if (n == 1) {
    return(0)
  }
  
  # Harmonic number
  # H_n = sum(1/i) for i = 1 to n
  harmonic_number <- sum(1 / seq_len(n))
  
  shapley_values <- numeric(n)
  for (i in seq_len(n)) {
    mean_others <- sum(x[-i]) / (n - 1)
    # Applies the closed-form formula to assign player i their fair share.
    shapley_values[i] <- (1 / n) * x[i] + ((harmonic_number - 1) / n) * (x[i] - mean_others)
  }

  return(shapley_values)
}

#' Pointwise Inverse-CDF Evaluation Tests Combination (PIET)
#'
#' Uniformity test with respect to any continuous distribution.
#' H0: The value obtained via the inverse CDF transformation F^(-1)(x_i)
#' follows the distribution of X under uniformity.
#' HA: The p-value p_(i) provides evidence against uniformity.
#' 
#' @param x Numeric vector of PIT values in `[0, 1]`.
#' @return Numeric vector of p-values.
#' @noRd
.piet_test <- function(x) {
  cdf_exp <- pexp(-log(x), rate = 1) # same as 1-x but numerically more stable
  p_values <- 2 * pmin(cdf_exp, 1 - cdf_exp)

  return(p_values)
}

#' Pointwise Order Tests Combination (POT)
#'
#' Uniformity test based on a beta distribution.
#' H0: The i-th order statistic u_(i) follows a beta distribution
#' under uniformity.
#' HA: The p-value p_(i) provides evidence against uniformity
#' at the i-th order statistic u_(i).
#' 
#' @param x Numeric vector of PIT values in `[0, 1]`.
#' @return Numeric vector of p-values.
#' @noRd
.pot_test <- function(x) {
  n <- length(x)
  # keep NA values instead of silent recycling via sort()
  # TODO: Can PIT values be NAN at this point?
  cdf_beta <- pbeta(sort(x, na.last = TRUE), 1:n, seq(n, 1, by = -1))
  p_values <- 2 * pmin(cdf_beta, 1 - cdf_beta)

  return(p_values)
}

#' Pointwise Rank-based Individual Tests Combination (PRIT)
#' 
#' Uniformity test based on a binomial distribution.
#' H0: The number of observations falling at or below x_i
#' follows a binomial distribution under uniformity.
#' HA: The p-value p_i provides evidence against uniformity.
#' 
#' @param x Numeric vector of PIT values in `[0, 1]`.
#' @return Numeric vector of p-values.
#' @noRd
.prit_test <- function(x) {
  n <- length(x)
  scaled_ecdf <- n * ecdf(x)(x)
  probs1 <- pbinom(scaled_ecdf - 1, n, x)
  probs2 <- pbinom(scaled_ecdf, n, x)
  p_values <- 2 * pmin(1 - probs1, probs2)
  
  return(p_values)
}

#' Truncated Cauchy combination test
#'
#' Combines dependent p-values using the Cauchy combination method.
#' If truncate, only p-values less than 0.5 are included.
#'
#' @param x Numeric vector of p-values transformed to follow a standard
#' Cauchy distribution.
#' @param truncate Boolean; If TRUE only p-values less than 0.5 are
#' included.
#' @return p-value of the Cauchy combination method.
#' @noRd
.cauchy_combination_test <- function(x, truncate = NULL) {
  if (truncate) {
    idx <- which(x < 0.5)
    if (length(idx) == 0) {
      stop("Cannot compute truncated Cauchy combination test. ",
           "No p-values below 0.5 found.")
    }
    1 - pcauchy(mean(-qcauchy(x[idx])))
  } else {
    1 - pcauchy(mean(-qcauchy(x)))
  }
}

#' Compute Cauchy transformation
#'
#' Transforms PIT values to follow a standard Cauchy distribution.
#'
#' @param x Numeric vector of PIT values in `[0, 1]`.
#' @return Numeric vector of Cauchy-transformed values.
#' @noRd
.compute_cauchy <- function(x) {
  tan((0.5 - x) * pi)
}

# Support pareto_pit in ppc_loo_pit_ecdf and ppc_pit_ecdf --------------

#' Pareto-smoothed probability integral transform
#'
#' Compute PIT values using the empirical CDF, then refine values in
#' the tails by fitting a generalized Pareto distribution (GPD) to
#' the tail draws. This gives smoother, more accurate PIT values in
#' the tails where the ECDF is coarse, and avoids PIT values of 0 and 1.
#' Due to use of generalized Pareto distribution CDF in tails, the
#' PIT values are not anymore rank based and continuous uniformity
#' test is appropriate.
#'
#' @name pareto_pit
#'
#' @param x (draws) A [`draws_matrix`] object or one coercible to a
#'   `draws_matrix` object, or an [`rvar`] object.
#'
#' @param y (observations) A 1D vector, or an array of dim(x), if x is `rvar`.
#'   Each element of `y` corresponds to a variable in `x`.
#'
#' @param weights A matrix of weights for each draw and variable. `weights`
#'   should have one column per variable in `x`, and `ndraws(x)` rows.
#'
#' @param log (logical) Are the weights passed already on the log scale? The
#'   default is `FALSE`, that is, expecting `weights` to be on the standard
#'   (non-log) scale.
#'
#' @param ndraws_tail (integer) Number of tail draws to use for GPD
#'   fitting. If `NULL` (the default), computed using [ps_tail_length()].
#'
#' @template args-methods-dots
#'
#' @details The function first computes raw PIT values identically to
#'   [pit()] (including support for weighted draws). It then fits a
#'   GPD to both tails of the draws (using the same approach as
#'   [pareto_smooth()]) and replaces PIT values for observations falling in
#'   the tail regions:
#'
#'   For a right-tail observation \eqn{y_i > c_R} (where \eqn{c_R} is
#'   the right-tail cutoff):
#'
#'   \deqn{PIT(y_i) = 1 - p_{tail}(1 - F_{GPD}(y_i; c_R, \sigma_R, k_R))}
#'
#'   For a left-tail observation \eqn{y_i < c_L}:
#'
#'   \deqn{PIT(y_i) = p_{tail}(1 - F_{GPD}(-y_i; -c_L, \sigma_L, k_L))}
#'
#'   where \eqn{p_{tail}} is the proportion of (weighted) mass in the
#'   tail.
#'
#'   When (log-)weights in `weights` are provided, they are used for
#'   the raw PIT computation (as in [pit()]) and for GPD fit.
#'
#' @return A numeric vector of length `length(y)` containing the PIT values, or
#'   an array of shape `dim(y)`, if `x` is an `rvar`.
#'
#' @seealso [pit()] for the unsmoothed version, [pareto_smooth()] for
#'   Pareto smoothing of draws.
#'
#' @examples
#' x <- example_draws()
#' y <- rnorm(nvariables(x), 5, 5)
#' pareto_pit(x, y)
#'
NULL

#' @rdname pareto_pit
#' @export
pareto_pit <- function(x, y, ...) UseMethod("pareto_pit")

#' @rdname pareto_pit
#' @export
pareto_pit.default <- function(x, y, weights = NULL, log = FALSE,
                               ndraws_tail = NULL, ...) {
  x <- posterior::as_draws_matrix(x)
  if (!is.null(weights)) {
    weights <- posterior::as_draws_matrix(weights)
  }
  pareto_pit(x, y, weights = weights, log = log,
             ndraws_tail = ndraws_tail, ...)
}

#' @rdname pareto_pit
#' @export
pareto_pit.draws_matrix <- function(x, y, weights = NULL, log = FALSE,
                                    ndraws_tail = NULL, ...) {
  y <- pareto_pit_validate_y(y, x)

  # validate and normalize weights to log scale (same as pit.draws_matrix)
  if (!is.null(weights)) {
    weights <- sapply(seq_len(posterior::nvariables(x)), function(var_idx) {
      posterior:::validate_weights(weights[, var_idx], x[, var_idx], log)
    })
    weights <- normalize_log_weights(weights)
  }

  ndraws <- posterior:::ndraws(x)

  if (is.null(ndraws_tail)) {
    ndraws_tail <- posterior::ps_tail_length(ndraws, 1)
  } else {
    ndraws_tail <- posterior::as_one_integer(ndraws_tail)
  }

  # validate ndraws_tail once for all variables
  gpd_ok <- !is.na(ndraws_tail) && ndraws_tail >= 5
  if (gpd_ok && ndraws_tail > ndraws / 2) {
    ndraws_tail <- floor(ndraws / 2)
  }
  if (gpd_ok && ndraws_tail >= ndraws) {
    gpd_ok <- FALSE
  }

  # precompute tail indices (shared across all variables)
  if (gpd_ok) {
    tail_ids <- seq(ndraws - ndraws_tail + 1, ndraws)
  }

  pit_values <- vapply(seq_len(ncol(x)), function(j) {
    draws <- x[, j]

    # --- raw PIT (same logic as pit.draws_matrix) ---
    sel_min <- draws < y[j]
    if (!any(sel_min)) {
      raw_pit <- 0
    } else {
      if (is.null(weights)) {
        raw_pit <- mean(sel_min)
      } else {
        raw_pit <- exp(posterior:::log_sum_exp(weights[sel_min, j]))
      }
    }

    sel_sup <- draws == y[j]
    if (any(sel_sup)) {
      if (is.null(weights)) {
        pit_sup <- raw_pit + mean(sel_sup)
      } else {
        pit_sup <- raw_pit + exp(posterior:::log_sum_exp(weights[sel_sup, j]))
      }
      raw_pit <- runif(1, raw_pit, pit_sup)
    }

    # --- GPD tail refinement ---
    if (!gpd_ok || posterior:::should_return_NA(draws)) {
      return(raw_pit)
    }

    # sort draws and carry weights along
    ord <- sort.int(draws, index.return = TRUE)
    sorted <- ord$x
    log_wt_sorted <- if (!is.null(weights)) weights[ord$ix, j] else NULL

    # tail proportion: sum of (normalized) weights in the tail
    if (!is.null(log_wt_sorted)) {
      tail_proportion <- exp(posterior:::log_sum_exp(log_wt_sorted[tail_ids]))
    } else {
      tail_proportion <- ndraws_tail / ndraws
    }

    # --- right tail ---
    right_replaced <- FALSE
    right_tail <- sorted[tail_ids]
    if (!posterior:::is_constant(right_tail)) {
      right_cutoff <- sorted[min(tail_ids) - 1]
      if (right_cutoff == right_tail[1]) {
        right_cutoff <- right_cutoff - .Machine$double.eps
      }
      right_tail_wt <- if (!is.null(log_wt_sorted)) {
        exp(log_wt_sorted[tail_ids])
      } else {
        NULL
      }
      right_fit <- gpdfit(right_tail - right_cutoff, sort_x = FALSE,
                          weights = right_tail_wt)
      if (is.finite(right_fit$k) && !is.na(right_fit$sigma)) {
        if (y[j] > right_cutoff) {
          gpd_cdf <- pgeneralized_pareto(
            y[j], mu = right_cutoff, sigma = right_fit$sigma, k = right_fit$k
          )
          raw_pit <- 1 - tail_proportion * (1 - gpd_cdf)
          right_replaced <- TRUE
        }
      }
    }

    # --- left tail (negate trick, same as ps_tail) ---
    if (!right_replaced) {
      left_draws <- -draws
      left_ord <- sort.int(left_draws, index.return = TRUE)
      left_sorted <- left_ord$x
      log_wt_left_sorted <- if (!is.null(weights)) weights[left_ord$ix, j] else NULL

      left_tail <- left_sorted[tail_ids]
      if (!posterior:::is_constant(left_tail)) {
        left_cutoff <- left_sorted[min(tail_ids) - 1]
        if (left_cutoff == left_tail[1]) {
          left_cutoff <- left_cutoff - .Machine$double.eps
        }
        left_tail_wt <- if (!is.null(log_wt_left_sorted)) {
          exp(log_wt_left_sorted[tail_ids])
        } else {
          NULL
        }
        left_fit <- gpdfit(left_tail - left_cutoff, sort_x = FALSE,
                           weights = left_tail_wt)
        if (is.finite(left_fit$k) && !is.na(left_fit$sigma)) {
          if (-y[j] > left_cutoff) {
            gpd_cdf <- pgeneralized_pareto(
              -y[j], mu = left_cutoff, sigma = left_fit$sigma, k = left_fit$k
            )
            if (!is.null(log_wt_left_sorted)) {
              left_tail_proportion <- exp(posterior:::log_sum_exp(log_wt_left_sorted[tail_ids]))
            } else {
              left_tail_proportion <- tail_proportion
            }
            raw_pit <- left_tail_proportion * (1 - gpd_cdf)
          }
        }
      }
    }

    raw_pit
  }, FUN.VALUE = 1.0)

  min_tail_prob <- 1/ndraws/1e4
  pit_values <- pmin(pmax(pit_values, min_tail_prob), 1-min_tail_prob)
  setNames(pit_values, posterior::variables(x))
}

#' @rdname pareto_pit
#' @export
pareto_pit.rvar <- function(x, y, weights = NULL, log = FALSE,
                            ndraws_tail = NULL, ...) {
  y <- pareto_pit_validate_y(y, x)
  out <- array(
    data = pareto_pit(
      x = as_draws_matrix(c(x)),
      y = c(y),
      weights = weights,
      log = log,
      ndraws_tail = ndraws_tail,
      ...
    ),
    dim = dim(x),
    dimnames = dimnames(x)
  )
  out
}

# internal ----------------------------------------------------------------

pareto_pit_validate_y <- function(y, x = NULL) {
  if (!is.numeric(y)) {
    posterior:::stop_no_call("`y` must be numeric.")
  }
  if (anyNA(y)) {
    posterior:::stop_no_call("NAs not allowed in `y`.")
  }
  if (posterior::is_rvar(x)) {
    if (length(x) != length(y) || any(dim(y) != dim(x))) {
      posterior:::stop_no_call("`dim(y)` must match `dim(x)`.")
    }
  } else if (posterior::is_draws(x)) {
    if (!is.vector(y, mode = "numeric") || length(y) != posterior::nvariables(x)) {
      posterior:::stop_no_call("`y` must be a vector of length `nvariables(x)`.")
    }
  }
  y
}

normalize_log_weights <- function(log_weights) {
  apply(log_weights, 2, function(col) col - posterior:::log_sum_exp(col))
}


#' Quantile function for the generalized Pareto distribution
#'
#' Computes the quantile function for a generalized Pareto distribution
#' with location `mu`, scale `sigma`, and shape `k`.
#' 
#' @param p Numeric vector of probabilities.
#' @param mu Location parameter.
#' @param sigma Scale parameter (must be positive).
#' @param k Shape parameter.
#' @param lower.tail Logical; if `TRUE` (default), probabilities are `P[X <= x]`.
#' @param log.p Logical; if `TRUE`, probabilities are given as `log(p)`.
#' @return A numeric vector of quantiles.
#' @keywords internal
#' @export
#' @examples
#' qgeneralized_pareto(p = c(0.1, 0.5, 0.9), mu = 0, sigma = 1, k = 0.2)
qgeneralized_pareto <- function(p, mu = 0, sigma = 1, k = 0, lower.tail = TRUE, log.p = FALSE) {
  stopifnot(length(mu) == 1 && length(sigma) == 1 && length(k) == 1)
  if (is.na(sigma) || sigma <= 0) {
    return(rep(NaN, length(p)))
  }
  if (log.p) {
    p <- exp(p)
  }
  if (!lower.tail) {
    p <- 1 - p
  }
  if (k == 0) {
    q <-  mu - sigma * log1p(-p)
  } else {
    q <- mu + sigma * expm1(-k * log1p(-p)) / k
  }
  q
}

#' Distribution function for the generalized Pareto distribution
#'
#' Computes the cumulative distribution function (CDF) for a generalized
#' Pareto distribution with location `mu`, scale `sigma`, and shape `k`.
#'
#' @param q Numeric vector of quantiles.
#' @param mu Location parameter.
#' @param sigma Scale parameter (must be positive).
#' @param k Shape parameter.
#' @param lower.tail Logical; if `TRUE` (default), probabilities are `P[X <= x]`.
#' @param log.p Logical; if `TRUE`, probabilities are returned as `log(p)`.
#' @return A numeric vector of probabilities.
#' @keywords internal
#' @export
#' @examples
#' pgeneralized_pareto(q = c(1, 2, 5), mu = 0, sigma = 1, k = 0.2)
pgeneralized_pareto <- function(q, mu = 0, sigma = 1, k = 0, lower.tail = TRUE, log.p = FALSE) {
  stopifnot(length(mu) == 1 && length(sigma) == 1 && length(k) == 1)
  if (is.na(sigma) || sigma <= 0) {
    return(rep(NaN, length(q)))
  }
  z <- (q - mu) / sigma
  if (abs(k) < 1e-15) {
    # for very small values of indistinguishable in floating point accuracy from the case k=0
    p <- -expm1(-z)
  } else {
    # pmax handles values outside the support
    p <- -expm1(log1p(pmax(k * z, -1)) / -k)
  }
  # force to [0, 1] for values outside the support
  p <- pmin(pmax(p, 0), 1)
  if (!lower.tail) {
    p <- 1 - p
  }
  if (log.p) {
    p <- log(p)
  }
  p
}

#' Estimate parameters of the Generalized Pareto distribution
#'
#' Given a sample \eqn{x}, Estimate the parameters \eqn{k} and
#' \eqn{\sigma} of the generalized Pareto distribution (GPD), assuming
#' the location parameter is 0. By default the fit uses a prior for
#' \eqn{k} (this is in addition to the prior described by Zhang and
#' Stephens, 2009), which will stabilize estimates for very small
#' sample sizes (and low effective sample sizes in the case of MCMC
#' samples). The weakly informative prior is a Gaussian prior centered
#' at 0.5 (see details in Vehtari et al., 2024). This is used
#' internally but is exported for use by other packages.
#' @family helper-functions
#' @param x A numeric vector. The sample from which to estimate the
#'   parameters.
#' @param wip Logical indicating whether to adjust \eqn{k} based on a
#'   weakly informative Gaussian prior centered on 0.5. Defaults to
#'   `TRUE`.
#' @param min_grid_pts The minimum number of grid points used in the
#'   fitting algorithm. The actual number used is `min_grid_pts +
#'   floor(sqrt(length(x)))`.
#' @param sort_x If `TRUE` (the default), the first step in the
#'   fitting algorithm is to sort the elements of `x`. If `x` is
#'   already sorted in ascending order then `sort_x` can be set to
#'   `FALSE` to skip the initial sorting step.
#' @param weights An optional numeric vector of positive weights the same
#'   length as `x`. If `NULL` (the default), all observations are
#'   weighted equally and the result is identical to the unweighted fit.
#'   Weights are normalized internally to sum to `length(x)`.
#' @return A named list with components `k` and `sigma`.
#'
#' @details Here the parameter \eqn{k} is the negative of \eqn{k} in Zhang &
#'   Stephens (2009).
#'
#'
#' @references
#' Zhang, J., and Stephens, M. A. (2009). A new and efficient estimation method
#' for the generalized Pareto distribution. *Technometrics* **51**, 316-325.
#'
#' @keywords internal
#' @export
gpdfit <- function(x, wip = TRUE, min_grid_pts = 30, sort_x = TRUE,
                   weights = NULL) {
  # see section 4 of Zhang and Stephens (2009)
  if (sort_x) {
    if (!is.null(weights)) {
      ord <- sort.int(x, index.return = TRUE)
      x <- ord$x
      weights <- weights[ord$ix]
    } else {
      x <- sort.int(x)
    }
  }
  N <- length(x)

  # normalize weights to sum to N so the log-likelihood scale is preserved
  if (!is.null(weights)) {
    weights <- weights / sum(weights) * N
  }

  prior <- 3
  M <- min_grid_pts + floor(sqrt(N))
  jj <- seq_len(M)
  xstar <- x[floor(N / 4 + 0.5)] # first quartile of sample
  if (xstar > x[1])  {
    # first quantile is bigger than the minimum
    theta <- 1 / x[N] + (1 - sqrt(M / (jj - 0.5))) / prior / xstar

    # log1p(-theta %o% x) is M x N matrix
    log1p_mat <- log1p(-theta %o% x)

    if (!is.null(weights)) {
      # weighted mean across observations for each theta value
      k <- drop(log1p_mat %*% weights) / N
    } else {
      k <- matrixStats::rowMeans2(log1p_mat)
    }

    l_theta <- N * (log(-theta / k) - k - 1) # profile log-lik
    w_theta <- exp(l_theta - matrixStats::logSumExp(l_theta)) # normalize
    theta_hat <- sum(theta * w_theta)

    if (!is.null(weights)) {
      k_hat <- sum(weights * log1p(-theta_hat * x)) / N
    } else {
      k_hat <- mean.default(log1p(-theta_hat * x))
    }
    sigma_hat <- -k_hat / theta_hat

    # adjust k_hat based on weakly informative prior, Gaussian centered on 0.5.
    # this stabilizes estimates for very small Monte Carlo sample sizes and low ess
    # (see Vehtari et al., 2024 for details)
    if (wip) {
      k_hat <- (k_hat * N + 0.5 * 10) / (N + 10)
    }
    if (is.na(k_hat)) {
      k_hat <- Inf
      sigma_hat <- NaN
    }
  } else {
    # first quantile is not bigger than the minimum, which indicates
    # that the distribution is far from a generalized Pareto
    # distribution
    k_hat <- NA
    sigma_hat <- NA
  }

  list(k = k_hat, sigma = sigma_hat)
}