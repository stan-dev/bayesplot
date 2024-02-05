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
#' or a 1-D array with values in [0,1].
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
#' @param z1 Left evaluation point in [0,1]
#' @param z2 Right evaluation point in [0,1] with z2 > z1.
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
