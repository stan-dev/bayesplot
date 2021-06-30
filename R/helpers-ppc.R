# Check if an object is a vector (but not list) or a 1-D array
is_vector_or_1Darray <- function(x) {
  if (is.vector(x) && !is.list(x)) {
    return(TRUE)
  }

  isTRUE(is.array(x) && length(dim(x)) == 1)
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


#' Validate yrep
#'
#' Checks that `yrep` is a numeric matrix, doesn't have any NAs, and has the
#' correct number of columns (equal to the length of `y`).
#'
#' @param yrep,y The user's `yrep` object and the `y` object returned by `validate_y()`.
#' @param match_ncols Is the number of columns in 'yrep' required to match the length of 'y'.
#' @return Either throws an error or returns a numeric matrix.
#' @noRd
validate_yrep <- function(yrep, y, match_ncols = TRUE) {
  stopifnot(is.matrix(yrep), is.numeric(yrep))
  if (is.integer(yrep)) {
    if (nrow(yrep) == 1) {
      yrep[1, ] <- as.numeric(yrep[1,, drop = FALSE])
    }
    else {
      yrep <- apply(yrep, 2, as.numeric)
    }
  }

  if (anyNA(yrep)) {
    abort("NAs not allowed in 'yrep'.")
  }

  if (all(match_ncols, ncol(yrep) != length(y))) {
    abort("ncol(yrep) must be equal to length(y).")
  }

  unclass(unname(yrep))
}


#' Validate PIT
#'
#' Checks that the probability integral transformation (PIT) values from
#' a numeric matrix with no NAs and that the provided values fall in [0,1].
#'
#' @param pit The 'pit' object provided by the user.
#' @return Either throws an error or returns a numeric matrix.
#' @noRd
validate_pit <- function(pit) {
  stopifnot(is.matrix(pit), is.numeric(yrep))
  if (any(pit < 0) || any(pit > 1)) {
    abort("'pit' values expected to lie between 0 and 1 (inclusive).")
  }

  if (anyNA(pit)) {
    abort("NAs not allowed in 'pit'.")
  }

  unclass(unname(pit))
}

#' Validate group
#'
#' Checks that grouping variable has same length as `y` and is either a vector or
#' factor variable.
#'
#' @param group,y The user's `group` object and the `y` object returned by
#'   `validate_y()`.
#' @return Either throws an error or returns `group` (coerced to a factor).
#' @noRd
validate_group <- function(group, y) {
  stopifnot(is.vector(group) || is.factor(group))

  if (!is.factor(group)) {
    group <- as.factor(group)
  }

  if (anyNA(group)) {
    abort("NAs not allowed in 'group'.")
  }

  if (length(group) != length(y)) {
    abort("length(group) must be equal to length(y).")
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


#' Convert yrep matrix into a molten data frame
#'
#' @param yrep A matrix, already validated using `validate_yrep()`.
#' @return A data frame with 4 columns:
#'   1. `y_id`: integer indicating the observation number (`yrep` column).
#'   1. `rep_id`: integer indicating the simulation number (`yrep` row).
#'   1. `rep_label`: factor with S levels, where S is `nrow(yrep)`, i.e. the
#'      number of simulations included in `yrep`.
#'   1. `value`: the simulation values.
#' @noRd
melt_yrep <- function(yrep) {
  out <- yrep %>%
    reshape2::melt(varnames = c("rep_id", "y_id")) %>%
    tibble::as_tibble()
  id <- create_yrep_ids(out$rep_id)
  out$rep_label <- factor(id, levels = unique(id))
  out[c("y_id", "rep_id", "rep_label", "value")]
}


#' Stack y below melted yrep data
#'
#' @param y Validated y input.
#' @param yrep Validated yrep input.
#' @return A data frame with the all the columns as the one returned by
#'   `melt_yrep()`, plus additional columns:
#'   1. `is_y`: logical indicating whether the values are observations (`TRUE`)
#'      or simulations (`FALSE`).
#'   1. `is_y_label`: factor with levels `italic(y)` for observations and
#'      `italic(y)[rep]` for simulations.
#' @noRd
melt_and_stack <- function(y, yrep) {
  y_text <- as.character(y_label())
  yrep_text <- as.character(yrep_label())

  molten_yrep <- melt_yrep(yrep)

  # Add a level in the labels for the observed y values
  levels(molten_yrep$rep_label) <- c(levels(molten_yrep$rep_label), y_text)

  ydat <- tibble::tibble(
    rep_label = factor(y_text, levels = levels(molten_yrep$rep_label)),
    rep_id = NA_integer_,
    y_id = seq_along(y),
    value = y)

  data <- dplyr::bind_rows(molten_yrep, ydat) %>%
    mutate(
      rep_label = relevel(.data$rep_label, y_text),
      is_y = is.na(.data$rep_id),
      is_y_label = ifelse(.data$is_y, y_text, yrep_text) %>%
        factor(levels = c(y_text, yrep_text)))

  data[c("y_id", "rep_id", "rep_label", "is_y", "is_y_label", "value")]
}


#' Prepare data for use in PPCs by group
#'
#' @param y,yrep,group Validated `y`, `yrep`, and `group` objects.
#' @param stat Either `NULL` or a string naming a function.
#' @return If `stat` is `NULL`, a molten data frame grouped by group and
#'   variable. If `stat` specifies a function then a summary table created
#'   by `dplyr::summarise()`.
#' @noRd
#'
#' @examples
#' y <- example_y_data()
#' yrep <- example_yrep_draws()
#' group <- example_group_data()
#' ppc_group_data(y, yrep, group)
#' ppc_group_data(y, yrep, group, median)
ppc_group_data <- function(y, yrep, group, stat = NULL) {
  d <- data.frame(
    group = factor(group),
    y = y,
    yrep = t(yrep)
  )
  colnames(d) <- gsub(".", "_", colnames(d), fixed = TRUE)
  molten_d <- reshape2::melt(d, id.vars = "group")
  molten_d <- dplyr::group_by(molten_d, .data$group, .data$variable)

  # Default to identity function.
  dplyr_fun <- dplyr::summarise
  if (is.null(stat)) {
    stat <- function(x) x
    dplyr_fun <- dplyr::mutate
  }

  stat <- match.fun(stat)
  dplyr_fun(molten_d, value = stat(.data$value))

  # todo: does this result need to be ungrouped. If mutating path, it has two
  # grouping vars. It summarising path, it has one grouping var.
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

adjust_gamma <- function(N, L, K=N, conf_level=0.95) {
  if (any(c(K, N, L) < 1)) {
    abort("Parameters 'N', 'L' and 'K' must be positive integers.")
  }
  if (conf_level >= 1 || conf_level <= 0) {
    abort("Value of 'conf_level' must be in (0,1).")
  }
  if (L==1) {
    gamma <- adjust_gamma_optimize(N, K, conf_level)
  }
  else {
    gamma <- adjust_gamma_simulate(N, L, K, conf_level)
  }
  gamma
}

adjust_gamma_optimize <- function(N, K, conf_level=0.95) {
  target <- function(gamma, conf_level, N, K) {
    z <- 1:(K - 1) / K
    z1 <- c(0,z)
    z2 <- c(z,1)

    # pre-compute quantiles and use symmetry for increased efficiency.
    x2_lower <- qbinom(gamma / 2, N, z2)
    x2_upper <- c(N - rev(x2_lower)[2:K], 1)

    # Compute the total probability of trajectories inside the confidence
    # intervals. Initialize the set and corresponding probasbilities known
    # to be 0 and 1 for the starting value z1 = 0.
    x1 <- 0
    p_int <- 1
    for (i in seq_along(z1)) {
      tmp <- p_interior(
        p_int, x1 = x1, x2 = x2_lower[i]: x2_upper[i],
        z1 = z1[i], z2 = z2[i], gamma = gamma, N = N
      )
      x1 <- tmp$x1
      p_int <- tmp$p_int
    }
    abs(conf_level - sum(p_int))
  }
  optimize(target, c(0, 1 - conf_level), conf_level, N = N, K = K)$minimum
}

adjust_gamma_simulate <-function(N, L, K, conf_level=0.95, M=5000) {
  gamma <- numeric(M)
  z <- (1:(K - 1)) / K
  if (L > 1){
    n <- N * (L - 1)
    k <- floor(z * N * L)
    for (m in seq_len(M)) {
      u = u_scale(replicate(L, runif(N)))
      scaled_ecdfs <- apply(outer(u, z, "<="), c(2,3), sum)
      gamma[m] <- 2 * min(
        apply(
          scaled_ecdfs, 1, phyper, m = N, n = n, k = k
        ),
        apply(
          scaled_ecdfs - 1, 1, phyper, m = N, n = n, k = k, lower.tail = FALSE
        )
      )
    }

  }
  else {
    for (m in seq_len(M)) {
      u <- runif(N)
      scaled_ecdf <- colSums(outer(u, z, "<="))
      gamma[m] <- 2 * min(
        pbinom(scaled_ecdf, N, z),
        pbinom(scaled_ecdfs - 1, N, z, lower.tail = FALSE)
      )
    }
  }
  alpha_quantile(gamma, 1 - conf_level)
}

p_interior <- function(p_int, x1, x2, z1, z2, gamma, N) {
  z_tilde <- (z2 - z1) / (1 - z1)

  N_tilde <- rep(N - x1, each = length(x2))
  p_int <- rep(p_int, each = length(x2))
  x_diff <- outer(x2, x1, "-")
  p_x2_int <- p_int * dbinom(x_diff, N_tilde, z_tilde)

  list(p_int = rowSums(p_x2_int), x1 = x2)
}

# alpha percent of the trials are allowed to be rejected
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

ecdf_intervals <- function(N, L, K, gamma) {
  lims <- list()
  z <- seq(0,1, length.out = K + 1)
  if (L == 1) {
    lims$lower <- qbinom(gamma / 2, N, z)
    lims$upper <- qbinom(1 - gamma / 2, N, z)
  } else {
    n <- N * (L - 1)
    k <- floor(z * L * N)
    lims$lower <- qhyper(gamma / 2, N, n, k)
    lims$upper <- qhyper(1 - gamma / 2, N, n, k)
  }
  lims$lower <- c(rep(lims$lower[1:K], each=2), lims$lower[K + 1])
  lims$upper <- c(rep(lims$upper[1:K], each=2), lims$upper[K + 1])
  lims
}

# Transform observations in 'x' into their corresponding fractional ranks.
u_scale <- function(x) {
  array(rank(x) / length(x), dim = dim(x), dimnames = dimnames(x))
}


empirical_pit <- function(y, yrep) {
  apply(outer(yrep, y, "<="), 3, sum) / length(yrep)
}

# labels ----------------------------------------------------------------
create_yrep_ids <- function(ids) paste('italic(y)[rep] (', ids, ")")
yrep_label <- function() expression(italic(y)[rep])
yrep_avg_label <- function() expression(paste("Average ", italic(y)[rep]))
y_label <- function() expression(italic(y))
Ty_label <- function() expression(italic(T(italic(y))))
Tyrep_label <- function() expression(italic(T)(italic(y)[rep]))
# Ty_label_2d <- function() {
#   expression(bgroup(
#     "(", list(italic(T)[1](italic(y)),
#               italic(T)[2](italic(y))), ")"
#   ))
# }
# Tyrep_label_2d <- function(k) {
#   stopifnot(k == 1 || k == 2)
#   if (k == 1) expression(paste(italic(T)[1](italic(y)[rep])))
#   else expression(paste(italic(T)[2](italic(y)[rep])))
# }
