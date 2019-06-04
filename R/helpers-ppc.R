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

  if (length(unique(group)) == 1) {
    abort("'group' must have more than one unique value.")
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
  args$... <- NULL
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


# labels ----------------------------------------------------------------
create_rep_ids <- function(ids) paste('italic(y)[rep] (', ids, ")")
y_label <- function() expression(italic(y))
yrep_label <- function() expression(italic(y)[rep])
ypred_label <- function() expression(italic(y)[pred])

