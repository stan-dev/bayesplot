# Check if an object is a vector (but not list) or a 1-D array
is_vector_or_1Darray <- function(x) {
  if (is.vector(x) && !is.list(x)) {
    return(TRUE)
  }

  isTRUE(is.array(x) && length(dim(x)) == 1)
}


#' Validate y
#'
#' Checks that y is numeric, doesn't have any NAs, and is either a vector, 1-D
#' array, or univariate time series object of class \code{ts}.
#'
#' @param y The y object from the user.
#' @return Either throws an error or returns a numeric vector.
#' @noRd
validate_y <- function(y) {
  stopifnot(is.numeric(y))

  if (!(inherits(y, "ts") && is.null(dim(y)))) {
    if (!is_vector_or_1Darray(y)) {
      stop("'y' must be a vector or 1D array.")
    }
    y <- as.vector(y)
  }

  if (anyNA(y)) {
    stop("NAs not allowed in 'y'.")
  }

  unname(y)
}


#' Validate yrep
#'
#' Checks that yrep is a numeric matrix, doesn't have any NAs, and has the
#' correct number of columns (equal to the length of y).
#'
#' @param yrep,y The user's yrep object and the y object returned by validate_y.
#' @return Either throws an error or returns a numeric matrix.
#' @noRd
validate_yrep <- function(yrep, y) {
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
    stop("NAs not allowed in 'yrep'.")
  }

  if (!missing(y)) {
    if (ncol(yrep) != length(y)) {
      stop("ncol(yrep) must be equal to length(y).")
    }
  }

  unclass(unname(yrep))
}


#' Validate group
#'
#' Checks that grouping variable has same length as y and is either a vector or
#' factor variable.
#'
#' @param group,y The user's group object and the y object returned by validate_y.
#' @return Either throws an error or returns \code{group} (coerced to a factor).
#' @noRd
validate_group <- function(group, y) {
  stopifnot(is.vector(group) || is.factor(group))

  if (!is.factor(group)) {
    group <- as.factor(group)
  }

  if (anyNA(group)) {
    stop("NAs not allowed in 'group'.")
  }

  if (length(group) != length(y)) {
    stop("length(group) must be equal to length(y).")
  }

  if (length(unique(group)) == 1) {
    stop("'group' must have more than one unique value.")
  }

  unname(group)
}


#' Validate x
#'
#' Checks that x is a numeric vector, doesn't have any NAs, and has the
#' same length as y.
#'
#' @param x,y The user's x vector and the y object returned by validate_y.
#' @param unique_x T/F indicating whether to require all unique values in x.
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
    stop("'x' must be a vector or 1D array.")
  }

  x <- as.vector(x)
  if (length(x) != length(y)) {
    stop("length(x) must be equal to length(y).")
  }

  if (anyNA(x)) {
    stop("NAs not allowed in 'x'.")
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
#' @md
melt_yrep <- function(yrep) {
  out <- yrep %>%
    reshape2::melt(varnames = c("rep_id", "y_id")) %>%
    dplyr::as_data_frame()
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
#' @md
melt_and_stack <- function(y, yrep) {
  y_text <- as.character(y_label())
  yrep_text <- as.character(yrep_label())

  molten_yrep <- melt_yrep(yrep)

  # Add a level in the labels for the observed y values
  levels(molten_yrep$rep_label) <- c(levels(molten_yrep$rep_label), y_text)

  ydat <- dplyr::data_frame(
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
#' @param y,yrep,group Validated y, yrep, and group objects.
#' @param stat Either NULL or a string naming a function.
#' @return If \code{stat} is NULL, a molten data frame grouped by group and
#'   variable. If \code{stat} specifies a function then a summary table created
#'   by dplyr::summarise.
#' @noRd
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

# check if x consists of whole numbers (very close to integers)
is_whole_number <- function(x, tol = .Machine$double.eps) {
  if (!is.numeric(x)) {
    FALSE
  } else {
    abs(x - round(x)) < tol
  }
}

# check if all values in x are counts (non-negative whole numbers)
all_counts <- function(x, ...) {
  all(is_whole_number(x, ...)) && min(x) >= 0
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
