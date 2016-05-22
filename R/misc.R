# Validate y
#
# Checks that y is numeric, doesn't have any NAs, and is either a vector or 1-D
# array.
#
# @param y The y object from the user.
# @return Either throws an error or returns a numeric vector.
#
validate_y <- function(y) {
  stopifnot(is.numeric(y))
  if (!is.vector(y)) {
    if (!(is.array(y) && length(dim(y)) == 1))
      stop("'y' must be a vector or a 1-D array.")
    y <- as.vector(y)
  }
  if (anyNA(y))
    stop("NAs not allowed in 'y'.")

  y
}

# Validate yrep
#
# Checks that yrep is a numeric matrix, doesn't have any NAs, and has the
# correct number of columns (equal to the length of y).
#
# @param yrep,y The user's yrep object and the y object returned by validate_y.
# @return Either throws an error or returns a numeric matrix.
#
validate_yrep <- function(yrep, y) {
  stopifnot(is.vector(y)) # y should already be validated
  stopifnot(is.matrix(yrep), is.numeric(yrep))
  if (anyNA(yrep))
    stop("NAs not allowed in 'yrep'.")
  if (ncol(yrep) != length(y))
    stop("ncol(yrep) not equal to length(y).")

  yrep
}


# Convert yrep matrix into a molten data frame
#
# @param yrep A matrix.
# @return A data frame with three columns:
# \itemize{
#  \item 'value': the numeric values.
#  \item 'y_id': integer indicating which yrep column each of the values comes
#  from.
# \item 'rep_id': factor with levels 'yrep_1', ..., 'yrep_S', where S is
#  nrow(yrep), i.e. the number of simulations included in yrep.
# }
#
melt_yrep <- function(yrep) {
  out <- reshape2::melt(
    data = yrep,
    varnames = c("rep_id", "y_id")
  )
  out$rep_id <- paste0('yrep_', out$rep_id)
  out$rep_id <- factor(out$rep_id, levels = unique(out$rep_id))
  out
}


# Call a geom, passing arguments as a list
#
# @param geom A string naming the geom (e.g. "histogram", "ribbon", etc.)
# @param args A list of arguments to pass to \code{geom}.
# @param ... Optional arguments passed to \code{do.call}.
#
call_geom <- function(geom, args, ...) {
  stopifnot(is.character(geom), is.list(args))
  do.call(
    what = paste0("geom_", geom),
    args = args,
    ...
  )
}


set_geom_args <- function(defaults, ...) {
  dots <- list(...)
  if (!length(dots))
    return(defaults)
  dot_names <- names(dots)
  def_names <- names(defaults)
  for (j in seq_along(def_names)) {
    if (def_names[j] %in% dot_names)
      defaults[[j]] <- dots[[def_names[j]]]
  }
  extras <- setdiff(dot_names, def_names)
  if (length(extras)) {
    for (j in seq_along(extras))
      defaults[[extras[j]]] <- dots[[extras[j]]]
  }
  defaults
}

`%ORifNULL%` <- function(a, b) {
  if (is.null(a)) b else a
}
