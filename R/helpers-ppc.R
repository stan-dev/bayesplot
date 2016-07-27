# Check if an object is a vector (but not list) or a 1-D array
is_vector_or_1Darray <- function(x) {
  if (is.vector(x) && !is.list(x))
    return(TRUE)
  isTRUE(is.array(x) && length(dim(x)) == 1)
}

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
  if (!is_vector_or_1Darray(y))
    stop("'y' must be a vector or 1D array.")

  y <- as.vector(y)
  if (anyNA(y))
    stop("NAs not allowed in 'y'.")

  unname(y)
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
  stopifnot(is.matrix(yrep), is.numeric(yrep))
  if (is.integer(yrep)) {
    if (nrow(yrep) == 1)
      yrep[1, ] <- as.numeric(yrep[1,, drop = FALSE])
    else
      yrep <- apply(yrep, 2, as.numeric)
  }
  if (anyNA(yrep))
    stop("NAs not allowed in 'yrep'.")
  if (ncol(yrep) != length(y))
    stop("ncol(yrep) must be equal to length(y).")

  unname(yrep)
}


# Validate group
#
# Checks that grouping variable has same length as y and is either a vector or
# factor variable.
#
# @param group,y The user's group object and the y object returned by validate_y.
# @return Either throws an error or returns \code{group} (coerced to a factor).
#
validate_group <- function(group, y) {
  stopifnot(is.vector(group) || is.factor(group))
  if (!is.factor(group))
    group <- as.factor(group)
  if (anyNA(group))
    stop("NAs not allowed in 'group'.")
  if (length(group) != length(y))
    stop("length(group) must be equal to length(y).")
  if (length(unique(group)) == 1)
    stop("'group' must have more than one unique value.")

  unname(group)
}

# Validate time
#
# Checks that time variable has same length as y and is numeric.
#
# @param time,y The user's time object and the y object returned by validate_y.
# @return Either throws an error or returns a numeric vector.
#
validate_time <- function(time, y, unique_times = TRUE) {
  if (missing(time))
    return(1:length(y))

  stopifnot(is.numeric(time))
  if (!is_vector_or_1Darray(time))
    stop("'time' must be a vector or a 1D array.")
  time <- as.vector(time)

  if (anyNA(time))
    stop("NAs not allowed in 'time'.")

  if (!identical(length(time), length(y)))
    stop("length(time) must be equal to length(y).")

  if (unique_times)
    stopifnot(identical(length(time), length(unique(time))))

  unname(time)
}


# Validate x
#
# Checks that x is a numeric vector, doesn't have any NAs, and has the
# same length as y.
#
# @param x,y The user's x vector and the y object returned by validate_y.
# @return Either throws an error or returns a numeric vector.
#
validate_x <- function(x, y) {
  stopifnot(is.numeric(x))

  if (!is_vector_or_1Darray(x))
    stop("'x' must be a vector or 1D array.")

  x <- as.vector(x)
  if (length(x) != length(y))
    stop("length(x) must be equal to length(y).")
  if (anyNA(x))
    stop("NAs not allowed in 'x'.")

  unname(x)
}


# Validate test statistic
#
# Checks that the correct number of functions is specified for computing test
# statistics and that they are specified using strings.
#
# @param stat The user's 'stat' argument.
# @param n_allowed The allowed length of 'stat'. Either 1 or 2.
# @return Either throws an error or returns a character vector.
#
validate_stat <- function(stat, n_allowed) {
  stopifnot(
    n_allowed %in% c(1,2),
    is.character(stat)
  )

  if (length(stat) != n_allowed)
    stop("For this function 'stat' must have length ", n_allowed, ".")

  stat
}


# Convert yrep matrix into a molten data frame
#
# @param yrep A matrix, already validated using validate_yrep().
# @return A data frame with three columns:
# \itemize{
#  \item 'value': the numeric values.
#  \item 'y_id': integer indicating from which yrep column each values comes.
# \item 'rep_id': factor with S levels, where S is nrow(yrep), i.e. the number
#   of simulations included in yrep.
# }
#
melt_yrep <- function(yrep, label = TRUE) {
  out <- reshape2::melt(
    data = yrep,
    varnames = c("rep_id", "y_id")
  )
  id <- if (label) create_yrep_ids(out$rep_id) else out$rep_id
  out$rep_id <- factor(id, levels = unique(id))
  out
}

# Stack y below melted yrep data
#
# @param y Validated y input.
# @param yrep Validated yrep input.
# @return A data frame with the all the columns as the one returned by
#   melt_yrep(), plus a column "is_y" indicating whether the values pertain
#   to y (or yrep).
#
melt_and_stack <- function(y, yrep) {
  molten_yrep <- melt_yrep(yrep)
  yobs_lab <- "italic(y)"
  levels(molten_yrep$rep_id) <- c(levels(molten_yrep$rep_id), yobs_lab)
  ydat <- data.frame(
    rep_id = yobs_lab,
    y_id = seq_along(y),
    value = y
  )
  within(data = rbind(molten_yrep, ydat), {
    rep_id <- relevel(rep_id, ref = yobs_lab)
    is_y <- rep_id == yobs_lab
  })
}

# Prepare data for use in PPCs by group
#
# @param y,yrep,group Validated y, yrep, and group objects from the user.
# @param stat Either NULL or a string naming a function.
# @value If \code{stat} is NULL, a molten data frame grouped by group and
#   variable. If \code{stat} specifies a function then a summary table created
#   by dplyr::summarise.
#
ppc_group_data <- function(y, yrep, group, stat = NULL) {
  d <- data.frame(
    group = factor(group),
    y = y,
    yrep = t(yrep)
  )
  colnames(d) <- gsub(".", "_", colnames(d), fixed = TRUE)
  molten_d <- reshape2::melt(d, id.vars = "group")
  molten_d <- dplyr::group_by_(molten_d, .dots = list(~group, ~variable))
  if (is.null(stat))
    return(molten_d)

  stat <- match.fun(stat)
  dplyr::summarise_(molten_d, value = ~stat(value))
}


# labels ----------------------------------------------------------------
create_yrep_ids <- function(ids) paste('italic(y)[rep] (', ids, ")")
yrep_label <- function() expression(italic(y)^rep)
yrep_avg_label <- function() expression(paste("Average ", italic(y)^rep))
y_label <- function() expression(italic(y))
Ty_label <- function() expression(italic(T(y)))
Tyrep_label <- function() expression(italic(T)(italic(y)^rep))
