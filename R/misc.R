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
  if (is.integer(yrep))
    yrep <- apply(yrep, 2, as.numeric)

  if (anyNA(yrep))
    stop("NAs not allowed in 'yrep'.")
  if (ncol(yrep) != length(y))
    stop("ncol(yrep) not equal to length(y).")

  yrep
}


# Validate group
#
# Checks that grouping variable has same length as y and is either a vector or
# factor variable.
#
# @param group,y The user's group object and the y object returned by validate_y.
# @return Either throws an error or returns a numeric matrix.
#
validate_group <- function(group, y) {
  stopifnot(is.vector(y)) # y should already be validated
  stopifnot(is.vector(group) || is.factor(group))
  if (is.character(group))
    group <- factor(group)

  if (anyNA(group))
    stop("NAs not allowed in 'group'.")
  if (length(group) != length(y))
    stop("length(group) not equal to length(y).")

  group
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

# Prepare data for use in PPCs by group
#
# @param y,yrep,group Validated y, yrep, and group objects from the user.
# @param stat Either NULL or a string naming a function.
# @value If \code{stat} is NULL, a molten data frame grouped by group and
#   variable. If \code{stat} specifies a function then a summary table created
#   by dplyr::summarise.
#
ppc_group_data <- function(y, yrep, group, stat = NULL) {
  if (!requireNamespace("dplyr", quietly = TRUE))
    stop("Please install the dplyr package.")

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

create_yrep_ids <- function(ids) paste('italic(y)[rep] (', ids, ")")
yrep_label <- function() expression(italic(y)^rep)
yrep_avg_label <- function() expression(paste("Average ", italic(y)^rep))
y_label <- function() expression(italic(y))
Ty_label <- function() expression(italic(T(y)))
Tyrep_label <- function() expression(italic(T)(italic(y)^rep))
