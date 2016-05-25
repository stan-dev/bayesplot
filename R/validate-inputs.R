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
  stopifnot(is.vector(y))
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
    stop("ncol(yrep) not equal to length(y).")

  unname(yrep)
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
  stopifnot(is.vector(group) || is.factor(group))
  if (is.character(group))
    group <- factor(group)

  if (anyNA(group))
    stop("NAs not allowed in 'group'.")
  if (length(group) != length(y))
    stop("length(group) not equal to length(y).")

  group
}

# Validate time
#
# Checks that time variable has same length as y and is numeric.
#
# @param time,y The user's time object and the y object returned by validate_y.
# @return Either throws an error or returns a numeric vector.
#
validate_time <- function(time, y) {
  if (missing(time))
    return(1:length(y))

  stopifnot(is.numeric(time))
  if (!is.vector(time)) {
    if (!(is.array(time) && length(dim(time)) == 1))
      stop("'time' must be a vector or a 1-D array.")
    time <- as.vector(time)
  }

  if (anyNA(time))
    stop("NAs not allowed in 'time'.")

  stopifnot(
    identical(length(time), length(y)),
    identical(length(time), length(unique(time)))
  )

  unname(time)
}
