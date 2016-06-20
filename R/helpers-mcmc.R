# Check if an object is a data.frame with a chain index column
#
# @param x object to check
# @return TRUE or FALSE
is_df_with_chain <- function(x) {
  is.data.frame(x) && any(tolower(colnames(x)) %in% "chain")
}

validate_df_with_chain <- function(x) {
  stopifnot(is.data.frame(x))
  if (!is.null(x$chain)) {
    if (is.null(x$Chain))
      x$Chain <- x$chain
    x$chain <- NULL
  }
  x$Chain <- as.integer(x$Chain)
  x
}

# Convert data.frame with Chain variable to a 3-D array
df_with_chain2array <- function(x) {
  stopifnot(is_df_with_chain(x))
  chain <- x$Chain
  n_chain <- length(unique(chain))
  a <- x[, !colnames(x) %in% "Chain", drop = FALSE]
  parnames <- colnames(a)
  a <- as.matrix(a)
  x <- array(NA, dim = c(ceiling(nrow(a) / n_chain), n_chain, ncol(a)))
  for (j in seq_len(n_chain))
    x[, j, ] <- a[chain == j,, drop=FALSE]

  set_mcmc_dimnames(x, parnames)
}

# Set dimnames of 3-D array
set_mcmc_dimnames <- function(x, parnames) {
  stopifnot(is.array(x) && length(dim(x)) == 3)
  structure(x,
            dimnames = list(
              Iteration = seq_len(nrow(x)),
              Chain = seq_len(ncol(x)),
              Parameter = parnames
            ))
}

# Check if an object is a 3-D array with the correct dimension names
is_mcmc_array <- function(x) {
  if (!is.array(x))
    return(FALSE)
  if (length(dim(x)) != 3)
    return(FALSE)
  if (!identical(names(dimnames(x)), c("Iteration", "Chain", "Parameter")))
    return(FALSE)

  TRUE
}

# Check if 3-D array has multiple chains
has_multiple_chains <- function(x) {
  stopifnot(is_mcmc_array(x))
  isTRUE(dim(x)[2] > 1)
}
# Check if 3-D array has multiple parameters
has_multiple_params <- function(x) {
  stopifnot(is_mcmc_array(x))
  isTRUE(dim(x)[3] > 1)
}

STOP_need_multiple_chains <- function(call. = FALSE) {
  stop("This function requires multiple chains.", call. = call.)
}
STOP_need_multiple_params <- function(call. = FALSE) {
  stop("This function requires multiple parameters", call. = call.)
}


# Prepare 3-D array for MCMC plots
#
# @param x User's 'x' input to one of the mcmc_* functions.
# @return A 3-D (Iterations x Chains x Parameters) array.
#
prepare_mcmc_array <-
  function(x,
           pars = character(),
           regex_pars = character(),
           transformations = list()) {

    if (is_df_with_chain(x)) {
      x <- validate_df_with_chain(x)
      x <- df_with_chain2array(x)
    }
    x <- validate_mcmc_x(x)

    parnames <- if (is.matrix(x)) {
      colnames(x)
    } else if (is.array(x)) {
      dimnames(x)[[3]]
    }
    if (is.null(parnames))
      stop("No parameter names found.")

    pars <-
      select_parameters(explicit = pars,
                        patterns = regex_pars,
                        complete = parnames)

    if (is.matrix(x)) {
      x <- x[, pars, drop=FALSE]
      if (length(transformations))
        x <- apply_transformations(x, transformations)
      x <- array(x, dim = c(nrow(x), 1, ncol(x)))
    } else {
      x <- x[, , pars, drop = FALSE]
      if (length(transformations))
        x <- apply_transformations(x, transformations)
    }

    set_mcmc_dimnames(x, pars)
  }

# Convert 3-D array to matrix with chains merged
#
# @param x A 3-D array (iter x chain x param)
# @return A matrix with one column per parameter
#
merge_chains <- function(x) {
  xdim <- dim(x)
  mat <- array(x, dim = c(prod(xdim[1:2]), xdim[3]))
  colnames(mat) <- dimnames(x)[[3]]
  mat
}

# Perform some checks on user's 'x' input for MCMC plots
#
# @param x User's 'x' input to one of the mcmc_* functions.
# @return x, unless an error is thrown.
#
validate_mcmc_x <- function(x) {
  stopifnot(!is_df_with_chain(x))
  if (is.data.frame(x))
    x <- as.matrix(x)

  stopifnot(is.matrix(x) || is.array(x))
  if (anyNA(x))
    stop("NAs not allowed in 'x'.")

  x
}


# Validate that transformations match parameter names
validate_transformations <-
  function(transformations = list(),
           pars = character()) {
    transformations <- lapply(transformations, match.fun)
    if (!all(names(transformations) %in% pars)) {
      not_found <- which(!names(transformations) %in% pars)
      stop(
        "Some names(transformations) don't match parameter names: ",
        paste(names(transformations)[not_found], collapse = ", ")
      )
    }
  }


# Apply transformations to matrix or 3-D array of parameter draws
#
# @param x A matrix or 3-D array of draws
# @param transformation User's 'transformations' argument to one of the mcmc_*
#   functions.
# @return x, with tranformations having been applied to some parameters.
#
apply_transformations <- function(x, transformations = list(), ...) {
  UseMethod("apply_transformations")
}
apply_transformations.matrix <- function(x, transformations = list()) {
  pars <- colnames(x)
  x_transforms <- validate_transformations(transformations, pars)
  for (p in names(x_transforms))
    x[, p] <- x_transforms[[p]](x[, p])

  x
}
apply_transformations.array <- function(x, transformations = list()) {
  stopifnot(length(dim(x)) == 3)
  pars <- dimnames(x)[[3]]
  x_transforms <- validate_transformations(transformations, pars)
  for (p in names(x_transforms))
    x[, , p] <- x_transforms[[p]](x[, , p])

  x
}

# Convert numeric vector of Rhat values to a factor
#
# @param x A numeric vector
# @param breaks A numeric vector of length two. The resulting factor variable
#   will have three levels ('low', 'ok', and 'high') corresponding to (x <=
#   breaks[1], breaks[1] < x <= breaks[2], x > breaks[2]).
# @return A factor the same length as x with three levels.
#
factor_rhat <- function(rhat, breaks = c(1.05, 1.1)) {
  stopifnot(is.numeric(rhat),
            isTRUE(all(rhat > 0)),
            length(breaks) == 2)
  cut(
    rhat,
    breaks = c(-Inf, breaks, Inf),
    labels = c("low", "ok", "high"),
    ordered_result = TRUE
  )
}

# Functions wrapping around scale_color_manual and scale_fill_manual, used to
# color the intervals by rhat value
scale_color_rhat <- function() rhat_color_scale("color")
scale_fill_rhat <- function() rhat_color_scale("fill")
rhat_color_scale <- function(aesthetic = c("color", "fill")) {
  aesthetic <- match.arg(aesthetic)
  color_levels <- c("light", "mid", "dark")
  if (aesthetic == "color")
    color_levels <- paste0(color_levels, "_highlight")

  do.call(
    match.fun(paste0("scale_", aesthetic, "_manual")),
    list(
      name = NULL,
      drop = FALSE,
      values = setNames(get_color(color_levels), c("low", "ok", "high")),
      labels = c(expression(hat(R) <= 1.05),
                 expression(hat(R) <= 1.10),
                 expression(hat(R) > 1.10))
    )
  )
}
