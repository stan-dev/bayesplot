# Check if an object is a data.frame with a chain index column
#
# @param x object to check
# @return TRUE or FALSE
is_df_with_chain <- function(x) {
  is.data.frame(x) && any(tolower(colnames(x)) %in% "chain")
}

validate_df_with_chain <- function(x) {
  stopifnot(is_df_with_chain(x))
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
  x <- validate_df_with_chain(x)
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


# Check if an object is a list but not a data.frame
#
# @param x object to check
# @return TRUE or FALSE
is_chain_list <- function(x) {
  !is.data.frame(x) && is.list(x)
}

validate_chain_list <- function(x) {
  stopifnot(is_chain_list(x))
  dims <- sapply(x, function(chain) length(dim(chain)))
  if (!isTRUE(all(dims == 2)))
    stop("If 'x' is a list then all elements must be matrices.")

  n_chain <- length(x)
  for (i in seq_len(n_chain)) {
    nms <- colnames(as.matrix(x[[i]]))
    if (is.null(nms) || !all(nzchar(nms)))
      stop(
        "Some parameters are missing names. ",
        "Check the column names for the matrices in your list of chains.",
        call. = FALSE
      )
  }
  if (n_chain > 1) {
    n_iter <- sapply(x, nrow)
    same_iters <- length(unique(n_iter)) == 1
    if (!same_iters)
      stop("Each chain should have the same number of iterations.")

    cnames <- sapply(x, colnames)
    if (is.array(cnames)) {
      same_params <- identical(cnames[, 1], cnames[, 2])
    } else {
      same_params <- length(unique(cnames)) == 1
    }
    if (!same_params)
      stop("The parameters for each chain should be in the same order ",
           "and have the same names.")
  }

  x
}

# Convert list of matrices to 3-D array
chain_list2array <- function(x) {
  x <- validate_chain_list(x)
  n_chain <- length(x)
  if (n_chain == 1) {
    n_iter <- nrow(x[[1]])
    param_names <- colnames(x[[1]])
  } else {
    n_iter <- sapply(x, nrow)
    cnames <- sapply(x, colnames)
    param_names <- if (is.array(cnames))
      cnames[, 1] else cnames
    n_iter <- n_iter[1]
  }
  param_names <- unique(param_names)
  n_param <- length(param_names)
  out <- array(NA, dim = c(n_iter, n_chain, n_param))
  for (i in seq_len(n_chain))
    out[, i,] <- x[[i]]

  set_mcmc_dimnames(out, param_names)
}

# Set dimnames of 3-D array
set_mcmc_dimnames <- function(x, parnames) {
  stopifnot(is_3d_array(x))
  structure(x,
            dimnames = list(
              Iteration = seq_len(nrow(x)),
              Chain = seq_len(ncol(x)),
              Parameter = parnames
            ))
}

# Get parameter names from a 3-D array
parameter_names <- function(x) {
  stopifnot(is_3d_array(x))
  if (is_mcmc_array(x))
    return(dimnames(x)[[3]])

  if (is.null(dimnames(x)[[3]]))
    stop("No parameter names found.")
  else
    dimnames(x)[[3]]
}


# Check if an object is a 3-D array
is_3d_array <- function(x) {
  if (!is.array(x))
    return(FALSE)
  if (length(dim(x)) != 3)
    return(FALSE)

  TRUE
}
# Check if an object is a 3-D array AND has correct dimension names
is_mcmc_array <- function(x) {
  if (!is_3d_array(x))
    return(FALSE)
  if (!identical(names(dimnames(x)), c("Iteration", "Chain", "Parameter")))
    return(FALSE)

  TRUE
}

# Check if 3-D array has multiple chains
has_multiple_chains <- function(x) {
  stopifnot(is_3d_array(x))
  isTRUE(dim(x)[2] > 1)
}
# Check if 3-D array has multiple parameters
has_multiple_params <- function(x) {
  stopifnot(is_3d_array(x))
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
      x <- df_with_chain2array(x)
    } else if (is_chain_list(x)) {
      x <- chain_list2array(x)
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

    pars <- rename_transformed_pars(pars, transformations)
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
  stopifnot(!is_df_with_chain(x), !is_chain_list(x))
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
    if (is.null(names(transformations)))
      stop("If specified, 'transformations' must be a _named_ list.")

    transformations <- lapply(transformations, match.fun)
    if (!all(names(transformations) %in% pars)) {
      not_found <- which(!names(transformations) %in% pars)
      stop(
        "Some names(transformations) don't match parameter names: ",
        paste(names(transformations)[not_found], collapse = ", ")
      )
    }
    return(transformations)
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

  return(x)
}
apply_transformations.array <- function(x, transformations = list()) {
  stopifnot(length(dim(x)) == 3)
  pars <- dimnames(x)[[3]]
  x_transforms <- validate_transformations(transformations, pars)
  for (p in names(x_transforms))
    x[, , p] <- x_transforms[[p]](x[, , p])

  return(x)
}

rename_transformed_pars <- function(pars, transformations) {
  stopifnot(is.character(pars), is.list(transformations))
  has_names <- sapply(transformations, is.character)
  if (any(has_names)) {
    nms <- names(which(has_names))
    for (nm in nms)
      pars[which(pars == nm)] <-
        paste0(
          transformations[[nm]], "(",
          pars[which(pars == nm)], ")"
        )
  }
  if (any(!has_names)) {
    nms <- names(which(!has_names))
    pars[pars %in% nms] <- paste0("t(", pars[pars %in% nms], ")")
  }
  return(pars)
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
