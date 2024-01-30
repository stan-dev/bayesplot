#' Prepare 3-D array for MCMC plots
#'
#' @noRd
#' @param x,pars,regex_pars,transformations Users's arguments to one of the
#'   mcmc_* functions.
#' @return A 3-D (Iterations x Chains x Parameters) array.
#'
prepare_mcmc_array <- function(x,
                               pars = character(),
                               regex_pars = character(),
                               transformations = list()) {
  if (posterior::is_draws(x)) {
    x <- posterior::as_draws_array(x)
  } else if (is_df_with_chain(x)) {
    x <- df_with_chain2array(x)
  } else if (is_chain_list(x)) {
    # this will apply to mcmc.list and similar objects
    x <- chain_list2array(x)
  } else if (is.data.frame(x)) {
    # data frame without Chain column
    x <- as.matrix(x)
  } else {
    # try object's as.array method
    x <- as.array(x)
  }

  stopifnot(is.matrix(x) || is.array(x))
  if (is.array(x) && !(length(dim(x)) %in% c(2,3))) {
    abort("Arrays should have 2 or 3 dimensions. See help('MCMC-overview').")
  }
  if (anyNA(x)) {
    abort("NAs not allowed in 'x'.")
  }

  if (rlang::is_quosures(pars)) {
    pars <- tidyselect_parameters(complete_pars = parameter_names(x),
                                  pars_list = pars)
  } else {
    pars <- select_parameters(complete_pars = parameter_names(x),
                              explicit = pars,
                              patterns = regex_pars)
  }

  # possibly recycle transformations (apply same to all pars)
  if (is.function(transformations) ||
      (is.character(transformations) && length(transformations) == 1)) {
    transformations <- rep(list(transformations), length(pars))
    transformations <- set_names(transformations, pars)
  }

  if (is.matrix(x)) {
    x <- x[, pars, drop=FALSE]
    if (length(transformations)) {
      x <- apply_transformations(x, transformations = transformations)
    }
    x <- array(x, dim = c(nrow(x), 1, ncol(x)))
  } else {
    x <- x[, , pars, drop = FALSE]
    if (length(transformations)) {
      x <- apply_transformations(x, transformations = transformations)
    }
  }

  pars <- rename_transformed_pars(pars, transformations)
  set_mcmc_dimnames(x, pars)
}


#' Explicit and/or regex parameter selection
#'
#' @noRd
#' @param explicit Character vector of selected parameter names.
#' @param patterns Character vector of regular expressions.
#' @param complete_pars Character vector of all possible parameter names.
#' @return Character vector of combined explicit and matched (via regex)
#'   parameter names, unless an error is thrown.
#'
select_parameters <-
  function(explicit = character(),
           patterns = character(),
           complete_pars = character()) {

    stopifnot(is.character(explicit),
              is.character(patterns),
              is.character(complete_pars))

    if (!length(explicit) && !length(patterns)) {
      return(complete_pars)
    }

    if (length(explicit)) {
      if (!all(explicit %in% complete_pars)) {
        not_found <- which(!explicit %in% complete_pars)
        abort(paste(
          "Some 'pars' don't match parameter names:",
          paste(explicit[not_found], collapse = ", "),
          call. = FALSE
        ))
      }
    }

    if (!length(patterns)) {
      return(unique(explicit))
    } else {
      regex_pars <-
        unlist(lapply(seq_along(patterns), function(j) {
          grep(patterns[j], complete_pars, value = TRUE)
        }))

      if (!length(regex_pars)) {
        abort("No matches for 'regex_pars'.")
      }
    }

    unique(c(explicit, regex_pars))
  }


#' Melt a 3-D array or matrix of MCMC draws
#'
#' @noRd
#' @param x An mcmc_array (from prepare_mcmc_array).
#' @param varnames,value.name,... Passed to reshape2::melt (array method).
#' @return A molten data frame.
#'
melt_mcmc <- function(x, ...) UseMethod("melt_mcmc")

#' @export
melt_mcmc.mcmc_array <- function(x,
                                 varnames =
                                   c("Iteration", "Chain", "Parameter"),
                                 value.name = "Value",
                                 as.is = TRUE,
                                 ...) {
  stopifnot(is_mcmc_array(x))

  long <- reshape2::melt(
    data = x,
    varnames = varnames,
    value.name = value.name,
    as.is = FALSE,
    ...)

  long$Parameter <- factor(long$Parameter)
  long
}

# If all chains are already merged
#' @export
melt_mcmc.matrix <- function(x,
                             varnames = c("Draw", "Parameter"),
                             value.name = "Value",
                             ...) {
  long <- reshape2::melt(
    data = x,
    varnames = varnames,
    value.name = value.name,
    as.is = FALSE,
    ...)

  long$Parameter <- factor(long$Parameter)
  long
}

#' Set dimnames of 3-D array
#' @noRd
#' @param x 3-D array
#' @param parnames Character vector of parameter names
#' @return x with a modified dimnames.
set_mcmc_dimnames <- function(x, parnames) {
  stopifnot(is_3d_array(x))
  dimnames(x) <- list(
    Iteration = seq_len(nrow(x)),
    Chain = seq_len(ncol(x)),
    Parameter = parnames
  )
  structure(x, class = c(class(x), "mcmc_array"))
}

#' Convert 3-D array to matrix with chains merged
#'
#' @noRd
#' @param x A 3-D array (iter x chain x param)
#' @return A matrix with one column per parameter
#'
merge_chains <- function(x) {
  xdim <- dim(x)
  mat <- array(x, dim = c(prod(xdim[1:2]), xdim[3]))
  colnames(mat) <- parameter_names(x)
  mat
}


#' Check if an object is a data.frame with a chain index column
#'
#' @noRd
#' @param x object to check
#' @return TRUE or FALSE
is_df_with_chain <- function(x) {
  is.data.frame(x) && any(tolower(colnames(x)) %in% "chain")
}

validate_df_with_chain <- function(x) {
  stopifnot(is_df_with_chain(x))
  x <- as.data.frame(x)
  if (!is.null(x$chain)) {
    if (is.null(x$Chain)) {
      x$Chain <- x$chain
    }
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
  for (j in seq_len(n_chain)) {
    x[, j, ] <- a[chain == j,, drop=FALSE]
  }

  set_mcmc_dimnames(x, parnames)
}


#' Check if an object is a list (but not a data.frame) that contains
#' all 2-D objects
#' @noRd
#' @param x object to check
#' @return TRUE or FALSE
is_chain_list <- function(x) {
  check1 <- !is.data.frame(x) && is.list(x)
  dims <- try(sapply(x, function(chain) length(dim(chain))), silent=TRUE)
  if (inherits(dims, "try-error")) {
    return(FALSE)
  }
  check2 <- isTRUE(all(dims == 2)) # all elements of list should be matrices/2-D arrays
  check1 && check2
}

validate_chain_list <- function(x) {
  n_chain <- length(x)
  for (i in seq_len(n_chain)) {
    nms <- colnames(as.matrix(x[[i]]))
    if (is.null(nms) || !all(nzchar(nms))) {
      abort(paste(
        "Some parameters are missing names.",
        "Check the column names for the matrices in your list of chains."
      ))
    }
  }
  if (n_chain > 1) {
    n_iter <- sapply(x, nrow)
    same_iters <- length(unique(n_iter)) == 1
    if (!same_iters) {
      abort("Each chain should have the same number of iterations.")
    }

    cnames <- sapply(x, colnames)
    if (is.array(cnames)) {
      same_params <- identical(cnames[, 1], cnames[, 2])
    } else {
      same_params <- length(unique(cnames)) == 1
    }
    if (!same_params) {
      abort(paste(
        "The parameters for each chain should be in the same order",
        "and have the same names."
      ))
    }
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
  for (i in seq_len(n_chain)) {
    out[, i,] <- x[[i]]
  }

  set_mcmc_dimnames(out, param_names)
}


# Get parameter names from a 3-D array
parameter_names <- function(x) UseMethod("parameter_names")

#' @export
parameter_names.array <- function(x) {
  stopifnot(is_3d_array(x))
  dimnames(x)[[3]] %||% abort("No parameter names found.")
}
#' @export
parameter_names.default <- function(x) {
  colnames(x) %||% abort("No parameter names found.")
}
#' @export
parameter_names.matrix <- function(x) {
  colnames(x) %||% abort("No parameter names found.")
}

# Check if an object is a 3-D array
is_3d_array <- function(x) {
  if (!is.array(x)) {
    return(FALSE)
  }

  if (length(dim(x)) != 3) {
    return(FALSE)
  }

  TRUE
}
# Check if an object is a 3-D array AND has correct dimension names
is_mcmc_array <- function(x) {
  if (!is_3d_array(x)) {
    return(FALSE)
  }

  if (!identical(names(dimnames(x)), c("Iteration", "Chain", "Parameter"))) {
    return(FALSE)
  }

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
  abort("This function requires multiple chains.")
}


# Validate that transformations match parameter names
validate_transformations <-
  function(transformations = list(),
           pars = character()) {
    if (is.null(names(transformations))) {
      abort("'transformations' must be a _named_ list.")
    } else if (any(!nzchar(names(transformations)))) {
      abort("Each element of 'transformations' must have a name.")
    }

    transformations <- lapply(transformations, match.fun)
    if (!all(names(transformations) %in% pars)) {
      not_found <- which(!names(transformations) %in% pars)
      abort(paste(
        "Some names(transformations) don't match parameter names:",
        paste(names(transformations)[not_found], collapse = ", ")
      ))
    }

    transformations
  }


#' Apply transformations to matrix or 3-D array of parameter draws
#'
#' @noRd
#' @param x A matrix or 3-D array of draws
#' @param transformation User's 'transformations' argument to one of the mcmc_*
#'   functions.
#' @return x, with tranformations having been applied to some parameters.
#'
apply_transformations <- function(x, ...) {
  UseMethod("apply_transformations")
}

#' @export
apply_transformations.matrix <- function(x, ..., transformations = list()) {
  pars <- colnames(x)
  x_transforms <- validate_transformations(transformations, pars)
  for (p in names(x_transforms)) {
    x[, p] <- x_transforms[[p]](x[, p])
  }

  x
}

#' @export
apply_transformations.array <- function(x, ..., transformations = list()) {
  stopifnot(length(dim(x)) == 3)
  pars <- dimnames(x)[[3]]
  x_transforms <- validate_transformations(transformations, pars)
  for (p in names(x_transforms)) {
    x[, , p] <- x_transforms[[p]](x[, , p])
  }

  x
}

rename_transformed_pars <- function(pars, transformations) {
  stopifnot(is.character(pars), is.list(transformations))
  has_names <- sapply(transformations, is.character)
  if (any(has_names)) {
    nms <- names(which(has_names))
    for (nm in nms) {
      pars[which(pars == nm)] <-
        paste0(
          transformations[[nm]], "(",
          pars[which(pars == nm)], ")"
        )
    }
  }
  if (any(!has_names)) {
    nms <- names(which(!has_names))
    pars[pars %in% nms] <- paste0("t(", pars[pars %in% nms], ")")
  }

  pars
}


num_chains <- function(x, ...) UseMethod("num_chains")
num_iters <- function(x, ...) UseMethod("num_iters")
num_params <- function(x, ...) UseMethod("num_params")

#' @export
num_params.mcmc_array <- function(x, ...) dim(x)[3]
#' @export
num_chains.mcmc_array <- function(x, ...) dim(x)[2]
#' @export
num_iters.mcmc_array <- function(x, ...) dim(x)[1]
#' @export
num_params.data.frame <- function(x, ...) {
  stopifnot("Parameter" %in% colnames(x))
  length(unique(x$Parameter))
}
#' @export
num_chains.data.frame <- function(x, ...) {
  stopifnot("Chain" %in% colnames(x))
  length(unique(x$Chain))
}
#' @export
num_iters.data.frame <- function(x, ...) {
  cols <- colnames(x)
  stopifnot("Iteration" %in% cols || "Draws" %in% cols)

  if ("Iteration" %in% cols) {
    n <- length(unique(x$Iteration))
  } else {
    n <- length(unique(x$Draw))
  }

  n
}
