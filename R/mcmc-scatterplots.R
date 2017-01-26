#' Scatterplots of MCMC draws
#'
#' Scatterplots of MCMC draws. See the \strong{Plot Descriptions} section,
#' below, for details.
#'
#' @name MCMC-scatterplots
#' @family MCMC
#'
#' @template args-mcmc-x
#' @param pars An optional character vector of parameter names. (\strong{Note}:
#' for \code{mcmc_scatter} only two parameters can be selected.)
#' @template args-regex_pars
#' @template args-transformations
#' @param ... Currently ignored.
#' @param size,alpha Passed to \code{\link[ggplot2]{geom_point}}.
#'
#' @return A ggplot object that can be further customized using the
#'   \pkg{ggplot2} package.
#'
#' @section Plot Descriptions:
#' \describe{
#'   \item{\code{mcmc_scatter}}{
#'    Bivariate scatterplot of posterior draws (for two parameters).
#'   }
#'   \item{\code{mcmc_pairs}}{
#'   Coming soon.
#'   }
#' }
#'
#' @examples
#' # some parameter draws to use for demonstration
#' x <- example_mcmc_draws(params = 6)
#' dimnames(x)
#'
#' # scatterplot of alpha vs log(sigma)
#' color_scheme_set("teal")
#' p <- mcmc_scatter(x, pars = c("alpha", "sigma"),
#'                   trans = list(sigma = "log"),
#'                   alpha = 0.5)
#' p + ggplot2::labs(caption = "A fascinating caption")
#'
#' # add ellipse
#' p + ggplot2::stat_ellipse(level = 0.9, color = "gray20", size = 1)
#'
#' # add contour
#' color_scheme_set("red")
#' p2 <- mcmc_scatter(x, pars = c("alpha", "sigma"))
#' p2 + ggplot2::stat_density_2d(color = "black")
#'
#' # can also add lines/smooths
#' color_scheme_set("pink")
#' (p3 <- mcmc_scatter(x, pars = c("alpha", "beta[3]"), alpha = 0.5, size = 3))
#' p3 + ggplot2::geom_smooth(method = "lm", se = FALSE, color = "gray20")
#'
#'
#'
NULL

#' @rdname MCMC-scatterplots
#' @export
mcmc_scatter <- function(x,
                         pars = character(),
                         regex_pars = character(),
                         transformations = list(),
                         ...,
                         size = 2.5,
                         alpha = 0.8) {
  check_ignored_arguments(...)
  x <- prepare_mcmc_array(x, pars, regex_pars, transformations)
  if (dim(x)[3] != 2)
    stop(
      "For 'mcmc_scatter' exactly 2 parameters must be selected. ",
      "'mcmc_pairs' for more than 2 parameters will be available ",
      "in a future release."
    )

  x <- merge_chains(x)
  parnames <- colnames(x)[1:2]
  ggplot(
    data = data.frame(x = c(x[, 1]), y = c(x[, 2])),
    mapping = aes_(x = ~ x, y = ~ y)
  ) +
    geom_point(
      shape = 21,
      color = get_color("dh"),
      fill = get_color("d"),
      size = size,
      alpha = alpha
    ) +
    labs(x = parnames[1], y = parnames[2]) +
    theme_default()
}

#' @rdname MCMC-scatterplots
#' @export
#' @param np A molten data frame of NUTS sampler parameters, either created by
#'   \code{\link{nuts_params}} or in the same form as the object returned by
#'   \code{\link{nuts_params}}.
#' @param lp A molten data frame of draws of the log-posterior or, more
#'   commonly, of a quantity equal to the log-posterior up to a constant.
#'   \code{lp} should either be created via \code{\link{log_posterior}} or be an
#'   object with the same form as the object returned by
#'   \code{\link{log_posterior}}.
#' @param condition There are many options for the \code{condition} argument:
#' \itemize{
#'   \item A (possibly abbreviated) character vector of length one can be passed
#'   among \code{"accept_stat__"}, \code{"stepsize__"}, \code{"treedepth__"},
#'   \code{"n_leapfrog__"}, \code{"divergent__"}, \code{"energy__"}, or
#'   \code{"lp__"}, which are the sampler parameters associated with
#'   \code{\link{NUTS}} (and \code{"lp__"} is the log-posterior up to an
#'   additive constant). In this case the lower panel will plot realizations
#'   that are below the median of the indicated variable (or are zero in the
#'   case of \code{"divergent__"}) and the upper panel will plot realizations
#'   that are greater than or equal to the median of the indicated variable (or
#'   are one in the case of \code{"divergent__"}). \strong{The default is}
#'   \code{"accept_stat__"}.
#'   \item \code{NULL}, in which case it will plot roughly half of the chains in
#'   the lower panel and the rest in the upper panel.
#'   \item An integer vector can be passed to select some subset of the chains,
#'   of which roughly half will be plotted in the lower panel and the rest in
#'   the upper panel.
#'   \item A list of two integer vectors can be passed, each specifying a subset
#'   of the chains to be plotted in the lower and upper panels respectively.
#'   \item A single number between zero and one exclusive can be passed, which
#'   is interpreted as the proportion of realizations (among all chains) to plot
#'   in the lower panel starting with the first realization in each chain, with
#'   the complement (from the end of each chain) plotted in the upper panel.
#'   \item Finally, any logical
#'   vector whose length is equal to the product of the number of iterations and
#'   the number of chains can be passed, in which case realizations
#'   corresponding to FALSE and TRUE will be plotted in the lower and upper
#'   panel respectively.
#'  }
#' @param max_td For models fit using NUTS, the maximum treedepth allowed when
#'   fitting the model. Defaults to \code{10}. Yellow points will be plotted to
#'   indicate a transition that hit the maximum treedepth rather than terminated
#'   its evolution normally.
#' @param binwidth For \code{mcmc_pairs}, an optional numeric value to override
#'   the default binwidth used for the histograms along the diagonal of the plot
#'   matrix.
#'
#' @details If \code{condition} is not \code{"divergent__"}, red points will be
#' superimposed onto the smoothed density plots indicating which (if any)
#' iterations encountered a divergent transition. Otherwise, yellow points
#' indicate a transition that hit the maximum treedepth rather than terminated
#' its evolution normally.
#'
#' Draws from the warmup phase should always be discarded before calling
#' \code{mcmc_pairs}.
#'
mcmc_pairs <- function(x, np, lp,
                       pars = character(),
                       regex_pars = character(),
                       transformations = list(),
                       # facet_args = list(),
                       ...,
                       condition = "accept_stat__", max_td = 10, size = 1, alpha = 0.8, binwidth = NULL) {
  check_ignored_arguments(...)
  x <- prepare_mcmc_array(x, pars, regex_pars, transformations)
  x <- drop_constants_and_duplicates(x)
  pars <- parameter_names(x)
  n_param <- length(pars)
  n_chain <- ncol(x)
  n_iter <- nrow(x)

  np <- validate_nuts_data_frame(np, lp)
  divs <- filter_(np, ~ Parameter == "divergent__")$Value
  divergent__ <- matrix(divs, nrow = n_iter * n_chain, ncol = n_param)

  gt_max_td <- filter_(np, ~ Parameter == "treedepth__")$Value > max_td
  max_td_hit <- matrix(gt_max_td, nrow = n_iter * n_chain, ncol = n_param)


  if (is.list(condition)) {
    if (length(condition) != 2)
      stop("If a list, 'condition' must be of length 2.")
    x <- x[, c(condition[[1]], condition[[2]]), , drop = FALSE]
    k <- length(condition[[1]])
    mark <- c(rep(TRUE, n_iter * k), rep(FALSE, n_iter * length(condition[[2]])))

  } else if (is.logical(condition)) {

    stopifnot(length(condition) == (n_iter * n_chain))
    mark <- !condition

  } else if (is.character(condition)) {

    condition <- match.arg(condition, several.ok = FALSE,
                           choices = c("accept_stat__", "stepsize__",
                                       "treedepth__", "n_leapfrog__",
                                       "divergent__", "energy__", "lp__"))
    if (condition == "lp__") {
      mark <- as.matrix(unstack(lp, form = Value ~ Chain))
    } else {
      mark <- filter_(np, ~ Parameter == condition)
      mark <- as.matrix(unstack(mark, form = Value ~ Chain))
    }
    if (condition == "divergent__") {
      mark <- as.logical(mark)
    } else {
      mark <- c(mark) >= median(mark)
    }
    if (length(unique(mark)) == 1)
      stop(condition, " is constant so it cannot be used as a condition.")

  } else if (!is.null(condition)) {

    if (all(condition == as.integer(condition))) {
      x <- x[, condition, , drop = FALSE]
      k <- ncol(x) %/% 2
      mark <- c(rep(FALSE, n_iter * k), rep(TRUE, n_iter * (n_chain - k)))
    } else if (condition > 0 && condition < 1) {
      mark <- rep(1:n_iter > (condition * n_iter), times = n_chain)
    } else {
      stop("If numeric, 'condition' must be an integer (vector) ",
           "or a number between 0 and 1 (exclusive).")
    }

  } else {

    k <- ncol(x) %/% 2
    mark <- c(rep(FALSE, n_iter * k), rep(TRUE, n_iter * (n_chain - k)))
  }

  x <- merge_chains(x)

  # if (isTRUE(log)) {
  #   xl <- apply(x >= 0, 2, FUN = all)
  #   sel <- c("log-posterior", "lp__") %in% names(xl)
  #   if (all(sel)) {
  #     stop("log-posterior and lp__ both included in x.")
  #   } else if (sel[1]) {
  #     xl["log-posterior"] <- FALSE
  #   } else if (sel[2]) {
  #     xl["lp__"] <- FALSE
  #   }
  #
  # } else if (is.numeric(log)) {
  #   xl <- log
  # } else {
  #   xl <- grepl("x", log)
  # }
  # if (is.numeric(xl) || any(xl)) {
  #   x[, xl] <- log(x[, xl])
  #   colnames(x)[xl] <- paste("log", colnames(x)[xl], sep = "-")
  # }

  mid <- floor(n_chain / 2)
  all_pairs <- expand.grid(pars, pars,
                           stringsAsFactors = FALSE,
                           KEEP.OUT.ATTRS = FALSE)

  n_plot <- n_param^2
  plots <- vector("list", length = n_plot)

  lower_tri <- lower_tri_idx(n_param)
  j_lookup <- matrix(seq_len(n_plot), nrow = n_param, byrow = TRUE)

  for (j in seq_len(nrow(all_pairs))) {
    pair <- as.character(all_pairs[j, ])
    if (identical(pair[1], pair[2])) {
      plots[[j]] <- mcmc_hist(x, pars = pair[1], binwidth = binwidth)
    } else {
      x_j <- x[, pair]
      idx <- arrayInd(j_lookup[j], .dim = c(n_param, n_param))
      if (row_match_found(idx, lower_tri)) {
        x_j <- x_j[!mark,, drop=FALSE]
        divs_j <- divergent__[!mark, 1]
        max_td_hit_j <- max_td_hit[!mark, 1]
      } else {
        x_j <- x_j[mark,, drop=FALSE]
        divs_j <- divergent__[mark, 1]
        max_td_hit_j <- max_td_hit[mark, 1]
      }




      plots[[j]] <- mcmc_scatter(x_j, size = size, alpha = alpha)

      if (!identical(condition, "divergent__") &&
          any(divs_j == 1)) {
        divs_j_fac <- factor(as.logical(divs_j), levels = c(FALSE, TRUE), labels = c("NoDiv", "Div"))
        plots[[j]] <- plots[[j]] +
          geom_point(aes_(color = divs_j_fac, fill = divs_j_fac, size = divs_j_fac),
                     shape = 4, size = size, #alpha = alpha,
                     na.rm = TRUE)
      }
      if (any(max_td_hit_j == 1)) {
        max_td_hit_j_fac <- factor(max_td_hit_j, levels = c(FALSE, TRUE), labels = c("NoHit", "Hit"))
        plots[[j]] <- plots[[j]] +
          geom_point(aes_(color = max_td_hit_j_fac, fill = max_td_hit_j_fac, size = max_td_hit_j_fac),
                     shape = 3, size = size, #alpha = alpha,
                     na.rm = TRUE)
      }

      if (any(divs_j == 1) || any(max_td_hit_j == 1)) {
        plots[[j]] <- plots[[j]] +
          scale_color_manual(
            values = setNames(c(NA, "red", NA, "yellow2"),
                              c("NoDiv", "Div", "NoHit", "Hit"))
          ) +
          scale_size_manual(
            values = setNames(c(0, 2, 0, 2), c("NoDiv", "Div", "NoHit", "Hit"))
          )
      }

    }

  }
  plots <- lapply(plots, function(x)
    x + xaxis_title(FALSE) + yaxis_title(FALSE) + legend_none())

  bayesplot_grid(plots = plots)
}


# internal ----------------------------------------------------------------

# Get indices of lower triangular elements
#
# @param n number of rows (columns) in the square matrix
lower_tri_idx <- function(n) {
  a <- rev(abs(sequence(seq.int(n - 1)) - n) + 1)
  b <- rep.int(seq.int(n - 1), rev(seq.int(n - 1)))
  cbind(row = a, col = b)
}

# Find which (if any) row in x is a match for y
#
# @param x a row vector (i.e., a matrix with 1 row)
# @param y a matrix
# @return either a row number in y or NA if no match
row_match_found <- function(x, y) {
  stopifnot(
    is.matrix(x),
    is.matrix(y),
    nrow(x) == 1
  )
  x <- as.data.frame(x)
  y <- as.data.frame(y)
  res <- match(
    do.call(function(...) paste(..., sep=":::"), x),
    do.call(function(...) paste(..., sep=":::"), y)
  )
  isTRUE(!is.na(res) && length(res == 1))
}


# Drop any constant or duplicate variables
# @param x 3-D array
drop_constants_and_duplicates <- function(x) {
  x1 <- drop_consts(x)
  drop_dupes(x1)
}
drop_consts <- function(x) {
  varying <- apply(x, 3, FUN = function(y) length(unique(c(y))) > 1)
  if (all(varying))
    return(x)

  message(
    "The following parameters were dropped because they are constant:\n",
    paste(names(varying)[!varying], collapse = ", ")
  )
  x[, , varying, drop = FALSE]
}
drop_dupes <- function(x) {
  dupes <- duplicated(x, MARGIN = 3)
  if (!any(dupes))
    return(x)

  message(
    "The following parameters were dropped because they are duplicative:\n",
    paste(parameter_names(x)[dupes], collapse = ", ")
  )
  x[, , !dupes, drop = FALSE]
}

