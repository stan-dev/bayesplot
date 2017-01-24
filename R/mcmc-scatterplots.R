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
mcmc_pairs <- function(x,
                       pars = character(),
                       regex_pars = character(),
                       transformations = list(),
                       # facet_args = list(),
                       ...,
                       condition = NULL) {
  check_ignored_arguments(...)
  x <- prepare_mcmc_array(x, pars, regex_pars, transformations)
  pars <- parameter_names(x)
  n_param <- length(pars)
  n_chain <- ncol(x)
  mid <- floor(n_chain / 2)
  chain_groups <- list(upper = 1:mid, lower = (mid + 1):n_chain)
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
      plots[[j]] <- mcmc_hist(x, pars = pair[1])
    } else {
      idx <- arrayInd(j_lookup[j], .dim = c(n_param, n_param))
      keep_chains <- if (row_match_found(idx, lower_tri))
        chain_groups[[2]] else chain_groups[[1]]
      plots[[j]] <- mcmc_scatter(x[,keep_chains,, drop=FALSE],
                                 pars = pair, size = ggplot2::rel(.5))
    }
  }
  plots <- lapply(plots, function(x)
    x + xaxis_title(FALSE) + yaxis_title(FALSE))

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



apply_condition <- function(x, cond, nuts_params) {
  pars <- parameter_names(x)
  n_param <- length(pars)
  n_plot <- n_param^2
  n_chain <- ncol(x)

  lower_tri <- lower_tri_idx(n_param)
  j_lookup <- matrix(seq_len(n_plot), nrow = n_param, byrow = TRUE)
  idx <- arrayInd(j_lookup[j], .dim = c(n_param, n_param))

  is_lower <- row_match_found(idx, lower_tri)
  if (is.null(cond)) {
    mid_chain <- floor(n_chain / 2)
    keep_chains <- if (is_lower)
      seq_len(mid_chain) else seq(mid_chain + 1, n_chain)
    return(x[, keep_chains,, drop=FALSE])
  } else {
    if (cond == "accept_stat__") {

    } else if (cond == "divergent__") {

    } else if (cond == "energy__") {

    } else if (cond == "lp__") {

    }
  }
}
