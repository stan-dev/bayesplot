#' Scatterplots of MCMC draws
#'
#' Scatterplots (and other similar plots) of MCMC draws. See the \strong{Plot
#' Descriptions} section, below, for details.
#'
#' @name MCMC-scatterplots
#' @family MCMC
#'
#' @template args-mcmc-x
#' @param pars An optional character vector of parameter names. For
#'   \code{mcmc_scatter} and \code{mcmc_hex} only two parameters can be
#'   selected. To plot more than two parameters use \code{mcmc_pairs}.
#' @template args-regex_pars
#' @template args-transformations
#' @param ... Currently ignored.
#' @param size,alpha For \code{mcmc_scatter}, passed to
#'   \code{\link[ggplot2]{geom_point}} to control the appearance of the points.
#' @param binwidth For \code{mcmc_hex}, an optional numeric vector of
#'   \emph{length two} passed to \code{\link[ggplot2]{geom_hex}} to override the
#'   default binwidth in both the vertical and horizontal directions.
#'
#' @return \code{mcmc_scatter} and \code{mcmc_hex} return a ggplot object that
#'   can be further customized using the \pkg{ggplot2} package.
#'   \code{mcmc_pairs} returns many ggplot objects organized into a grid using
#'   \code{\link{bayesplot_grid}}.
#'
#' @section Plot Descriptions:
#' \describe{
#'   \item{\code{mcmc_scatter}}{
#'    Bivariate scatterplot of posterior draws.
#'   }
#'   \item{\code{mcmc_hex}}{
#'    Hexagonal heatmap of 2-D bin counts. If \code{x} contains a very large
#'    number of posterior draws then this plot may be preferable to a
#'    scatterplot which can suffer from overplotting.
#'   }
#'   \item{\code{mcmc_pairs}}{
#'    Pairs plot with scatterplots (or hex plots) off the diagonal and
#'    histograms (or densities) along the diagonal. The default is to split the
#'    chains so that (roughly) half are displayed above the diagonal and half
#'    below (all chains are always merged for the plots along the diagonal).
#'    Many other possibilities are available by setting the \code{condition}
#'    argument. Additionally, extra diagonistic information for models fit using
#'    \code{\link{NUTS}} can be added to the pairs plot (see \strong{Examples}).
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
#' (p <- mcmc_scatter(x, pars = c("alpha", "sigma"),
#'                   transform = list(sigma = "log")))
#' p +
#'   ggplot2::labs(
#'     title = "Insert your own headline-grabbing title",
#'     subtitle = "with a provocative subtitle",
#'     caption = "and a controversial caption",
#'     x = expression(alpha),
#'     y = expression(log(sigma))
#'     )
#'
#' # add ellipse
#' p + ggplot2::stat_ellipse(level = 0.9, color = "gray20", size = 1)
#'
#' # add contour
#' color_scheme_set("red")
#' p2 <- mcmc_scatter(x, pars = c("alpha", "sigma"), size = 3.5, alpha = 0.25)
#' p2 + ggplot2::stat_density_2d(color = "black")
#'
#' # can also add lines/smooths
#' color_scheme_set("pink")
#' (p3 <- mcmc_scatter(x, pars = c("alpha", "beta[3]"), alpha = 0.25, size = 3))
#' p3 + ggplot2::geom_smooth(method = "lm", se = FALSE, color = "gray20",
#'                           size = .75, linetype = 2)
#'
#' \donttest{
#' # hexagonal heatmap
#' color_scheme_set("brightblue")
#' (p <- mcmc_hex(x, pars = c("sigma", "alpha"), transform = list(sigma = "log")))
#' p + plot_bg(fill = "gray95")
#' p + plot_bg(fill = "gray95") + panel_bg(fill = "gray70")
#' }
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
  .mcmc_scatter(
    x,
    pars = pars,
    regex_pars = regex_pars,
    transformations = transformations,
    size = size,
    alpha = alpha,
    hex = FALSE,
    binwidth = NULL
  )
}

#' @rdname MCMC-scatterplots
#' @export
#'
mcmc_hex <- function(x,
                     pars = character(),
                     regex_pars = character(),
                     transformations = list(),
                     ...,
                     binwidth = NULL) {
  check_ignored_arguments(...)
  .mcmc_scatter(
    x,
    pars = pars,
    regex_pars = regex_pars,
    transformations = transformations,
    hex = TRUE,
    binwidth = binwidth,
    size = NULL,
    alpha = NULL
  )
}

#' @rdname MCMC-scatterplots
#' @export
#' @param condition For \code{mcmc_pairs}, this argument is used to specify a
#'   criterion for determining which iterations (or chains) are shown in the
#'   plots above the diagonal and which are shown in the plots below the
#'   diagnoal. The histograms (or density plots) along the diagonal are always
#'   made using all chains and iterations, but the scatterplots (or hex plots)
#'   plots above and below the diagonal show different combinations of
#'   chains/iterations depending on the \code{condition} argument.
#'   There are many options for \code{condition}:
#' \itemize{
#'   \item \code{NULL} (the default), in which case half of the chains
#'   (or roughly half if there are an odd number) will be used in the
#'   plots above the diagonal and the rest in the plots below the diagonal.
#'   \item An integer vector that is used to select some subset of the chains
#'   (e.g., \code{c(1,3,4,7)} to select only chains 1, 3, 4, and 7). The
#'   behavior is the same as when \code{condition=NULL} except using only the
#'   specified subset of chains.
#'   \item A list of two integer vectors can be passed, each specifying a subset
#'   of the chains. In this case the chains specified in the first vector
#'   are shown in the plots above the diagonal and the chains specified in the
#'   second vector are shown in plots below the diagonal.
#'   \item A single number between zero and one (exclusive), which
#'   is interpreted as the proportion of realizations (among all chains) to plot
#'   in the lower panel starting with the first realization in each chain, with
#'   the complement (from the end of each chain) plotted in the upper panel.
#'   \item A \link{logical} vector with length equal to the product
#'   of the number of iterations and the number of chains, in
#'   which case realizations corresponding to \code{FALSE} and \code{TRUE} will
#'   be plotted in the lower and upper panels, respectively.
#'   \item For models fit using NUTS, a (possibly abbreviated) string to select
#'   among \code{"accept_stat__"}, \code{"stepsize__"}, \code{"treedepth__"},
#'   \code{"n_leapfrog__"}, \code{"divergent__"}, \code{"energy__"}, or
#'   \code{"lp__"}. These are the sampler parameters associated with
#'   \code{\link{NUTS}} (and \code{"lp__"} is the log-posterior up to an
#'   additive constant). In this case, plots below the diagonal will contain
#'   realizations below the median of the indicated variable (or are zero in the
#'   case of \code{"divergent__"}), and plots above the diagonal wil contain
#'   realizations that are greater than or equal to the median of the indicated
#'   variable (or are one in the case of \code{"divergent__"}). If
#'   \code{condition} is one of these NUTS diagnostics then the \code{np}
#'   argument must also be specified. If \code{condition} is \code{lp__} then
#'   the \code{lp} argument must also be specified.
#'  }
#' @param lp For \code{mcmc_pairs}, a molten data frame of draws of the
#'   log-posterior or, more commonly, of a quantity equal to the log-posterior
#'   up to a constant. \code{lp} should either be created via
#'   \code{\link{log_posterior}} or be an object with the same form as the
#'   object returned by \code{\link{log_posterior}}.
#' @param np For \code{mcmc_pairs}, a molten data frame of NUTS sampler
#'   parameters, either created by \code{\link{nuts_params}} or in the same form
#'   as the object returned by \code{\link{nuts_params}}. If \code{np} is
#'   specified (and \code{condition} is \emph{not} \code{"divergent__"}), then
#'   points (red, by default) will be superimposed onto the off-diagonal plots
#'   indicating which (if any) iterations encountered a divergent transition.
#'   Also, if both \code{np} and \code{max_treedepth} are specified then points
#'   (yellow, by default) will be superimposed to indicate a transition that hit
#'   the maximum treedepth rather than terminated its evolution normally.
#' @param np_style A named list of length one, two, or three, which is used to
#'   specify optional arguments controlling the appearance of superimposed
#'   points representing NUTS diagnostic warnings (if \code{np} is specified).
#'   The elements "color", "shape", and "size" can be specified (note: here,
#'   "size" is interpreted as a scaling factor). Each of the specified elements
#'   must be a vector of length two (the first element is used for a divergence
#'   and the second element for a transition hitting max treedepth). As an
#'   example, the default settings correspond to specifying the following:
#'
#'   \code{np_style = list(color = c("red", "yellow2"),
#'                         shape = c(4,3),
#'                         size = c(1,1))}
#'
#' @param max_treedepth For \code{mcmc_pairs}, an integer representing the
#'   maximum treedepth allowed when fitting the model (if fit using NUTS). This
#'   is only needed for detecting which transitions (if any) hit the maximum
#'   treedepth.
#' @param diag_fun,off_diag_fun For \code{mcmc_pairs}, the plotting function to
#'   use for the plots along the diagonal and for the off-diagonal plots,
#'   respectively. Currently \code{diag_fun} can be \code{"hist"} for histogram
#'   or \code{"dens"} for density, and \code{off_diag_fun} can be
#'   \code{"scatter"} for scatterplot or \code{"hex"} for a hexagonal heatmap.
#' @param diag_args,off_diag_args Optional named lists of arguments to pass to
#'   the functions implied by the \code{diag_fun} and \code{off_diag_fun}
#'   arguments, respectively. For example, if \code{off_diag_fun} is
#'   \code{"scatter"} then \code{off_diag_args} could include optional arguments
#'   to \code{mcmc_scatter} like \code{size} and \code{alpha}.
#'
#' @examples
#' \donttest{
#' color_scheme_set("purple")
#'
#' # pairs plots
#' # default of condition=NULL implies splitting chains between upper and lower panels
#' mcmc_pairs(x, pars = "alpha", regex_pars = "beta\\[[1,4]\\]",
#'            off_diag_args = list(size = 1, alpha = 0.5))
#'
#' # change to density plots instead of histograms and hex plots instead of
#' # scatterplots
#' mcmc_pairs(x, pars = "alpha", regex_pars = "beta\\[[1,4]\\]",
#'            diag_fun = "dens", off_diag_fun = "hex")
#'
#' # plot chain 1 separately from chains 2, 3, and 4
#' color_scheme_set("brightblue")
#' mcmc_pairs(x, pars = "alpha", regex_pars = "beta\\[[1,4]\\]",
#'            diag_fun = "dens", off_diag_fun = "hex",
#'            condition = list(1, 2:4))
#' }
#'
#' \dontrun{
#' # pairs plot with NUTS diagnostic info overlaid,
#' # example using rstanarm package
#' library(rstanarm)
#'
#' # for demonstration purposes, intentionally fit a model that
#' # will (almost certainly) have some divergences
#' fit <- stan_glm(
#'   mpg ~ ., data = mtcars,
#'   iter = 1000,
#'   # this combo of prior and adapt_delta should lead to some divergences
#'   prior = hs(),
#'   adapt_delta = 0.9
#' )
#' post <- as.array(fit)
#' np <- nuts_params(fit)
#'
#' # split the draws according to above/below median accept_stat__ and
#' # show approximate location of divergences (red points)
#' mcmc_pairs(
#'   post,
#'   pars = c("wt", "cyl", "sigma"),
#'   off_diag_args = list(size = 1, alpha = 0.5),
#'   condition = "accept_stat__",
#'   np = np
#' )
#'
#' # using median log-posterior as 'condition', hex instead of scatter for
#' # off-diagonal plots, and also indicating where max treedepth hit
#' color_scheme_set("darkgray")
#' mcmc_pairs(
#'   post,
#'   pars = c("wt", "cyl", "sigma"),
#'   off_diag_fun = "hex",
#'   condition = "lp__",
#'   lp = log_posterior(fit),
#'   np = np,
#'   np_style = list(color = c("firebrick", "dodgerblue"), size = c(1,2)),
#'   # for demonstration purposes, set max_treedepth to a value that will
#'   # result in at least a few warnings points (colored np_colors[2])
#'   max_treedepth = with(np, max(Value[Parameter == "treedepth__"]) - 1)
#' )
#' }
#'
mcmc_pairs <- function(x,
                       pars = character(),
                       regex_pars = character(),
                       transformations = list(),
                       ...,
                       diag_fun = c("hist", "dens"),
                       off_diag_fun = c("scatter", "hex"),
                       diag_args = list(),
                       off_diag_args = list(),
                       condition = NULL,
                       lp = NULL,
                       np = NULL,
                       np_style = list(),
                       max_treedepth = NULL) {
  check_ignored_arguments(...)
  x <- prepare_mcmc_array(x, pars, regex_pars, transformations)
  x <- drop_constants_and_duplicates(x)
  n_iter <- nrow(x)
  n_chain <- ncol(x)
  if (n_chain == 1)
    warning("Only one chain in 'x'. This plot is more useful with multiple chains.")

  pars <- parameter_names(x)
  n_param <- length(pars)
  if (n_param < 2)
    stop("This plot requires at least two parameters in 'x'.")

  stopifnot(is.list(diag_args), is.list(off_diag_args))
  plot_diagonal <- pairs_plotfun(match.arg(diag_fun))
  plot_off_diagonal <- pairs_plotfun(match.arg(off_diag_fun))

  no_np <- is.null(np)
  no_lp <- is.null(lp)
  no_max_td <- is.null(max_treedepth)
  np_args <- validate_np_style(np_style)

  if (!no_np) {
    np <- validate_nuts_data_frame(np, lp)
    divs <- filter_(np, ~ Parameter == "divergent__")$Value
    divergent__ <- matrix(divs, nrow = n_iter * n_chain, ncol = n_param)[, 1]

    if (!no_max_td) {
      gt_max_td <- filter_(np, ~ Parameter == "treedepth__")$Value > max_treedepth
      max_td_hit <- matrix(gt_max_td, nrow = n_iter * n_chain, ncol = n_param)[, 1]
    }
  }

  if (is.null(condition)) {

    k <- ncol(x) %/% 2
    mark <- c(rep(FALSE, n_iter * k), rep(TRUE, n_iter * (n_chain - k)))

  } else if (is.list(condition)) {
    # List of integer vectors
    if (length(condition) != 2)
      stop("If a list, 'condition' must be of length 2.")
    x <- x[, c(condition[[1]], condition[[2]]), , drop = FALSE]
    k1 <- length(condition[[1]])
    k2 <- length(condition[[2]])
    mark <- c(rep(TRUE, n_iter * k1), rep(FALSE, n_iter * k2))

  } else if (is.logical(condition)) {
    # T/F for each iteration to split into upper and lower
    stopifnot(length(condition) == (n_iter * n_chain))
    mark <- !condition

  } else if (is.character(condition)) {
    # NUTS sampler param or lp__
    condition <- match.arg(condition, several.ok = FALSE,
                           choices = c("accept_stat__", "stepsize__",
                                       "treedepth__", "n_leapfrog__",
                                       "divergent__", "energy__", "lp__"))
    if (no_np && !identical(condition, "lp__"))
      stop("To use this value of 'condition' the 'np' argument must also be specified.")

    if (condition == "lp__") {
      if (no_lp)
        stop("To use this value of 'condition' the 'lp' argument must also be specified.")
      mark <- unstack_to_matrix(lp, Value ~ Chain)
    } else {
      mark <- filter_(np, ~ Parameter == condition)
      mark <- unstack_to_matrix(mark, Value ~ Chain)
    }
    if (condition == "divergent__") {
      mark <- as.logical(mark)
    } else {
      mark <- c(mark) >= median(mark)
    }
    if (length(unique(mark)) == 1)
      stop(condition, " is constant so it cannot be used as a condition.")

  } else {
    # Numeric

    if (all(condition == as.integer(condition))) {
      # Integer vector
      x <- x[, condition, , drop = FALSE]
      k <- ncol(x) %/% 2
      mark <- c(rep(FALSE, n_iter * k), rep(TRUE, n_iter * (n_chain - k)))
    } else if (condition > 0 && condition < 1) {
      # Proportion
      mark <- rep(1:n_iter > (condition * n_iter), times = n_chain)

    } else {
      stop("If numeric, 'condition' must be an integer (vector) ",
           "or a number between 0 and 1 (exclusive).")
    }

  }

  all_pairs <- expand.grid(pars, pars,
                           stringsAsFactors = FALSE,
                           KEEP.OUT.ATTRS = FALSE)
  n_plot <- nrow(all_pairs)
  lower_tri <- lower_tri_idx(n_param)
  j_lookup <- matrix(seq_len(n_plot), nrow = n_param, byrow = TRUE)

  x <- merge_chains(x)
  plots <- vector("list", length = n_plot)
  for (j in seq_len(n_plot)) {
    pair <- as.character(all_pairs[j, ])

    if (identical(pair[1], pair[2])) {
      # Diagonal
      diag_args[["x"]] <- x[, pair[1], drop=FALSE]
      plots[[j]] <- do.call(plot_diagonal, diag_args)

    } else {
      # Off-diagonal
      x_j <- x[, pair]
      idx <- arrayInd(j_lookup[j], .dim = c(n_param, n_param))
      if (row_match_found(idx, lower_tri)) { # below diagonal
        x_j <- x_j[!mark,, drop=FALSE]
        if (!no_np) {
          divs_j <- divergent__[!mark]
          max_td_hit_j <- if (no_max_td) NULL else max_td_hit[!mark]
        } else {
          divs_j <- NULL
          max_td_hit_j <- NULL
        }
      } else { # above diagonal
        x_j <- x_j[mark,, drop=FALSE]
        if (!no_np) {
          divs_j <- divergent__[mark]
          max_td_hit_j <- if (no_max_td) NULL else max_td_hit[mark]
        } else {
          divs_j <- NULL
          max_td_hit_j <- NULL
        }
      }

      off_diag_args[["x"]] <- x_j
      plots[[j]] <- do.call(plot_off_diagonal, off_diag_args)

      if (!identical(condition, "divergent__") &&
          isTRUE(any(divs_j == 1))) {
        divs_j_fac <- factor(as.logical(divs_j), levels = c(FALSE, TRUE),
                             labels = c("NoDiv", "Div"))
        plots[[j]] <- plots[[j]] +
          geom_point(aes_(color = divs_j_fac, size = divs_j_fac),
                     shape = np_args$shape[1], na.rm = TRUE)
      }
      if (isTRUE(any(max_td_hit_j == 1))) {
        max_td_hit_j_fac <- factor(max_td_hit_j, levels = c(FALSE, TRUE),
                                   labels = c("NoHit", "Hit"))
        plots[[j]] <- plots[[j]] +
          geom_point(aes_(color = max_td_hit_j_fac, size = max_td_hit_j_fac),
                     shape = np_args$shape[2], na.rm = TRUE)
      }

      if (isTRUE(any(divs_j == 1)) || isTRUE(any(max_td_hit_j == 1))) {
        plots[[j]] <- plots[[j]] +
          scale_color_manual(
            values = setNames(c(NA, np_args$color[1], NA, np_args$color[2]),
                              c("NoDiv", "Div", "NoHit", "Hit"))
          ) +
          scale_size_manual(
            values = setNames(c(0, rel(np_args$size[1]), 0, rel(np_args$size[2])),
                              c("NoDiv", "Div", "NoHit", "Hit"))
          )
      }

    }

  }
  plots <- lapply(plots, function(x)
    x + xaxis_title(FALSE) + yaxis_title(FALSE))

  bayesplot_grid(plots = plots, legends = FALSE)
}


# internal ----------------------------------------------------------------
.mcmc_scatter <- function(x,
                         pars = character(),
                         regex_pars = character(),
                         transformations = list(),
                         hex = FALSE,
                         size = 2.5,
                         alpha = 0.8,
                         binwidth = NULL) {
  x <- prepare_mcmc_array(x, pars, regex_pars, transformations)
  if (dim(x)[3] != 2)
    stop(
      "For 'mcmc_scatter' and 'mcmc_hex' exactly 2 parameters must be selected. ",
      "'mcmc_pairs' can be used for more than 2 parameters."
    )

  x <- merge_chains(x)
  parnames <- colnames(x)[1:2]
  graph <- ggplot(
    data = data.frame(x = c(x[, 1]), y = c(x[, 2])),
    mapping = aes_(x = ~ x, y = ~ y)
  )
  if (!hex) { # scatterplot
    graph <- graph +
      geom_point(
        shape = 21,
        color = get_color("dh"),
        fill = get_color("d"),
        size = size,
        alpha = alpha
      )
  } else { # hex binning
    suggested_package("scales")
    graph <- graph +
      geom_hex(
        aes_(fill = ~ scales::rescale(..density..)),
        binwidth = binwidth
      ) +
      scale_fill_gradientn(
        "Density",
        colors = unlist(color_scheme_get()),
        breaks = c(.1, .9),
        labels = c("low", "high")
      )
  }

  graph +
    labs(x = parnames[1], y = parnames[2]) +
    theme_default()
}


# Get plotting functions from user-specified diag_fun, off_diag_fun arguments
#
# @param x User specified diag_fun or off_diag_fun argument to mcmc_pairs
pairs_plotfun <- function(x) {
  fun <- paste0("mcmc_", x)
  utils::getFromNamespace(fun, "bayesplot")
}

# Validate np_style argument
# @param style User's np_style argument
validate_np_style <- function(x = list()) {
  style <- list(
    color = x[["color"]] %||% c("red", "yellow2"),
    shape = x[["shape"]] %||% c(4, 3),
    size = x[["size"]] %||% c(1, 1)
  )
  if (!all(sapply(style, length) == 2))
    stop("All specified elements of 'np_style' must have length 2.",
         call. = FALSE)
  stopifnot(
    is.numeric(style[["shape"]]) || is.character(style[["shape"]]),
    is.character(style[["color"]]),
    is.numeric(style[["size"]])
  )
  return(style)
}

# Unstack molten data frame
#
# @param df A data frame (from nuts_params(), log_posterior(), etc)
# @param .form Same as 'form' arg to utils::unstack
unstack_to_matrix <- function(df, .form) {
  x <- utils::unstack(df, form = .form)
  as.matrix(x)
}


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

  warning(
    "The following parameters were dropped because they are constant: ",
    paste(names(varying)[!varying], collapse = ", ")
  )
  x[, , varying, drop = FALSE]
}
drop_dupes <- function(x) {
  dupes <- duplicated(x, MARGIN = 3)
  if (!any(dupes))
    return(x)

  warning(
    "The following parameters were dropped because they are duplicative: ",
    paste(parameter_names(x)[dupes], collapse = ", ")
  )
  x[, , !dupes, drop = FALSE]
}

