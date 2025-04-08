#' Scatterplots of MCMC draws
#'
#' Scatterplots, hexagonal heatmaps, and pairs plots from MCMC draws. See the
#' **Plot Descriptions** section, below, for details.
#'
#' @name MCMC-scatterplots
#' @family MCMC
#'
#' @template args-mcmc-x
#' @template args-pars
#' @template args-regex_pars
#' @template args-transformations
#' @param ... Currently ignored.
#' @param size,alpha For `mcmc_scatter()`, passed to
#'   [ggplot2::geom_point()] to control the appearance of the points.
#' @param bins,binwidth For `mcmc_hex()`, an optional numeric vector of
#'   *length two* passed to [ggplot2::geom_hex()] to override the
#'   default binwidth in both the vertical and horizontal directions.
#'
#' @param np Optionally, a data frame of NUTS sampler parameters, either created
#'   by [nuts_params()] or in the same form as the object returned by
#'   [nuts_params()]. The colors, shapes, and sizes of the
#'   superimposed points can be customized using the `np_style` argument.
#' @param np_style If `np` is specified, `np_style` can be a call to
#'   the `scatter_style_np()` helper function (for `mcmc_scatter()`) or
#'   the `pairs_style_np()` helper function (for `mcmc_pairs()`) to specify
#'   arguments controlling the appearance of superimposed points representing
#'   NUTS diagnostic information. (Note: for `pairs_style_np()` the
#'   `size` arguments are interpreted as scaling factors).
#'
#' @return `mcmc_scatter()` and `mcmc_hex()` return a ggplot object that
#'   can be further customized using the **ggplot2** package.
#'
#'   `mcmc_pairs()` returns many ggplot objects organized into a grid via
#'   [bayesplot_grid()].
#'
#' @section Plot Descriptions:
#' \describe{
#'   \item{`mcmc_scatter()`}{
#'    Bivariate scatterplot of posterior draws. If using a very large number of
#'    posterior draws then `mcmc_hex()` may be preferable to avoid
#'    overplotting. For models fit using [NUTS] the `np`,
#'    and `np_style` arguments can be used to add additional information in
#'    the plot (in this case the approximate location of divergences).
#'    For more on why the scatter plot with divergences is a useful
#'    diagnostic tool see [Gabry et al. (2019)](https://github.com/jgabry/bayes-vis-paper#readme).
#'   }
#'   \item{`mcmc_hex()`}{
#'    Hexagonal heatmap of 2-D bin counts. This plot is useful in cases where
#'    the posterior sample size is large enough that `mcmc_scatter()` suffers
#'    from overplotting.
#'   }
#'   \item{`mcmc_pairs()`}{
#'    A square plot matrix with univariate marginal distributions along the
#'    diagonal (as histograms or kernel density plots) and bivariate
#'    distributions off the diagonal (as scatterplots or hex heatmaps).
#'
#'    For the off-diagonal plots, the default is to split the chains so that
#'    (roughly) half are displayed above the diagonal and half are below (all
#'    chains are always merged together for the plots along the diagonal). Other
#'    possibilities are available by setting the `condition` argument.
#'
#'    Additionally, extra diagnostic information for models fit using
#'    [NUTS] can be added to the pairs plot using the `lp`,
#'    `np`, and `np_style` arguments. If `np` is specified (and
#'    `condition` is *not* `"divergent__"`), then points (red, by
#'    default) will be superimposed onto the off-diagonal plots indicating which
#'    (if any) iterations encountered a divergent transition. Also, if both
#'    `np` and `max_treedepth` are specified then points (yellow, by
#'    default) will be superimposed to indicate a transition that hit the
#'    maximum treedepth rather than terminated its evolution normally. The
#'    `np_style` argument can be used with the `pairs_style_np()`
#'    convenience function to change the appearance of these overlaid points.
#'    See the **Examples** section.
#'   }
#' }
#'
#' @template reference-vis-paper
#'
#' @examples
#' library("ggplot2")
#'
#' # some parameter draws to use for demonstration
#' x <- example_mcmc_draws(params = 6)
#' dimnames(x)
#'
#' # scatterplot of alpha vs log(sigma)
#' color_scheme_set("teal")
#' (p <- mcmc_scatter(x, pars = c("alpha", "sigma"),
#'                   transform = list(sigma = "log")))
#' p +
#'   labs(
#'     title = "Insert your own headline-grabbing title",
#'     subtitle = "with a provocative subtitle",
#'     caption = "and a controversial caption",
#'     x = expression(alpha),
#'     y = expression(log(sigma))
#'    )
#'
#' # add ellipse
#' p + stat_ellipse(level = 0.9, color = "gray20", size = 1)
#'
#' # add contour
#' color_scheme_set("red")
#' p2 <- mcmc_scatter(x, pars = c("alpha", "sigma"), size = 3.5, alpha = 0.25)
#' p2 + stat_density_2d(color = "black", size = .5)
#'
#' # can also add lines/smooths
#' color_scheme_set("pink")
#' (p3 <- mcmc_scatter(x, pars = c("alpha", "beta[3]"), alpha = 0.25, size = 3))
#' p3 + geom_smooth(method = "lm", se = FALSE, color = "gray20",
#'                  size = .75, linetype = 2)
#'
#' \donttest{
#' if (requireNamespace("hexbin", quietly = TRUE)) {
#'  # hexagonal heatmap
#'  color_scheme_set("brightblue")
#'  (p <- mcmc_hex(x, pars = c("sigma", "alpha"), transform = list(sigma = "log")))
#'  p + plot_bg(fill = "gray95")
#'  p + plot_bg(fill = "gray95") + panel_bg(fill = "gray70")
#' }
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
                         alpha = 0.8,
                         np = NULL,
                         np_style = scatter_style_np()) {
  check_ignored_arguments(...)
  .mcmc_scatter(
    x,
    pars = pars,
    regex_pars = regex_pars,
    transformations = transformations,
    size = size,
    alpha = alpha,
    hex = FALSE,
    binwidth = NULL,
    np = np,
    np_style = np_style
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
                     bins = 30,
                     binwidth = NULL) {
  suggested_package("scales")
  suggested_package("hexbin")
  check_ignored_arguments(...)
  .mcmc_scatter(
    x,
    pars = pars,
    regex_pars = regex_pars,
    transformations = transformations,
    hex = TRUE,
    bins = bins,
    binwidth = binwidth,
    size = NULL,
    alpha = NULL
  )
}

#' @rdname MCMC-scatterplots
#' @export
#' @param condition For `mcmc_pairs()`, a call to the `pairs_condition()`
#'   helper function, which is used to specify a criterion for determining which
#'   chains (or iterations) are shown in the plots above the diagonal and which
#'   are shown in the plots below the diagonal. The histograms (or density
#'   plots) along the diagonal are always made using all chains and iterations,
#'   but the scatterplots (or hex plots) above and below the diagonal show
#'   different combinations of chains/iterations depending on `condition`.
#'   The default is a call to `pairs_condition()` with none of its arguments
#'   specified. In this case half of the chains (or roughly half if there are an
#'   odd number) will be used in the plots above the diagonal and the rest in
#'   the plots below the diagonal. The `chains`, `draws`, and
#'   `nuts` arguments to `pairs_condition()`, which are documented
#'   below, can be used to change this default.
#'
#' @param lp For `mcmc_pairs()`, a molten data frame of draws of the
#'   log-posterior or, more commonly, of a quantity equal to the log-posterior
#'   up to a constant. `lp` should either be created via
#'   [log_posterior()] or be an object with the same form as the
#'   object returned by [log_posterior()].
#' @param max_treedepth For `mcmc_pairs()`, an integer representing the
#'   maximum treedepth allowed when fitting the model (if fit using NUTS). This
#'   is only needed for detecting which transitions (if any) hit the maximum
#'   treedepth.
#' @param diag_fun,off_diag_fun For `mcmc_pairs()`, the plotting function to
#'   use for the plots along the diagonal and for the off-diagonal plots,
#'   respectively. Currently `diag_fun` can be `"hist"` for histogram
#'   or `"dens"` for density, and `off_diag_fun` can be
#'   `"scatter"` for scatterplot or `"hex"` for a hexagonal heatmap.
#' @param diag_args,off_diag_args For `mcmc_pairs()`, optional named lists of
#'   arguments to pass to the functions implied by the `diag_fun` and
#'   `off_diag_fun` arguments, respectively. For example, if
#'   `off_diag_fun` is `"scatter"` then `off_diag_args` could
#'   include optional arguments to `mcmc_scatter()` like `size` and
#'   `alpha`.
#' @param grid_args,save_gg_objects For `mcmc_pairs()`, arguments to pass to
#'   [bayesplot_grid()]. For example, since `mcmc_pairs()` returns
#'   more than a single ggplot object, using `ggtitle()` afterwards
#'   will not work. But you you can still add a title to the plot using
#'   `grid_args = list(top="My title")`.
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
#' # plot chain 1 above diagonal and chains 2, 3, and 4 below
#' color_scheme_set("brightblue")
#' mcmc_pairs(x, pars = "alpha", regex_pars = "beta\\[[1,4]\\]",
#'            diag_fun = "dens", off_diag_fun = "hex",
#'            condition = pairs_condition(chains = list(1, 2:4)))
#' }
#'
#' \dontrun{
#' ### Adding NUTS diagnostics to scatterplots and pairs plots
#'
#' # examples using rstanarm package
#' library(rstanarm)
#'
#' # for demonstration purposes, intentionally fit a model that
#' # will (almost certainly) have some divergences
#' fit <- stan_glm(
#'   mpg ~ ., data = mtcars,
#'   iter = 1000, refresh = 0,
#'   # this combo of prior and adapt_delta should lead to some divergences
#'   prior = hs(),
#'   adapt_delta = 0.9
#' )
#' posterior <- as.array(fit)
#' np <- nuts_params(fit)
#'
#' # mcmc_scatter with divergences highlighted
#' color_scheme_set("brightblue")
#' mcmc_scatter(posterior, pars = c("wt", "sigma"), np = np)
#'
#' color_scheme_set("darkgray")
#' div_style <- scatter_style_np(div_color = "green", div_shape = 4, div_size = 4)
#' mcmc_scatter(posterior, pars = c("sigma", "(Intercept)"),
#'              np = np, np_style = div_style)
#'
#' # split the draws according to above/below median accept_stat__
#' # and show approximate location of divergences (red points)
#' color_scheme_set("brightblue")
#' mcmc_pairs(
#'   posterior,
#'   pars = c("wt", "cyl", "sigma"),
#'   off_diag_args = list(size = 1, alpha = 1/3),
#'   condition = pairs_condition(nuts = "accept_stat__"),
#'   np = np
#' )
#'
#' # more customizations:
#' # - transform sigma to log(sigma)
#' # - median log-posterior as 'condition'
#' # - hex instead of scatter for off-diagonal plots
#' # - show points where max treedepth hit in blue
#' color_scheme_set("darkgray")
#' mcmc_pairs(
#'   posterior,
#'   pars = c("wt", "cyl", "sigma"),
#'   transform = list(sigma = "log"),
#'   off_diag_fun = "hex",
#'   condition = pairs_condition(nuts = "lp__"),
#'   lp = log_posterior(fit),
#'   np = np,
#'   np_style = pairs_style_np(div_color = "firebrick",
#'                             td_color = "blue",
#'                             td_size = 2),
#'   # for demonstration purposes, set max_treedepth to a value that will
#'   # result in at least a few max treedepth warnings
#'   max_treedepth = with(np, -1 + max(Value[Parameter == "treedepth__"]))
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
                       condition = pairs_condition(),
                       lp = NULL,
                       np = NULL,
                       np_style = pairs_style_np(),
                       max_treedepth = NULL,
                       grid_args = list(),
                       save_gg_objects = TRUE) {
  check_ignored_arguments(...)

  stopifnot(
    is.list(diag_args),
    is.list(off_diag_args),
    inherits(np_style, "nuts_style"),
    inherits(condition, "pairs_condition")
  )

  diag_fun <- match.arg(diag_fun)
  off_diag_fun <- match.arg(off_diag_fun)
  plot_diagonal <- pairs_plotfun(diag_fun)
  plot_off_diagonal <- pairs_plotfun(off_diag_fun)

  x <- prepare_mcmc_array(x, pars, regex_pars, transformations)
  x <- drop_constants_and_duplicates(x)

  n_iter <- num_iters(x)
  n_chain <- num_chains(x)
  n_param <- num_params(x)
  pars <- parameter_names(x)

  if (n_chain == 1) {
    warn("Only one chain in 'x'. This plot is more useful with multiple chains.")
  }
  if (n_param < 2) {
    abort("This plot requires at least two parameters in 'x'.")
  }

  no_np <- is.null(np)
  no_lp <- is.null(lp)
  no_max_td <- is.null(max_treedepth)
  if (!no_np) {
    param <- sym("Parameter")
    val <- sym("Value")
    np <- validate_nuts_data_frame(np, lp)
    divs <- dplyr::filter(np, UQ(param) == "divergent__") %>% pull(UQ(val))
    divergent__ <- matrix(divs, nrow = n_iter * n_chain, ncol = n_param)[, 1]
    if (!no_max_td) {
      gt_max_td <- (dplyr::filter(np, UQ(param) == "treedepth__") %>% pull(UQ(val))) >= max_treedepth
      max_td_hit__ <- matrix(gt_max_td, nrow = n_iter * n_chain, ncol = n_param)[, 1]
    }
  }
  cond <- handle_condition(x, condition, np, lp)
  x <- merge_chains(cond[["x"]])
  mark <- cond[["mark"]]

  all_pairs <- expand.grid(pars, pars,
                           stringsAsFactors = FALSE,
                           KEEP.OUT.ATTRS = FALSE)
  plots <- vector("list", length = nrow(all_pairs))
  use_default_binwidth <- is.null(diag_args[["binwidth"]])
  for (j in seq_len(nrow(all_pairs))) {
    pair <- as.character(all_pairs[j,])

    if (identical(pair[1], pair[2])) {
      # Diagonal
      diag_args[["x"]] <- x[, pair[1], drop = FALSE]

      # silence ggplot2's "Pick better value with `binwidth`" message
      if (diag_fun == "hist" && use_default_binwidth)
        diag_args[["binwidth"]] <- diff(range(diag_args[["x"]]))/30

      plots[[j]] <-
        do.call(plot_diagonal, diag_args) +
        labs(subtitle = pair[1]) +
        theme(axis.line.y = element_blank(),
              plot.subtitle = element_text(hjust = 0.5))

    } else {
      # Off-diagonal

      # use mark if above diagonal and !mark if below the diagonal
      mark2 <- if (is_lower_tri(j, n_param)) !mark else mark
      x_j <- x[mark2, pair, drop = FALSE]

      if (!no_np) {
        divs_j <- divergent__[mark2]
        max_td_hit_j <- if (no_max_td) NULL else max_td_hit__[mark2]
      } else {
        divs_j <- max_td_hit_j <- NULL
      }
      off_diag_args[["x"]] <- x_j
      plots[[j]] <- do.call(plot_off_diagonal, off_diag_args)

      if (isTRUE(any(divs_j == 1))) {
        divs_j_fac <- factor(as.logical(divs_j),
                             levels = c(FALSE, TRUE),
                             labels = c("NoDiv", "Div"))
        plots[[j]] <- plots[[j]] +
          geom_point(
            aes(color = divs_j_fac, size = divs_j_fac),
            shape = np_style$shape[["div"]],
            alpha = np_style$alpha[["div"]],
            na.rm = TRUE
          )
      }
      if (isTRUE(any(max_td_hit_j == 1))) {
        max_td_hit_j_fac <- factor(max_td_hit_j, levels = c(FALSE, TRUE),
                                   labels = c("NoHit", "Hit"))
        plots[[j]] <- plots[[j]] +
          geom_point(
            aes(color = max_td_hit_j_fac, size = max_td_hit_j_fac),
            shape = np_style$shape[["td"]],
            alpha = np_style$alpha[["td"]],
            na.rm = TRUE
          )
      }
      if (isTRUE(any(divs_j == 1)) ||
          isTRUE(any(max_td_hit_j == 1)))
        plots[[j]] <- format_nuts_points(plots[[j]], np_style)
    }
  }

  plots <- lapply(plots, function(x)
    x + xaxis_title(FALSE) + yaxis_title(FALSE))

  bayesplot_grid(plots = plots,
                 legends = FALSE,
                 grid_args = grid_args,
                 save_gg_objects = save_gg_objects)
}


#' @rdname MCMC-scatterplots
#' @export
#' @param div_color,div_shape,div_size,div_alpha,td_color,td_shape,td_size,td_alpha
#'   Optional arguments to the `scatter_style_np()` or `pairs_style_np()`
#'   helper functions that are eventually passed to
#'   [ggplot2::geom_point()].The default values are displayed in the
#'   **Usage** section above.
scatter_style_np <-
  function(div_color = "red",
           div_shape = 16,
           div_size = 2.5,
           div_alpha = 1) {
    stopifnot(
      is.numeric(div_shape) || is.character(div_shape),
      is.character(div_color),
      is.numeric(div_size),
      is.numeric(div_alpha) && div_alpha >= 0 && div_alpha <= 1
    )
    style <- list(
      color = c(div = div_color),
      shape = c(div = div_shape),
      size = c(div = div_size),
      alpha = c(div = div_alpha)
    )
    structure(style, class = c(class(style), "nuts_style"))
  }

#' @rdname MCMC-scatterplots
#' @export
pairs_style_np <-
  function(div_color = "red",
           div_shape = 4,
           div_size = 1,
           div_alpha = 1,
           td_color = "yellow2",
           td_shape = 3,
           td_size = 1,
           td_alpha = 1) {
    stopifnot(
      is.numeric(div_shape) || is.character(div_shape),
      is.numeric(td_shape) || is.character(td_shape),
      is.character(div_color),
      is.character(td_color),
      is.numeric(div_size),
      is.numeric(td_size),
      is.numeric(div_alpha) && div_alpha >= 0 && div_alpha <= 1,
      is.numeric(td_alpha) && td_alpha >= 0 && td_alpha <= 1
    )
    style <- list(
      color = c(div = div_color, td = td_color),
      shape = c(div = div_shape, td = td_shape),
      size = c(div = div_size, td = td_size),
      alpha = c(div = div_alpha, td = td_alpha)
    )
    structure(style, class = c(class(style), "nuts_style"))
  }


#' @rdname MCMC-scatterplots
#' @export
#' @param chains,draws,nuts Optional arguments to the `pairs_condition()`
#'   helper function, which is used to specify the `condition` argument for
#'   `mcmc_pairs()`.
#' \itemize{
#'   \item The `chains` argument can be used to select some subset of the
#'   chains. If `chains` is an integer vector then the behavior is the same
#'   as the default (half the chains above the diagonal and half below) except
#'   using only the specified subset of chains. Alternatively, `chains` can
#'   be a list of two integer vectors with the first specifying the chains to be
#'   shown in the plots above the diagonal and the second for below the
#'   diagonal.
#'   \item The `draws` argument to `pairs_condition()` can be used to
#'   directly specify which realizations are plotted above and below the
#'   diagonal. `draws` can be a single proportion, which is interpreted as
#'   the proportion of realizations (among all chains) to plot in the lower
#'   panel starting with the first realization in each chain, with the
#'   complement (from the end of each chain) plotted in the upper panel.
#'   Alternatively `draws` can be a logical vector with length equal to the
#'   product of the number of iterations and the number of chains, in which case
#'   realizations corresponding to `FALSE` and `TRUE` will be plotted
#'   in the lower and upper panels, respectively.
#'   \item For models fit using NUTS, the `nuts` argument to
#'   `pairs_condition()` can be used. It takes a (possibly abbreviated)
#'   string to select among `"accept_stat__"`, `"stepsize__"`,
#'   `"treedepth__"`, `"n_leapfrog__"`, `"divergent__"`,
#'   `"energy__"`, and `"lp__"`. These are the sampler parameters
#'   associated with [NUTS()] (and `"lp__"` is the log-posterior
#'   up to an additive constant). In this case, plots below the diagonal will
#'   contain realizations that are below the median of the indicated variable
#'   (or are zero in the case of `"divergent__"`), and plots above the
#'   diagonal will contain realizations that are greater than or equal to the
#'   median of the indicated variable (or are one in the case of
#'   `"divergent__"`). If `"lp__"` is used then the `lp`
#'   argument to `mcmc_pairs` must also be specified. For the other NUTS
#'   parameters the `np` argument to `mcmc_pairs()` must also be
#'   specified.
#'  }
#'
pairs_condition <- function(chains = NULL, draws = NULL, nuts = NULL) {
  .ignore_args <- function(..., why = NULL) {
    dots <- list(...)
    nms <- names(dots)[!sapply(dots, is.null)]
    if (length(nms)) {
      inform(paste0(
        "The following specified arguments were ignored by 'pairs_condition' ",
        "because ", why, ": ",
        paste(sQuote(nms), collapse = ", ")
      ))
    }
  }
  .error_duplicate_chains <- function() {
    abort("Each chain can only be specified once in the 'chains' argument to 'pairs_condition'.")
  }


  if (is.null(chains) &&
      is.null(draws) &&
      is.null(nuts)) {
    # default: half of the chains above diag, half below

    cond <- list()
    cond_type <- "default"

  } else if (!is.null(chains)) {
    # Using 'chains' argument
    .ignore_args(
      draws = draws, nuts = nuts,
      why = "they are superseded by 'chains'"
    )

    if (is.list(chains)) {
      # list of two integer vectors, each specifying a subset of the chains
      stopifnot(length(chains) == 2)
      chain_vec <- unlist(chains, use.names = FALSE)
      if (length(chain_vec) != length(unique(chain_vec)))
        .error_duplicate_chains()

      cond <- list(upper = as.integer(chains[[1]]),
                   lower = as.integer(chains[[2]]))
      cond_type <- "chain_list"
    } else if (is.numeric(chains)) {
      # single vector specifying a subset of chains
      stopifnot(NCOL(chains) == 1, all(chains == as.integer(chains)))
      if (length(chains) != length(unique(chains)))
        .error_duplicate_chains()

      cond <- as.integer(chains)
      cond_type <- "chain_vector"
    } else {
      abort(paste(
        "The 'chains' argument to 'pairs_condition' must be",
        "an integer vector or a list of two integer vectors."
      ))
    }

  } else if (!is.null(draws)) {
    # Using 'draws' argument
    .ignore_args(nuts = nuts, why = "they are superseded by 'draws'")

    if (is.numeric(draws)) {
      # proportion of realizations (among all chains) to plot in the lower panel
      stopifnot(draws > 0 && draws < 1)
      cond <- draws
      cond_type <- "draws_proportion"
    } else if (is.logical(draws)) {
      # T/F for each iteration to split into upper/lower panels
      cond <- draws
      cond_type <- "draws_selection"
    } else {
      abort(paste(
        "The 'draws' argument to 'pairs_condition' must be",
        "a single proportion or a logical vector."
      ))
    }
  } else {
    # Using 'nuts' argument
    if (!is.character(nuts) || length(nuts) > 1) {
      abort("The 'nuts' argument to 'pairs_condition' must be a single string.")
    }

    cond_type <- "nuts"
    cond <- match.arg(nuts, several.ok = FALSE,
                      choices = c("accept_stat__", "stepsize__",
                                  "treedepth__", "n_leapfrog__",
                                  "divergent__", "energy__", "lp__"))
  }

  structure(
    cond,
    class = c(class(cond), "pairs_condition"),
    type = cond_type # this attribute is used later by handle_condition()
  )
}



# internal ----------------------------------------------------------------
#' @importFrom dplyr pull
.mcmc_scatter <- function(x,
                          pars = character(),
                          regex_pars = character(),
                          transformations = list(),
                          hex = FALSE,
                          size = 2.5,
                          alpha = 0.8,
                          bins = 30,
                          binwidth = NULL,
                          np = NULL,
                          np_style = scatter_style_np()) {
  x <- prepare_mcmc_array(x, pars, regex_pars, transformations)
  if (num_params(x) != 2) {
    abort(paste(
      "For 'mcmc_scatter' and 'mcmc_hex' exactly 2 parameters must be selected.",
      "'mcmc_pairs' can be used for more than 2 parameters."
    ))
  }

  x <- merge_chains(x)
  parnames <- colnames(x)[1:2]
  has_divs <- !is.null(np)

  xydata <- data.frame(x = c(x[, 1]), y = c(x[, 2]))
  if (has_divs) {
    if (hex) {
      warn("'np' is currently ignored for hex plots.")
    }
    stopifnot(inherits(np_style, "nuts_style"))
    np <- validate_nuts_data_frame(np)
    param <- sym("Parameter")
    val <- sym("Value")
    divg <- sym("Divergent")
    xydata$Divergent <-
      np %>%
      dplyr::filter(UQ(param) == "divergent__") %>%
      pull(UQ(val))

    divdata <- dplyr::filter(xydata, UQ(divg) == 1)
    xydata <- dplyr::filter(xydata, UQ(divg) == 0)
  }

  graph <- ggplot(data = xydata, aes(x = .data$x, y = .data$y)) +
    bayesplot_theme_get()

  if (!hex) { # scatterplot
    graph <- graph +
      geom_point(
        shape = 21,
        color = get_color("dh"),
        fill = get_color("d"),
        size = size,
        alpha = alpha
      )

    if (has_divs) {
      graph <- graph +
        geom_point(
          data = divdata,
          color = np_style$color[["div"]],
          size = np_style$size[["div"]],
          alpha = np_style$alpha[["div"]],
          shape = np_style$shape[["div"]]
        )
    }
  } else { # hex binning
    graph <- graph +
      geom_hex(
        aes(fill = scales::rescale(after_stat(density))),
        bins = bins,
        binwidth = binwidth
      ) +
      scale_fill_gradientn(
        "Density",
        colors = unlist(color_scheme_get()),
        breaks = c(.1, .9),
        labels = c("low", "high")
      )
  }

  graph + labs(x = parnames[1], y = parnames[2])
}



# internal for mcmc_pairs -------------------------------------------------

#' Get plotting functions from user-specified
#' `diag_fun` and `off_diag_fun` arguments
#'
#' @noRd
#' @param x User specified `diag_fun` or `off_diag_fun` argument to `mcmc_pairs()`
pairs_plotfun <- function(x) {
  fun <- paste0("mcmc_", x)
  utils::getFromNamespace(fun, "bayesplot")
}

#' Unstack molten data frame
#'
#' @noRd
#' @param df A data frame (from `nuts_params()`, `log_posterior()`, etc)
#' @param .form Same as `form` arg to `utils::unstack()`
unstack_to_matrix <- function(df, .form) {
  x <- utils::unstack(df, form = .form)
  as.matrix(x)
}

#' Check if off-diagonal plot is above or below the diagonal
#'
#' @noRd
#' @param j integer (index)
#' @param n Number of parameters (number of plots = `n^2`)
#' @return `TRUE` if below the diagonal, `FALSE` if above the diagonal
is_lower_tri <- function(j, n) {
  idx <- array_idx_j(j, n)
  lower_tri <- lower_tri_idx(n)
  row_match_found(idx, lower_tri)
}

#' Get array indices of the jth element in the plot matrix
#'
#' @noRd
#' @param j integer (index)
#' @param n number of parameters (number of plots = n^2)
#' @return rwo vector (1-row matrix) containing the array indices of the jth
#'   element in the plot matrix
array_idx_j <- function(j, n) {
  jj <- matrix(seq_len(n^2), nrow = n, byrow = TRUE)[j]
  arrayInd(jj, .dim = c(n, n))
}

#' Get indices of lower triangular elements of a square matrix
#' @noRd
#' @param n number of rows (columns) in the square matrix
lower_tri_idx <- function(n) {
  a <- rev(abs(sequence(seq.int(n - 1)) - n) + 1)
  b <- rep.int(seq.int(n - 1), rev(seq.int(n - 1)))
  cbind(row = a, col = b)
}

#' Find which (if any) row in y is a match for x
#' @noRd
#' @param x a row vector (i.e., a matrix with 1 row)
#' @param y a matrix
#' @return either a row number in `y` or `NA` if no match
row_match_found <- function(x, y) {
  stopifnot(is.matrix(x), is.matrix(y), nrow(x) == 1)
  x <- as.data.frame(x)
  y <- as.data.frame(y)
  res <- match(
    do.call(function(...) paste(..., sep=":::"), x),
    do.call(function(...) paste(..., sep=":::"), y)
  )
  isTRUE(!is.na(res) && length(res) == 1)
}


#' Drop any constant or duplicate variables
#' @noRd
#' @param x 3-D array
drop_constants_and_duplicates <- function(x) {
  x2 <- drop_consts(x)
  x2 <- drop_dupes(x2)
  class(x2) <- c(class(x2), "mcmc_array")
  x2
}
drop_consts <- function(x) {
  varying <- apply(x, 3, FUN = function(y) length(unique(c(y))) > 1)
  if (all(varying))
    return(x)

  warn(paste(
    "The following parameters were dropped because they are constant:",
    paste(names(varying)[!varying], collapse = ", ")
  ))
  x[, , varying, drop = FALSE]
}
drop_dupes <- function(x) {
  dupes <- duplicated(x, MARGIN = 3)
  if (!any(dupes))
    return(x)

  warn(paste(
    "The following parameters were dropped because they are duplicative:",
    paste(parameter_names(x)[dupes], collapse = ", ")
  ))
  x[, , !dupes, drop = FALSE]
}

#' Handle user's specified `condition`
#' @noRd
#' @param x 3-D mcmc array.
#' @param condition Object returned by `pairs_condition()`.
#' @param np,lp User-specified arguments to `mcmc_pairs()`.
#' @return A named list containing `"x"` (`x`, possibly modified) and `"mark"`
#'   (logical or interger vector for eventually splitting `x`).
handle_condition <- function(x, condition=NULL, np=NULL, lp=NULL) {
  n_iter <- num_iters(x)
  n_chain <- num_chains(x)
  no_np <- is.null(np)
  no_lp <- is.null(lp)

  cond_type <- attr(condition, "type")

  if (cond_type == "default") {
    k <- ncol(x) %/% 2
    mark <- c(rep(FALSE, n_iter * k), rep(TRUE, n_iter * (n_chain - k)))
  } else if (cond_type == "chain_vector") {
    x <- x[, condition, , drop = FALSE]
    k <- ncol(x) %/% 2
    n_chain <- length(condition)
    mark <- c(rep(FALSE, n_iter * k), rep(TRUE, n_iter * (n_chain - k)))
  } else if (cond_type == "chain_list") {
    x <- x[, c(condition[[1]], condition[[2]]), , drop = FALSE]
    k1 <- length(condition[[1]])
    k2 <- length(condition[[2]])
    mark <- c(rep(TRUE, n_iter * k1), rep(FALSE, n_iter * k2))
  } else if (cond_type == "draws_proportion") {
    mark <- rep(1:n_iter > (condition * n_iter), times = n_chain)
  } else if (cond_type == "draws_selection") {
    # T/F for each iteration to split into upper and lower
    stopifnot(length(condition) == (n_iter * n_chain))
    mark <- !condition
  } else if (cond_type == "nuts") {
    # NUTS sampler param or lp__
    if (no_np && condition != "lp__")
      abort(paste(
        "To use this value of 'condition' the 'np' argument",
        "to 'mcmc_pairs' must also be specified."
      ))

    if (condition == "lp__") {
      if (no_lp)
        abort(paste(
          "If 'condition' is 'lp__' then the 'lp' argument",
          "to 'mcmc_pairs' must also be specified."
        ))
      mark <- unstack_to_matrix(lp, Value ~ Chain)

    } else {
      param <- sym("Parameter")
      mark <- dplyr::filter(np, UQ(param) == condition)
      mark <- unstack_to_matrix(mark, Value ~ Chain)
    }
    if (condition == "divergent__") {
      mark <- as.logical(mark)
    } else {
      mark <- c(mark) >= median(mark)
    }
    if (length(unique(mark)) == 1)
      abort(paste(condition, "is constant so it cannot be used as a condition."))
  }

  list(x = x, mark = mark)
}


#' Apply scale_color_manual and scale_size_manual if plotting divergences and
#' hitting max_treedepth
#'
#' @noRd
#' @param graph ggplot object
#' @param np_args list of style arguments returned by `pairs_style_np()`
#' @return `graph`, updated
format_nuts_points <- function(graph, np_args) {
  graph +
    scale_color_manual(
      values = set_names(c(NA, np_args$color[["div"]], NA, np_args$color[["td"]]),
                         c("NoDiv", "Div", "NoHit", "Hit"))
    ) +
    scale_size_manual(
      values = set_names(c(NA, rel(np_args$size[["div"]]), NA, rel(np_args$size[["td"]])),
                         c("NoDiv", "Div", "NoHit", "Hit"))
    )
}
