#' Trace and rank plots of MCMC draws
#'
#' Trace and rank plots of MCMC draws. See the **Plot Descriptions**
#' section, below, for details.
#'
#' @name MCMC-traces
#' @family MCMC
#' @template args-mcmc-x
#' @template args-pars
#' @template args-regex_pars
#' @template args-transformations
#' @template args-facet_args
#' @template args-pit-ecdf
#' @param ... Currently ignored.
#' @param size An optional value to override the default line size
#'   for `mcmc_trace()` or the default point size for `mcmc_trace_highlight()`.
#' @param alpha For `mcmc_trace_highlight()`, passed to
#'   [ggplot2::geom_point()] to control the transparency of the points
#'   for the chains not highlighted.
#' @param n_warmup An integer; the number of warmup iterations included in
#'   `x`. The default is `n_warmup = 0`, i.e. to assume no warmup
#'   iterations are included. If `n_warmup > 0` then the background for
#'   iterations `1:n_warmup` is shaded gray.
#' @param iter1 An integer; the iteration number of the first included draw
#'   (default is `0`). This can be used to make it more obvious that the warmup
#'   iterations have been discarded from the traceplot. It cannot be specified
#'   if `n_warmup` is also set to a positive value.
#' @param window An integer vector of length two specifying the limits of a
#'   range of iterations to display.
#' @param np For models fit using [NUTS] (more generally, any
#'   [symplectic integrator](https://en.wikipedia.org/wiki/Symplectic_integrator)),
#'   an optional data frame providing NUTS diagnostic information. The data
#'   frame should be the object returned by [nuts_params()] or one with the same
#'   structure. If `np` is specified then tick marks are added to the bottom of
#'   the trace plot indicating within which iterations there was a divergence
#'   (if there were any). See the end of the **Examples** section, below.
#' @param np_style A call to the `trace_style_np()` helper function to
#'   specify arguments controlling the appearance of tick marks representing
#'   divergences (if the `np` argument is specified).
#' @param divergences Deprecated. Use the `np` argument instead.
#'
#' @template return-ggplot-or-data
#' @return `mcmc_trace_data()` returns the data for the trace *and* rank plots
#'   in the same data frame.
#'
#' @section Plot Descriptions:
#' \describe{
#'   \item{`mcmc_trace()`}{
#'    Standard trace plots of MCMC draws. For models fit using [NUTS],
#'    the `np` argument can be used to also show divergences on the trace plot.
#'   }
#'   \item{`mcmc_trace_highlight()`}{
#'    Traces are plotted using points rather than lines and the opacity of all
#'    chains but one (specified by the `highlight` argument) is reduced.
#'   }
#'   \item{`mcmc_rank_hist()`}{
#'    Whereas traditional trace plots visualize how the chains mix over the
#'    course of sampling, rank histograms visualize how the values
#'    from the chains mix together in terms of ranking. An ideal plot would
#'    show the rankings mixing or overlapping in a uniform distribution.
#'    See Vehtari et al. (2019) for details.
#'   }
#'   \item{`mcmc_rank_overlay()`}{
#'    Ranks from `mcmc_rank_hist()` are plotted using overlaid lines in a
#'    single panel.
#'   }
#'   \item{`mcmc_rank_ecdf()`}{
#'    The ECDFs of the ranks from `mcmc_rank_hist()` are plotted with the
#'    simultaneous confidence bands with a coverage determined by `prob`, that
#'    is, bands that completely cover all of the rank ECDFs with the probability
#'    `prob`. If `plot_diff = TRUE`, the difference between the observed rank
#'    ECDFs and the theoretical expectation for samples originating from the
#'    same distribution is drawn. See Säilynoja et al. (2021) for details.
#'   }
#' }
#'
#' @template reference-improved-rhat
#' @template reference-uniformity-test
#' @examples
#' # some parameter draws to use for demonstration
#' x <- example_mcmc_draws(chains = 4, params = 6)
#' dim(x)
#' dimnames(x)
#'
#' # trace plots of the betas
#' color_scheme_set("viridis")
#' mcmc_trace(x, regex_pars = "beta")
#' \donttest{
#' color_scheme_set("viridisA")
#' mcmc_trace(x, regex_pars = "beta")
#'
#' color_scheme_set("viridisC")
#' mcmc_trace(x, regex_pars = "beta")
#' }
#'
#' # mix color schemes
#' color_scheme_set("mix-blue-red")
#' mcmc_trace(x, regex_pars = "beta")
#'
#' # use traditional ggplot discrete color scale
#' mcmc_trace(x, pars = c("alpha", "sigma")) +
#'  ggplot2::scale_color_discrete()
#'
#' # zoom in on a window of iterations, increase line size,
#' # add tick marks, move legend to the top, add gray background
#' color_scheme_set("viridisA")
#' mcmc_trace(x[,, 1:4], window = c(100, 130), size = 1) +
#'   panel_bg(fill = "gray90", color = NA) +
#'   legend_move("top")
#'
#' # Rank-normalized histogram plots. Instead of showing how chains mix over
#' # time, look at how the ranking of MCMC samples mixed between chains.
#' color_scheme_set("viridisE")
#' mcmc_rank_hist(x, "alpha")
#' mcmc_rank_hist(x, pars = c("alpha", "sigma"), ref_line = TRUE)
#' mcmc_rank_overlay(x, "alpha")
#'
#' # ECDF and ECDF difference plots of the ranking of MCMC samples between chains.
#' # Provide 99% simultaneous confidence intervals for the chains sampling from
#' # the same distribution.
#' mcmc_rank_ecdf(x, prob = 0.99)
#' mcmc_rank_ecdf(x, prob = 0.99, plot_diff = TRUE)
#'
#' \dontrun{
#' # parse facet label text
#' color_scheme_set("purple")
#' p <- mcmc_trace(
#'   x,
#'   regex_pars = "beta\\[[1,3]\\]",
#'   facet_args = list(labeller = ggplot2::label_parsed)
#' )
#' p + facet_text(size = 15)
#'
#' # mark first 100 draws as warmup
#' mcmc_trace(x, n_warmup = 100)
#'
#' # plot as points, highlighting chain 2
#' color_scheme_set("brightblue")
#' mcmc_trace_highlight(x, pars = "sigma", highlight = 2, size = 2)
#'
#' # for models fit using HMC/NUTS divergences can be displayed in the trace plot
#' library("rstanarm")
#' fit <- stan_glm(mpg ~ ., data = mtcars, refresh = 0,
#'   # next line to keep example fast and also ensure we get some divergences
#'                 prior = hs(), iter = 400, adapt_delta = 0.8)
#'
#' # extract draws using as.array (instead of as.matrix) to keep
#' # chains separate for trace plot
#' posterior <- as.array(fit)
#'
#' # for stanfit and stanreg objects use nuts_params() to get the divergences
#' mcmc_trace(posterior, pars = "sigma", np = nuts_params(fit))
#'
#' color_scheme_set("viridis")
#' mcmc_trace(
#'   posterior,
#'   pars = c("wt", "sigma"),
#'   size = 0.5,
#'   facet_args = list(nrow = 2),
#'   np = nuts_params(fit),
#'   np_style = trace_style_np(div_color = "black", div_size = 0.5)
#' )
#' }
#'
NULL

#' @rdname MCMC-traces
#' @export
mcmc_trace <-
  function(x,
           pars = character(),
           regex_pars = character(),
           transformations = list(),
           ...,
           facet_args = list(),
           n_warmup = 0,
           iter1 = 0,
           window = NULL,
           size = NULL,
           np = NULL,
           np_style = trace_style_np(),
           divergences = NULL) {

  # deprecate 'divergences' arg in favor of 'np'
  # (for consistency across functions)
  if (!is.null(np) && !is.null(divergences)) {
    abort(paste0(
      "'np' and 'divergences' can't both be specified. ",
      "Use only 'np' (the 'divergences' argument is deprecated)."
    ))
  } else if (!is.null(divergences)) {
    warn(paste0(
      "The 'divergences' argument is deprecated ",
      "and will be removed in a future release. ",
      "Use the 'np' argument instead."
    ))
    np <- divergences
  }

  check_ignored_arguments(...)
  .mcmc_trace(
    x,
    pars = pars,
    regex_pars = regex_pars,
    transformations = transformations,
    facet_args = facet_args,
    n_warmup = n_warmup,
    window = window,
    size = size,
    style = "line",
    np = np,
    np_style = np_style,
    iter1 = iter1,
    ...
  )
}

#' @rdname MCMC-traces
#' @export
#' @param highlight For `mcmc_trace_highlight()`, an integer specifying one
#'   of the chains that will be more visible than the others in the plot.
mcmc_trace_highlight <- function(x,
                                 pars = character(),
                                 regex_pars = character(),
                                 transformations = list(),
                                 ...,
                                 facet_args = list(),
                                 n_warmup = 0,
                                 window = NULL,
                                 size = NULL,
                                 alpha = 0.2,
                                 highlight = 1) {
  check_ignored_arguments(...)
  .mcmc_trace(
    x,
    pars = pars,
    regex_pars = regex_pars,
    transformations = transformations,
    facet_args = facet_args,
    n_warmup = n_warmup,
    window = window,
    size = size,
    alpha = alpha,
    highlight = highlight,
    style = "point",
    ...
  )
}


#' @rdname MCMC-traces
#' @export
#' @param div_color,div_size,div_alpha Optional arguments to the
#'   `trace_style_np()` helper function that are eventually passed to
#'   [ggplot2::geom_rug()] if the `np` argument is also specified. They control
#'   the color, size, and transparency specifications for showing divergences in
#'   the plot. The default values are displayed in the **Usage** section above.
#'
trace_style_np <- function(div_color = "red", div_size = 0.25, div_alpha = 1) {
  stopifnot(
    is.character(div_color),
    is.numeric(div_size),
    is.numeric(div_alpha) && div_alpha >= 0 && div_alpha <= 1
  )

  style <- list(
    color = c(div = div_color),
    size = c(div = div_size),
    alpha = c(div = div_alpha)
  )

  structure(style, class = c(class(style), "nuts_style"))
}

#' @rdname MCMC-traces
#' @param n_bins For the rank plots, the number of bins to use for the histogram
#'   of rank-normalized MCMC samples. Defaults to `20`.
#' @param ref_line For the rank plots, whether to draw a horizontal line at the
#'   average number of ranks per bin. Defaults to `FALSE`.
#' @export
mcmc_rank_overlay <- function(x,
                              pars = character(),
                              regex_pars = character(),
                              transformations = list(),
                              facet_args = list(),
                              ...,
                              n_bins = 20,
                              ref_line = FALSE) {
  check_ignored_arguments(...)
  data <- mcmc_trace_data(
    x,
    pars = pars,
    regex_pars = regex_pars,
    transformations = transformations
  )

  n_chains <- unique(data$n_chains)
  n_param <- unique(data$n_parameters)

  # We have to bin and count the data ourselves because
  # ggplot2::stat_bin(geom = "step") does not draw the final bin.
  histobins <- data %>%
    dplyr::distinct(.data$value_rank) %>%
    mutate(cut = cut(.data$value_rank, n_bins)) %>%
    group_by(.data$cut) %>%
    mutate(bin_start = min(.data$value_rank)) %>%
    ungroup()

  # Count how many values fall into each bin per chain & parameter
  d_bin_counts <- data %>%
    left_join(histobins, by = "value_rank") %>%
    count(.data$parameter, .data$chain, .data$bin_start)

  # Now ensure that all combinations of parameter, chain, and bin_start exist
  # even if no counts are present (https://github.com/stan-dev/bayesplot/issues/331)
  all_combos <- dplyr::as_tibble(expand.grid(
    parameter = unique(data$parameter),
    chain = unique(data$chain),
    bin_start = unique(histobins$bin_start),
    stringsAsFactors = FALSE
  ))
  d_bin_counts <- all_combos %>%
    left_join(d_bin_counts, by = c("parameter", "chain", "bin_start")) %>%
    mutate(n = dplyr::if_else(is.na(n), 0L, n))

  # Duplicate the final bin, setting the left edge to the greatest x value, so
  # that the entire x-axis is used,
  right_edge <- max(data$value_rank)
  d_bin_counts <- d_bin_counts %>%
    dplyr::filter(.data$bin_start == max(.data$bin_start)) %>%
    mutate(bin_start = right_edge) %>%
    dplyr::bind_rows(d_bin_counts)

  scale_color <- scale_color_manual("Chain", values = chain_colors(n_chains))

  layer_ref_line <- if (ref_line) {
    geom_hline(
      yintercept = (right_edge / n_bins) / n_chains,
      color = get_color("dark_highlight"),
      size = 1,
      linetype = "dashed"
    )
  } else {
    NULL
  }

  facet_call <- NULL
  if (n_param > 1) {
    facet_args$facets <- vars(.data$parameter)
    facet_args$scales <- facet_args$scales %||% "fixed"
    facet_call <- do.call("facet_wrap", facet_args)
  }

  ggplot(d_bin_counts) +
    aes(x = .data$bin_start, y =  .data$n, color = .data$chain) +
    geom_step() +
    layer_ref_line +
    facet_call +
    scale_color +
    ylim(c(0, NA)) +
    bayesplot_theme_get() +
    force_x_axis_in_facets() +
    labs(x = "Rank", y = NULL)
}

#' @rdname MCMC-traces
#' @export
mcmc_rank_hist <- function(x,
                           pars = character(),
                           regex_pars = character(),
                           transformations = list(),
                           ...,
                           facet_args = list(),
                           n_bins = 20,
                           ref_line = FALSE) {
  check_ignored_arguments(...)
  data <- mcmc_trace_data(
    x,
    pars = pars,
    regex_pars = regex_pars,
    transformations = transformations
  )

  n_iter <- unique(data$n_iterations)
  n_chains <- unique(data$n_chains)
  n_param <- unique(data$n_parameters)

  # Create a dataframe with chain x parameter x min(rank) x max(rank) to set
  # x axis range in each facet
  data_boundaries <- data %>%
    dplyr::distinct(.data$chain, .data$parameter)

  data_boundaries <- dplyr::bind_rows(
    mutate(data_boundaries, value_rank = min(data$value_rank)),
    mutate(data_boundaries, value_rank = max(data$value_rank))
  )

  right_edge <- max(data_boundaries$value_rank)

  facet_args[["scales"]] <- facet_args[["scales"]] %||% "fixed"

  # If there is one parameter, put the chains in one row.
  # Otherwise, use a grid.
  if (n_param > 1) {
    facet_f <- facet_grid
    facet_args[["rows"]] <- facet_args[["rows"]] %||% vars(.data$parameter)
    facet_args[["cols"]] <- facet_args[["cols"]] %||% vars(.data$chain)
  } else {
    facet_f <- facet_wrap
    facet_args[["facets"]] <- vars(.data$parameter, .data$chain)
    facet_args[["nrow"]] <- facet_args[["nrow"]] %||% 1
    labeller <- function(x) label_value(x, multi_line = FALSE)
    facet_args[["labeller"]] <- facet_args[["labeller"]] %||% labeller
  }

  layer_ref_line <- if (ref_line) {
    geom_hline(
      yintercept = (right_edge / n_bins) / n_chains,
      color = get_color("dark_highlight"),
      size = .5,
      linetype = "dashed"
    )
  } else {
    NULL
  }

  facet_call <- do.call(facet_f, facet_args)

  ggplot(data) +
    aes(x = .data$value_rank) +
    geom_histogram(
      color = get_color("mid_highlight"),
      fill = get_color("mid"),
      binwidth = right_edge / n_bins,
      boundary = right_edge,
      linewidth = 0.25
    ) +
    layer_ref_line +
    geom_blank(data = data_boundaries) +
    facet_call +
    force_x_axis_in_facets() +
    dont_expand_y_axis(c(0.005, 0)) +
    bayesplot_theme_get() +
    theme(
      axis.line.y = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank()
    ) +
    labs(x = "Rank")
}

#' @rdname MCMC-traces
#' @param prob For `mcmc_rank_ecdf()`, a value between 0 and 1
#' specifying the desired simultaneous confidence of the confidence bands to be
#' drawn for the rank ECDF plots.
#' @param plot_diff For `mcmc_rank_ecdf()`, a boolean specifying if the
#' difference between the observed rank ECDFs and the theoretical expectation
#' should be drawn instead of the unmodified rank ECDF plots.
#' @export
mcmc_rank_ecdf <-
  function(x,
           pars = character(),
           regex_pars = character(),
           transformations = list(),
           ...,
           K = NULL,
           facet_args = list(),
           prob = 0.99,
           plot_diff = FALSE,
           interpolate_adj = NULL) {
  check_ignored_arguments(...,
    ok_args = c("K", "pit", "prob", "plot_diff", "interpolate_adj", "M")
  )
  data <- mcmc_trace_data(
    x,
    pars = pars,
    regex_pars = regex_pars,
    transformations = transformations,
    highlight = 1
  )
  n_iter <- unique(data$n_iterations)
  n_chain <- unique(data$n_chains)
  n_param <- unique(data$n_parameters)

  x <- if (is.null(K)) {
    0:n_iter / n_iter
  } else {
    0:K / K
  }
  gamma <- adjust_gamma(
    N = n_iter,
    L = n_chain,
    K = if (is.null(K)) {
      n_iter
    } else {
      K
    },
    prob = prob,
    interpolate_adj = interpolate_adj,
    ...
  )
  lims <- ecdf_intervals(
    gamma = gamma,
    N = n_iter,
    K = if (is.null(K)) {
      n_iter
    } else {
      K
    },
    L <- n_chain
  )
  data_lim <- data.frame(
    upper = lims$upper / n_iter - (plot_diff == TRUE) * x,
    lower = lims$lower / n_iter - (plot_diff == TRUE) * x,
    x = x
  )
  data <- data %>%
    group_by(.data$parameter, .data$chain) %>%
    dplyr::group_map(~ data.frame(
      parameter = .y[1],
      chain = .y[2],
      ecdf_value = ecdf(.x$value_rank / (n_iter * n_chain))(x) -
        (plot_diff == TRUE) * x,
      x = x
    )) %>%
    dplyr::bind_rows()

  mapping <- aes(
    x = .data$x,
    y = .data$ecdf_value,
    color = .data$chain,
    group = .data$chain
  )

  scale_color <- scale_color_manual("Chain", values = chain_colors(n_chain))

  facet_call <- NULL
  if (n_param == 1) {
    facet_call <- ylab(levels(data$parameter))
  } else {
    facet_args$facets <- vars(.data$parameter)
    facet_args$scales <- facet_args$scales %||% "free"
    facet_call <- do.call("facet_wrap", facet_args)
  }

  ggplot() +
    geom_step(data = data_lim, aes(x = .data$x, y = .data$upper), show.legend = FALSE) +
    geom_step(data = data_lim, aes(x = .data$x, y = .data$lower), show.legend = FALSE) +
    geom_step(mapping, data) +
    bayesplot_theme_get() +
    scale_color +
    facet_call +
    scale_x_continuous(breaks = pretty) +
    legend_move(ifelse(n_chain > 1, "right", "none")) +
    xaxis_title(FALSE) +
    yaxis_title(on = n_param == 1)
}

#' @rdname MCMC-traces
#' @export
mcmc_trace_data <- function(x,
                            pars = character(),
                            regex_pars = character(),
                            transformations = list(),
                            ...,
                            highlight = NULL,
                            n_warmup = 0,
                            iter1 = 0) {
  check_ignored_arguments(...)

  x <- prepare_mcmc_array(x, pars, regex_pars, transformations)

  if (iter1 < 0) {
    abort("'iter1' cannot be negative.")
  }

  if (n_warmup > 0 && iter1 > 0) {
    abort("'n_warmup' and 'iter1' can't both be specified.")
  }

  if (!is.null(highlight)) {
    stopifnot(length(highlight) == 1)

    if (!has_multiple_chains(x)){
      STOP_need_multiple_chains()
    }

    if (!highlight %in% seq_len(ncol(x))) {
      abort(paste0(
        "'highlight' is ", highlight,
        ", but 'x' contains ", ncol(x), " chains."
      ))
    }
  }

  data <- melt_mcmc(x)
  data$Chain <- factor(data$Chain)
  data$n_chains <- num_chains(data)
  data$n_iterations <- num_iters(data)
  data$n_parameters <- num_params(data)
  data <- rlang::set_names(data, tolower)

  first_cols <- syms(c("parameter", "value", "value_rank"))
  data <- data %>%
    group_by(.data$parameter) %>%
    mutate(
      value_rank = rank(.data$value, ties.method = "average")
    ) %>%
    ungroup() %>%
    select(!!! first_cols, tidyselect::everything())

  data$highlight <- if (!is.null(highlight)) {
    data$chain == highlight
  } else {
    FALSE
  }

  data$warmup <- data$iteration <= n_warmup
  data$iteration <- data$iteration + as.integer(iter1)
  tibble::as_tibble(data)
}


# internal -----------------------------------------------------------------
.mcmc_trace <- function(x,
                        pars = character(),
                        regex_pars = character(),
                        transformations = list(),
                        n_warmup = 0,
                        window = NULL,
                        size = NULL,
                        facet_args = list(),
                        highlight = NULL,
                        style = c("line", "point"),
                        alpha = 0.2,
                        np = NULL,
                        np_style = trace_style_np(),
                        iter1 = 0,
                        ...) {
  style <- match.arg(style)
  data <- mcmc_trace_data(
    x,
    pars = pars,
    regex_pars = regex_pars,
    transformations = transformations,
    highlight = highlight,
    n_warmup = n_warmup,
    iter1 = iter1
  )
  n_iter <- unique(data$n_iterations)
  n_chain <- unique(data$n_chains)
  n_param <- unique(data$n_parameters)

  mapping <- aes(
    x = .data$iteration,
    y = .data$value,
    color = .data$chain
  )

  if (!is.null(highlight)) {
    mapping <- modify_aes(
      mapping,
      alpha = .data$highlight,
      color = .data$highlight
    )
  }

  layer_warmup <- if (n_warmup > 0) {
    layer_warmup <- annotate(
      "rect", xmin = -Inf, xmax = n_warmup, ymin = -Inf, ymax = Inf, linewidth = 1,
      color = "gray88", fill = "gray88", alpha = 0.5
    )
  } else {
    NULL
  }

  geom_args <- list()
  if (style == "line") {
    geom_args$linewidth = size %||% 1 / 3
  } else {
    geom_args$size = size %||% 1
  }
  layer_draws <- do.call(paste0("geom_", style), geom_args)

  coord_window <- if (!is.null(window)) {
    stopifnot(length(window) == 2)
    coord_cartesian(xlim = window)
  } else {
    NULL
  }

  scale_alpha <- NULL
  scale_color <- NULL
  div_rug <- NULL
  div_guides <- NULL

  if (!is.null(highlight)) {
    ## scale_alpha_discrete() warns on default
    scale_alpha <- scale_alpha_ordinal(range = c(alpha, 1), guide = "none")
    scale_color <- scale_color_manual(
      "",
      values = get_color(c("lh", "d")),
      labels = c("Other chains", paste("Chain", highlight)))
  } else {
    scale_color <- scale_color_manual("Chain", values = chain_colors(n_chain))

    if (!is.null(np)) {
      div_rug <- divergence_rug(np, np_style, n_iter, n_chain)
      if (!is.null(div_rug)) {
        div_guides <- guides(
          color = guide_legend(order = 1),
          linetype = guide_legend(
            order = 2, title = NULL, keywidth = rel(1/2),
            override.aes = list(size = rel(1/2)))
        )
      }
    }
  }

  facet_call <- NULL
  if (n_param == 1) {
    facet_call <- ylab(levels(data$parameter))
  } else {
    facet_args$facets <- vars(.data$parameter)
    facet_args$scales <- facet_args$scales %||% "free"
    facet_call <- do.call("facet_wrap", facet_args)
  }

  ggplot(data, mapping) +
    bayesplot_theme_get() +
    layer_warmup +
    layer_draws +
    coord_window +
    scale_alpha +
    scale_color +
    div_rug +
    div_guides +
    facet_call +
    scale_x_continuous(breaks = pretty) +
    legend_move(ifelse(n_chain > 1, "right", "none")) +
    xaxis_title(FALSE) +
    yaxis_title(on = n_param == 1)
}

chain_colors <- function(n) {
  all_clrs <- unlist(color_scheme_get())
  clrs <- switch(
    as.character(n),
    "1" = get_color("m"),
    "2" = get_color(c("l", "d")),
    "3" = get_color(c("l", "m", "d")),
    "4" = all_clrs[-c(2, 4)],
    "5" = all_clrs[-3],
    "6" = all_clrs,
    rep_len(all_clrs, n)
  )
  unname(rev(clrs))
}


#' Add divergences to trace plot using geom_rug
#'
#' @noRd
#' @param np User's `np` argument, if specified.
#' @param np_style User's `np_style` argument, if specified.
#' @param n_iter Number of iterations in the trace plot (to check against number
#'   of iterations provided in `np`).
#' @param n_chain Number of chains in the trace plot (to check against number of
#'   chains provided in `np`).
#' @return Object returned by `ggplot2::geom_rug()`.
#'
#' @importFrom dplyr summarise group_by select
divergence_rug <- function(np, np_style, n_iter, n_chain) {
  if (is.data.frame(np)) {
    np <- validate_nuts_data_frame(np)
    stopifnot(num_iters(np) == n_iter, num_chains(np) == n_chain)

    iter <- sym("Iteration")
    val <- sym("Value")
    param <- sym("Parameter")
    divg <- sym("Divergent")

    div_info <- np %>%
      dplyr::filter(UQ(param) == "divergent__") %>%
      group_by(!! iter) %>%
      summarise(
        Divergent = ifelse(sum(!! val) > 0, !! iter, NA)
      ) %>%
      select(!! divg)

  } else {
    # not using a data frame is deprecated but maintain backwards
    # compatibility for now

    divergences <- np
    stopifnot(
      is_vector_or_1Darray(divergences),
      length(divergences) == n_iter,
      all(divergences %in% c(0, 1))
    )
    divergences <- ifelse(divergences == 1, seq_along(divergences), NA)
    div_info <- data.frame(Divergent = divergences)
  }

  if (all(is.na(div_info$Divergent))) {
    inform("No divergences to plot.")
    return(NULL)
  }

  geom_rug(
    aes(x = .data$Divergent, linetype = "Divergence"),
    data = div_info,
    na.rm = TRUE,
    inherit.aes = FALSE,
    sides = "b",
    color = np_style$color[["div"]],
    linewidth = np_style$size[["div"]],
    alpha = np_style$alpha[["div"]]
  )
}
