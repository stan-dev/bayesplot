#' Trace plot (time series plot) of MCMC draws
#'
#' Trace plot (or traceplot) of MCMC draws. See the \strong{Plot Descriptions}
#' section, below, for details.
#'
#' @name MCMC-traces
#' @family MCMC
#'
#' @template args-mcmc-x
#' @template args-pars
#' @template args-regex_pars
#' @template args-transformations
#' @template args-facet_args
#' @param ... Currently ignored.
#' @param size An optional value to override the default line size
#'   (\code{mcmc_trace}) or the default point size
#'   (\code{mcmc_trace_highlight}).
#' @param alpha For \code{mcmc_trace_highlight}, passed to
#'   \code{\link[ggplot2]{geom_point}} to control the transparency of the points
#'   for the chains not highlighted.
#' @param n_warmup An integer; the number of warmup iterations included in
#'   \code{x}. The default is \code{n_warmup = 0}, i.e. to assume no warmup
#'   iterations are included. If \code{n_warmup > 0} then the background for
#'   iterations \code{1:n_warmup} is shaded gray.
#' @param iter1 An integer; the iteration number of the first included draw
#'   (default 0). This can be used to make it more obvious that the warmup
#'   iterations have been discarded from the traceplot. It cannot be specified
#'   if \code{n_warmup} is also set to a positive value.
#' @param window An integer vector of length two specifying the limits of a
#'   range of iterations to display.
#' @param np For models fit using \code{\link{NUTS}} (more generally, any
#'   \href{http://en.wikipedia.org/wiki/Symplectic_integrator}{symplectic
#'   integrator}), an optional data frame providing NUTS diagnostic
#'   information. The data frame should be the object returned by
#'   \code{\link{nuts_params}} or one with the same structure. If \code{np} is
#'   specified then tick marks are added to the bottom of the trace plot
#'   indicating within which iterations there was a divergence (if there were any).
#'   See the end of the \strong{Examples} section, below.
#' @param np_style A call to the \code{trace_style_np} helper function to
#'   specify arguments controlling the appearance of tick marks representing
#'   divergences (if the \code{np} argument is specified).
#' @param divergences Deprecated. Use the \code{np} argument instead.
#'
#' @template return-ggplot
#'
#' @section Plot Descriptions:
#' \describe{
#'   \item{\code{mcmc_trace}}{
#'    Standard trace plots of MCMC draws. For models fit using \code{\link{NUTS}},
#'    the \code{np} argument can be used to also show divergences on the trace plot.
#'   }
#'   \item{\code{mcmc_trace_highlight}}{
#'    Traces are plotted using points rather than lines and the opacity of all
#'    chains but one (specified by the \code{highlight} argument) is reduced.
#'   }
#' }
#'
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
#' fit <- stan_glm(mpg ~ ., data = mtcars,
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
#'
#' color_scheme_set("viridis")
#' mcmc_trace(
#'   posterior,
#'   pars = c("wt", "sigma"),
#'   size = 0.8,
#'   facet_args = list(nrow = 2),
#'   divergences = nuts_params(fit),
#'   div_color = "black"
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
           facet_args = list(),
           ...,
           n_warmup = 0,
           iter1 = 0,
           window = NULL,
           size = NULL,
           np = NULL,
           np_style = trace_style_np(),
           divergences = NULL) {

    # deprecate 'divergences' arg in favor of 'np' (for consistency across functions)
    if (!is.null(divergences)) {
      warning(
        "The 'divergences' argument is deprecated ",
        "and will be removed in a future release. ",
        "Use the 'np' argument instead."
      )

      if (is.null(np)) {
        np <- divergences
      } else {
        stop(
          "'np' and 'divergences' can't both be specified. ",
          "Use only 'np' (the 'divergences' argument is deprecated)."
        )
      }
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
#' @param highlight For \code{mcmc_trace_highlight}, an integer specifying one
#'   of the chains that will be more visible than the others in the plot.
mcmc_trace_highlight <-
  function(x,
           pars = character(),
           regex_pars = character(),
           transformations = list(),
           facet_args = list(),
           ...,
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
#'   \code{trace_style_np} helper function that are eventually passed to
#'   \code{\link[ggplot2]{geom_rug}} if the \code{np} argument is also
#'   specified. They control the color, size, and transparency specifications
#'   for showing divergences in the plot. The default values are displayed in
#'   the \strong{Usage} section above.
trace_style_np <-
  function(div_color = "red",
           div_size = 0.25,
           div_alpha = 1) {
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
    x, pars = pars, regex_pars = regex_pars, transformations = transformations,
    highlight = highlight, n_warmup = n_warmup, iter1 = iter1, window = window,
    np = np, np_style = np_style
  )
  n_iter <- unique(data$n_iterations)
  n_chain <- unique(data$n_chains)
  n_param <- unique(data$n_parameters)

  mapping <- aes_(
    x = ~ iteration,
    y = ~ value,
    color = ~ chain
  )

  if (!is.null(highlight)) {
    mapping <- modify_aes_(
      mapping,
      alpha = ~ highlight,
      color = ~ highlight
    )
  }

  layer_warmup <- if (n_warmup > 0) {
    layer_warmup <- annotate(
      "rect", xmin = -Inf, xmax = n_warmup, ymin = -Inf, ymax = Inf, size = 1,
      color = "gray88", fill = "gray88", alpha = 0.5)
  } else {
    NULL
  }

  geom_args <- list()
  geom_args$size <- size %||% ifelse(style == "line", 1/3, 1)
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
      if (!is.null(div_rug))
        div_guides <- guides(
          color = guide_legend(order = 1),
          linetype = guide_legend(
            order = 2, title = NULL, keywidth = rel(1/2),
            override.aes = list(size = rel(1/2)))
        )
    }
  }


  facet_call <- NULL
  if (n_param == 1) {
    facet_call <- ylab(levels(data$parameter))
  } else {
    facet_args$facets <- ~ parameter
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


mcmc_trace_data <- function(x,
                            pars = character(),
                            regex_pars = character(),
                            transformations = list(),
                            facet_args = list(),
                            ...,
                            highlight = NULL,
                            n_warmup = 0,
                            iter1 = 0,
                            window = NULL,
                            size = NULL,
                            np = NULL,
                            np_style = trace_style_np()) {

  check_ignored_arguments(...)

  x <- prepare_mcmc_array(x, pars, regex_pars, transformations)

  if (iter1 < 0) {
    stop("'iter1' cannot be negative.")
  }

  if (n_warmup > 0 && iter1 > 0) {
    stop("'n_warmup' and 'iter1' can't both be specified.")
  }

  if (!is.null(highlight)) {
    stopifnot(length(highlight) == 1)

    if (!has_multiple_chains(x)){
      STOP_need_multiple_chains()
    }

    if (!highlight %in% seq_len(ncol(x)))
      stop(
        "'highlight' is ", highlight,
        ", but 'x' contains ", ncol(x), " chains."
      )
  }

  ## @todo: filter to just window?

  data <- melt_mcmc(x)
  data$Chain <- factor(data$Chain)
  data$n_chains <- num_chains(data)
  data$n_iterations <- num_iters(data)
  data$n_parameters <- num_chains(data)
  data <- rlang::set_names(data, tolower)

  data <- data %>%
    group_by(.data$parameter) %>%
    mutate(value_rank = dplyr::row_number(.data$value)) %>%
    ungroup()

  data$highlight <- if (!is.null(highlight)) {
    data$chain == highlight
  } else {
    FALSE
  }

  data$warmup <- data$iteration <= n_warmup
  data$iteration <- data$iteration + iter1

  tibble::as_tibble(data)
}

# Add divergences to trace plot using geom_rug
#
# @param np User's 'np' argument, if specified.
# @param np_style User's 'np_style' argument, if specified.
# @param n_iter Number of iterations in the trace plot (to check against number
#   of iterations provided in 'np').
# @param n_chain Number of chains in the trace plot (to check against number
#   of chains provided in 'np').
# @return Object returned by geom_rug.
#
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
    message("No divergences to plot.")
    return(NULL)
  }

  geom_rug(
    aes_(x = ~ Divergent, linetype = "Divergence"),
    data = div_info,
    na.rm = TRUE,
    inherit.aes = FALSE,
    sides = "b",
    color = np_style$color[["div"]],
    size = np_style$size[["div"]],
    alpha = np_style$alpha[["div"]]
  )
}
