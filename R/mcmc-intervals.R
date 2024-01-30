#' Plot interval estimates from MCMC draws
#'
#' Plot central (quantile-based) posterior interval estimates from MCMC draws.
#' See the **Plot Descriptions** section, below, for details.
#'
#' @name MCMC-intervals
#' @family MCMC
#'
#' @template args-mcmc-x
#' @template args-pars
#' @template args-regex_pars
#' @template args-transformations
#' @param ... Currently unused.
#' @param prob The probability mass to include in the inner interval (for
#'   `mcmc_intervals()`) or in the shaded region (for `mcmc_areas()`). The
#'   default is `0.5` (50% interval) and `1` for `mcmc_areas_ridges()`.
#' @param prob_outer The probability mass to include in the outer interval. The
#'   default is `0.9` for `mcmc_intervals()` (90% interval) and
#'   `1` for `mcmc_areas()` and for `mcmc_areas_ridges()`.
#' @param area_method How to constrain the areas in `mcmc_areas()`. The
#'   default is `"equal area"`, setting the density curves to have the same
#'   area. With `"equal height"`, the curves are scaled so that the highest
#'   points across the curves are the same height. The method `"scaled
#'   height"` tries a compromise between to the two: the heights from
#'   `"equal height"` are scaled using `height*sqrt(height)`
#' @param point_est The point estimate to show. Either `"median"` (the
#'   default), `"mean"`, or `"none"`.
#' @param inner_size,outer_size For `mcmc_intervals()`, the size of
#'   the inner and interval segments, respectively.
#' @param point_size For `mcmc_intervals()`, the size of point estimate.
#' @param rhat An optional numeric vector of R-hat estimates, with one element
#'   per parameter included in `x`. If `rhat` is provided, the intervals/areas
#'   and point estimates in the resulting plot are colored based on R-hat value.
#'   See [rhat()] for methods for extracting R-hat estimates.
#' @template args-density-controls
#'
#' @template return-ggplot-or-data
#'
#' @section Plot Descriptions:
#' \describe{
#'   \item{`mcmc_intervals()`}{
#'    Plots of uncertainty intervals computed from posterior draws with all
#'    chains merged.
#'   }
#'   \item{`mcmc_areas()`}{
#'    Density plots computed from posterior draws with all chains merged,
#'    with uncertainty intervals shown as shaded areas under the curves.
#'   }
#'   \item{`mcmc_areas_ridges()`}{
#'    Density plot, as in `mcmc_areas()`, but drawn with overlapping
#'    ridgelines. This plot provides a compact display of (hierarchically)
#'    related distributions.
#'   }
#' }
#'
#' @examples
#' set.seed(9262017)
#'
#' # load ggplot2 to use its functions to modify our plots
#' library(ggplot2)
#'
#' # some parameter draws to use for demonstration
#' x <- example_mcmc_draws(params = 6)
#' dim(x)
#' dimnames(x)
#'
#' color_scheme_set("brightblue")
#' mcmc_intervals(x)
#' mcmc_intervals(x, pars = c("beta[1]", "beta[2]"))
#' mcmc_areas(x, regex_pars = "beta\\[[1-3]\\]",  prob = 0.8) +
#'  labs(
#'    title = "Posterior distributions",
#'    subtitle = "with medians and 80% intervals"
#'  )
#'
#' color_scheme_set("red")
#' p <- mcmc_areas(
#'    x,
#'    pars = c("alpha", "beta[4]"),
#'    prob = 2/3,
#'    prob_outer = 0.9,
#'    point_est = "mean",
#'    border_size = 1.5 # make the ridgelines fatter
#' )
#' plot(p)
#'
#' \donttest{
#' # control spacing at top and bottom of plot
#' # see ?ggplot2::expansion
#' p + scale_y_discrete(
#'   limits = c("beta[4]", "alpha"),
#'   expand = expansion(add = c(1, 2))
#' )
#' p + scale_y_discrete(
#'   limits = c("beta[4]", "alpha"),
#'   expand = expansion(add = c(.1, .3))
#' )
#'
#' # relabel parameters
#' p + scale_y_discrete(
#'   labels = c("alpha" = "param label 1",
#'              "beta[4]" = "param label 2")
#')
#'
#' # relabel parameters and define the order
#' p + scale_y_discrete(
#'   labels = c("alpha" = "param label 1",
#'              "beta[4]" = "param label 2"),
#'   limits = c("beta[4]", "alpha")
#' )
#'
#' # color by rhat value
#' color_scheme_set("blue")
#' fake_rhat_values <- c(1, 1.07, 1.3, 1.01, 1.15, 1.005)
#' mcmc_intervals(x, rhat = fake_rhat_values)
#'
#' # get the dataframe that is used in the plotting functions
#' mcmc_intervals_data(x)
#' mcmc_intervals_data(x, rhat = fake_rhat_values)
#' mcmc_areas_data(x, pars = "alpha")
#'
#' color_scheme_set("gray")
#' p <- mcmc_areas(x, pars = c("alpha", "beta[4]"), rhat = c(1, 1.1))
#' p + legend_move("bottom")
#' p + legend_move("none") # or p + legend_none()
#'
#' }
#'
#' # Different area calculations
#' b3 <- c("beta[1]", "beta[2]", "beta[3]")
#'
#' mcmc_areas(x, pars = b3, area_method = "equal area") +
#'   labs(
#'     title = "Curves have same area",
#'     subtitle = "A wide, uncertain interval is spread thin when areas are equal"
#'    )
#'
#' mcmc_areas(x, pars = b3, area_method = "equal height") +
#'   labs(
#'     title = "Curves have same maximum height",
#'     subtitle = "Local curvature is clearer but more uncertain curves use more area"
#'   )
#'
#' mcmc_areas(x, pars = b3, area_method = "scaled height") +
#'   labs(
#'     title = "Same maximum heights but heights scaled by square-root",
#'     subtitle = "Compromise: Local curvature is accentuated and less area is used"
#'    )
#'
#' \donttest{
#' # apply transformations
#' mcmc_intervals(
#'   x,
#'   pars = c("beta[2]", "sigma"),
#'   transformations = list("sigma" = "log", "beta[2]" = function(x) x + 3)
#' )
#'
#' # apply same transformation to all selected parameters
#' mcmc_intervals(x, regex_pars = "beta", transformations = "exp")
#' }
#'
#' \dontrun{
#' # example using fitted model from rstanarm package
#' library(rstanarm)
#' fit <- stan_glm(
#'  mpg ~ 0 + wt + factor(cyl),
#'  data = mtcars,
#'  iter = 500,
#'  refresh = 0
#' )
#' x <- as.matrix(fit)
#'
#' color_scheme_set("teal")
#' mcmc_intervals(x, point_est = "mean", prob = 0.8, prob_outer = 0.95)
#' mcmc_areas(x, regex_pars = "cyl", bw = "SJ",
#'            rhat = rhat(fit, regex_pars = "cyl"))
#' }
#'
#' \dontrun{
#' # Example of hierarchically related parameters
#' # plotted with ridgelines
#' m <- shinystan::eight_schools@posterior_sample
#' mcmc_areas_ridges(m, pars = "mu", regex_pars = "theta", border_size = 0.75) +
#'   ggtitle("Treatment effect on eight schools (Rubin, 1981)")
#' }
#'
NULL

#' @rdname MCMC-intervals
#' @export
mcmc_intervals <- function(x,
                           pars = character(),
                           regex_pars = character(),
                           transformations = list(),
                           ...,
                           prob = 0.5,
                           prob_outer = 0.9,
                           point_est = c("median", "mean", "none"),
                           outer_size = 0.5,
                           inner_size = 2,
                           point_size = 4,
                           rhat = numeric()) {
  check_ignored_arguments(...)

  data <- mcmc_intervals_data(x, pars, regex_pars, transformations,
                              prob = prob, prob_outer = prob_outer,
                              point_est = point_est, rhat = rhat)

  color_by_rhat <- rlang::has_name(data, "rhat_rating")
  no_point_est <- all(data$point_est == "none")

  x_lim <- range(c(data$ll, data$hh))
  x_range <- diff(x_lim)
  x_lim[1] <- x_lim[1] - 0.05 * x_range
  x_lim[2] <- x_lim[2] + 0.05 * x_range

  # faint vertical line at zero if zero is within x_lim
  layer_vertical_line <- if (0 > x_lim[1] && 0 < x_lim[2]) {
    vline_0(color = "gray90", linewidth = 0.5)
  } else {
    geom_ignore()
  }

  args_outer <- list(
    mapping = aes(x = .data$ll, xend = .data$hh, y = .data$parameter, yend = .data$parameter),
    color = get_color("mid"),
    linewidth = outer_size
  )
  args_inner <- list(
    mapping = aes(x = .data$l, xend = .data$h, y = .data$parameter, yend = .data$parameter),
    linewidth = inner_size,
    show.legend = FALSE
  )
  args_point <- list(
    mapping = aes(x = .data$m, y = .data$parameter),
    data = data,
    size = point_size,
    shape = 21
  )

  if (color_by_rhat) {
    args_inner$mapping <- args_inner$mapping %>%
      modify_aes(color = .data$rhat_rating)
    args_point$mapping <- args_point$mapping %>%
      modify_aes(color = .data$rhat_rating,
                 fill = .data$rhat_rating)
  } else {
    args_inner$color <- get_color("dark")
    args_point$color <- get_color("dark_highlight")
    args_point$fill <- get_color("light")
  }

  point_func <- if (no_point_est) geom_ignore else geom_point

  layer_outer <- do.call(geom_segment, args_outer)
  layer_inner <- do.call(geom_segment, args_inner)
  layer_point <- do.call(point_func, args_point)

  # Do something or add an invisible layer
  if (color_by_rhat) {
    scale_color <- scale_color_diagnostic("rhat")
    scale_fill <- scale_fill_diagnostic("rhat")
  } else {
    scale_color <- geom_ignore()
    scale_fill <- geom_ignore()
  }

  ggplot(data) +
    layer_vertical_line +
    layer_outer +
    layer_inner +
    layer_point +
    scale_color +
    scale_fill +
    scale_y_discrete(limits = unique(rev(data$parameter))) +
    xlim(x_lim) +
    bayesplot_theme_get() +
    legend_move(ifelse(color_by_rhat, "top", "none")) +
    yaxis_text(face = "bold") +
    yaxis_title(FALSE) +
    yaxis_ticks(linewidth = 1) +
    xaxis_title(FALSE)
}


#' @rdname MCMC-intervals
#' @export
#' @param border_size For `mcmc_areas()` and `mcmc_areas_ridges()`, the size of
#'   the ridgelines.
mcmc_areas <- function(x,
                       pars = character(),
                       regex_pars = character(),
                       transformations = list(),
                       ...,
                       area_method = c("equal area", "equal height", "scaled height"),
                       prob = 0.5,
                       prob_outer = 1,
                       point_est = c("median", "mean", "none"),
                       rhat = numeric(),
                       border_size = NULL,
                       bw = NULL,
                       adjust = NULL,
                       kernel = NULL,
                       n_dens = NULL) {
  check_ignored_arguments(...)
  area_method <- match.arg(area_method)

  data <- mcmc_areas_data(
    x, pars, regex_pars, transformations,
    prob = prob, prob_outer = prob_outer,
    point_est = point_est, rhat = rhat,
    bw = bw, adjust = adjust, kernel = kernel, n_dens = n_dens
  )
  datas <- split(data, data$interval)

  # Use a dummy empty dataframe if no point estimate
  no_point_est <- !rlang::has_name(datas, "point")
  datas$point <- if (no_point_est) {
    dplyr::filter(datas$inner, FALSE)
  } else {
    datas$point
  }

  color_by_rhat <- rlang::has_name(data, "rhat_rating")

  # faint vertical line at zero if zero is within x_lim
  x_lim <- range(datas$outer$x)
  x_range <- diff(x_lim)
  x_lim[1] <- x_lim[1] - 0.05 * x_range
  x_lim[2] <- x_lim[2] + 0.05 * x_range

  layer_vertical_line <- if (0 > x_lim[1] && 0 < x_lim[2]) {
    vline_0(color = "gray90", linewidth = 0.5)
  } else {
    geom_ignore()
  }

  # Need to include rhat rating as a grouping variable if coloring by rhat so
  # that datas$bottom has an rhat_rating column that can map to color aesthetic
  groups <- if (color_by_rhat) {
    rlang::syms(c("parameter", "rhat_rating"))
  } else {
    rlang::syms(c("parameter"))
  }

  datas$bottom <- datas$outer %>%
    group_by(!!! groups) %>%
    summarise(
      ll = min(.data$x),
      hh = max(.data$x),
      .groups = "drop_last"
    ) %>%
    ungroup()

  args_bottom <- list(
    mapping = aes(x = .data$ll, xend = .data$hh, yend = .data$parameter),
    data = datas$bottom
  )
  args_inner <- list(
    mapping = aes(scale = 0.9),
    data = datas$inner
  )
  args_point <- list(
    mapping = aes(scale = 0.9),
    data = datas$point,
    color = NA
  )
  args_outer <- list(
    mapping = aes(scale = 0.9),
    fill = NA
  )
  if (area_method == "equal height") {
    args_inner$mapping <- modify_aes(args_inner$mapping, height = .data$scaled_density)
    args_point$mapping <- modify_aes(args_point$mapping, height = .data$scaled_density)
    args_outer$mapping <- modify_aes(args_outer$mapping, height = .data$scaled_density)
  } else if (area_method == "scaled height") {
    args_inner$mapping <- modify_aes(args_inner$mapping, height = .data$scaled_density * sqrt(.data$scaled_density))
    args_point$mapping <- modify_aes(args_point$mapping, height = .data$scaled_density * sqrt(.data$scaled_density))
    args_outer$mapping <- modify_aes(args_outer$mapping, height = .data$scaled_density * sqrt(.data$scaled_density))
  } else {
    args_inner$mapping <- modify_aes(args_inner$mapping, height = .data$plotting_density)
    args_point$mapping <- modify_aes(args_point$mapping, height = .data$plotting_density)
    args_outer$mapping <- modify_aes(args_outer$mapping, height = .data$plotting_density)
  }

  if (!is.null(border_size)) {
    args_bottom$linewidth <- border_size
    args_outer$linewidth <- border_size
    args_inner$linewidth <- border_size
  }

  if (color_by_rhat) {
    args_bottom$mapping <- args_bottom$mapping %>%
      modify_aes(color = .data$rhat_rating)
    args_inner$mapping <- args_inner$mapping %>%
      modify_aes(color = .data$rhat_rating,
                  fill = .data$rhat_rating)
    args_outer$mapping <- args_outer$mapping %>%
      modify_aes(color = .data$rhat_rating)
    # rhat fill color scale uses light/mid/dark colors. The point estimate needs
    # to be drawn with highlighted color scale, so we manually set the color for
    # the rhat fills.
    dc <- diagnostic_colors("rhat", "color")[["values"]]
    args_point$fill <- dc[datas$point$rhat_rating]
  } else {
    args_bottom$color <- get_color("dark")
    args_inner$color <- get_color("dark")
    args_inner$fill <- get_color("light")
    args_point$fill <- get_color("mid_highlight")
    args_outer$color <- get_color("dark")
  }

  # An invisible layer that is 2.5% taller than the plotted one
  args_outer2 <- args_outer
  args_outer2$mapping <- args_outer2$mapping %>%
    modify_aes(scale = .925)
  args_outer2$color <- NA

  layer_bottom <- do.call(geom_segment, args_bottom)
  layer_inner <- do.call(ggridges::geom_ridgeline, args_inner)
  layer_outer <- do.call(ggridges::geom_ridgeline, args_outer)
  layer_outer2 <- do.call(ggridges::geom_ridgeline, args_outer2)

  point_geom <- if (no_point_est) {
    geom_ignore
  } else {
    ggridges::geom_ridgeline
  }
  layer_point <- do.call(point_geom, args_point)

  # Do something or add an invisible layer
  if (color_by_rhat) {
    scale_color <- scale_color_diagnostic("rhat")
    scale_fill <- scale_fill_diagnostic("rhat")
  } else {
    scale_color <- geom_ignore()
    scale_fill <- geom_ignore()
  }

  ggplot(datas$outer) +
    aes(x = .data$x, y = .data$parameter) +
    layer_vertical_line +
    layer_inner +
    layer_point +
    layer_outer +
    layer_outer2 +
    layer_bottom +
    scale_color +
    scale_fill +
    scale_y_discrete(
      limits = unique(rev(data$parameter)),
      expand = expansion(
        add = c(0, 0.5 + 1/(2 * nlevels(data$parameter))),
        mult = c(0.05, 1/(2 * nlevels(data$parameter)))
      )
    ) +
    xlim(x_lim) +
    bayesplot_theme_get() +
    legend_move(ifelse(color_by_rhat, "top", "none")) +
    yaxis_text(face = "bold") +
    yaxis_title(FALSE) +
    yaxis_ticks(linewidth = 1) +
    xaxis_title(FALSE)
}

#' @rdname MCMC-intervals
#' @export
mcmc_areas_ridges <- function(x,
                             pars = character(),
                             regex_pars = character(),
                             transformations = list(),
                             ...,
                             prob_outer = 1,
                             prob = 1,
                             border_size = NULL,
                             bw = NULL, adjust = NULL, kernel = NULL,
                             n_dens = NULL) {
  check_ignored_arguments(...)
  data <- mcmc_areas_ridges_data(x, pars = pars, regex_pars = regex_pars,
                                 transformations = transformations,
                                 prob = prob, prob_outer = prob_outer,
                                 bw = bw, adjust = adjust, kernel = kernel,
                                 n_dens = n_dens)

  datas <- data %>%
    split(data$interval)

  # faint vertical line at zero if zero is within x_lim
  x_lim <- range(datas$outer$x)
  x_range <- diff(x_lim)
  x_lim[1] <- x_lim[1] - 0.05 * x_range
  x_lim[2] <- x_lim[2] + 0.05 * x_range

  layer_vertical_line <- if (0 > x_lim[1] && 0 < x_lim[2]) {
    vline_0(color = "gray90", linewidth = 0.5)
  } else {
    geom_ignore()
  }

  args_outer <- list(
    mapping = aes(height = .data$density),
    color = get_color("dark"),
    fill = NA,
    stat = "identity"
  )
  if (!is.null(border_size)) {
    args_outer$linewidth <- border_size
  }

  layer_outer <- do.call(ggridges::geom_density_ridges, args_outer)

  # Force ggridges to compute the scaling now
  test_plot <- ggplot(datas$outer) +
    aes(x = .data$x, y = .data$parameter) +
    layer_outer

  soft_build <- ggplot_build(test_plot)
  scaler1 <- unique(soft_build$data[[1]][["scale"]])
  scaler2 <- unique(soft_build$data[[1]][["iscale"]])
  scale <- scaler1 * scaler2

  # Draw each ridgeline from top the bottom
  layer_list_inner <- list()
  par_draw_order <- levels(unique(data$parameter))
  bg <- bayesplot_theme_get()[["panel.background"]][["fill"]] %||% "white"

  for (par_num in seq_along(unique(data$parameter))) {
    # Basically, draw the current ridgeline normally, but draw all the ones
    # under it (which would overlap it vertically) with a blank fill
    this_par <- par_draw_order[par_num]
    next_pars <- par_draw_order[par_num < seq_along(par_draw_order)]

    this_par_data <- datas$inner %>%
      dplyr::filter(.data$parameter == this_par) %>%
      mutate(color = get_color("dark"), fill = get_color("light"))

    next_par_data <- datas$outer %>%
      dplyr::filter(.data$parameter %in% next_pars) %>%
      mutate(color = get_color("dark"), fill = bg)

    args_inner <- list(
        mapping = aes(height = .data$density, color = .data$color, fill = .data$fill),
        data = dplyr::bind_rows(this_par_data, next_par_data),
        scale = scale,
        stat = "identity")

    if (!is.null(border_size)) {
      args_inner$linewidth <- border_size
    }

    layer_list_inner[[par_num]] <- do.call(ggridges::geom_ridgeline, args_inner)
  }

  ggplot(datas$outer) +
    aes(x = .data$x, y = .data$parameter) +
    layer_outer +
    scale_y_discrete(limits = unique(rev(data$parameter)),
                     expand = expansion(
                       add = c(0, 1.4 + 1/(2 * nlevels(data$parameter))),
                       mult = c(0.05, 1/(2 * nlevels(data$parameter)))
                     )) +
    layer_list_inner +
    layer_vertical_line +
    scale_fill_identity() +
    scale_color_identity() +
    xlim(x_lim) +
    yaxis_title(FALSE) +
    xaxis_title(FALSE) +
    bayesplot_theme_get() +
    grid_lines_y(color = "gray90") +
    theme(axis.text.y = element_text(hjust = 1, vjust = 0))
}



#' @rdname MCMC-intervals
#' @export
mcmc_intervals_data <- function(x,
                                pars = character(),
                                regex_pars = character(),
                                transformations = list(),
                                ...,
                                prob = 0.5,
                                prob_outer = 0.9,
                                point_est = c("median", "mean", "none"),
                                rhat = numeric()) {
  check_ignored_arguments(...)
  probs <- check_interval_widths(prob, prob_outer)
  prob <- probs[1]
  prob_outer <- probs[2]

  x <- prepare_mcmc_array(x, pars, regex_pars, transformations)
  x <- merge_chains(x)

  data_long <- melt_mcmc(x) %>%
    dplyr::as_tibble() %>%
    rlang::set_names(tolower)

  probs <- c(0.5 - prob_outer / 2,
             0.5 - prob / 2,
             0.5 + prob / 2,
             0.5 + prob_outer / 2)

  point_est <- match.arg(point_est)
  m_func <- if (point_est == "mean") mean else median

  data <- data_long %>%
    group_by(.data$parameter) %>%
    summarise(
      outer_width = prob_outer,
      inner_width = prob,
      point_est = point_est,
      ll = unname(quantile(.data$value, probs[1])),
      l  = unname(quantile(.data$value, probs[2])),
      m  = m_func(.data$value),
      h  = unname(quantile(.data$value, probs[3])),
      hh = unname(quantile(.data$value, probs[4]))
    )

  if (point_est == "none") {
    data$m <- NULL
  }

  color_by_rhat <- isTRUE(length(rhat) > 0)

  if (color_by_rhat) {
    rhat <- drop_NAs_and_warn(new_rhat(rhat))

    if (length(rhat) != nrow(data)) {
      abort(paste(
        "'rhat' has length", length(rhat),
        "but 'x' has", nrow(data), "parameters."
      ))
    }

    rhat <- set_names(rhat, data$parameter)

    rhat_tbl <- rhat %>%
      mcmc_rhat_data() %>%
      select(one_of("parameter"),
             rhat_value = "value",
             rhat_rating = "rating",
             rhat_description = "description") %>%
      mutate(parameter = factor(.data$parameter, levels(data$parameter)))

    data <- dplyr::inner_join(data, rhat_tbl, by = "parameter")
  }

  data
}


# Don't import `filter`: otherwise, you get a warning when using
# `devtools::load_all(".")` because stats also has a `filter` function

#' @importFrom dplyr inner_join one_of top_n
#' @rdname MCMC-intervals
#' @export
mcmc_areas_data <- function(x,
                            pars = character(),
                            regex_pars = character(),
                            transformations = list(),
                            ...,
                            prob = 0.5,
                            prob_outer = 1,
                            point_est = c("median", "mean", "none"),
                            rhat = numeric(),
                            bw = NULL,
                            adjust = NULL,
                            kernel = NULL,
                            n_dens = NULL) {
  probs <- check_interval_widths(prob, prob_outer)

  # First compute normal intervals so we know the width of the data, point
  # estimates, and have prepared rhat values.

  # Compute intervals with a median (for now) if no point estimate. It will be
  # cleaner to ignore results later than to have two branching code paths.
  point_est <- match.arg(point_est)
  temp_point_est <- if (point_est == "none") "median" else point_est

  intervals <- mcmc_intervals_data(x, pars, regex_pars, transformations,
                                   prob = probs[1],  prob_outer = probs[2],
                                   point_est = temp_point_est, rhat = rhat)

  x <- prepare_mcmc_array(x, pars, regex_pars, transformations)
  x <- merge_chains(x)

  data_long <- melt_mcmc(x) %>%
    dplyr::as_tibble() %>%
    rlang::set_names(tolower)

  # Compute the density intervals
  data_inner <- data_long %>%
    compute_column_density(
      group_vars = "parameter",
      value_var = "value",
      interval_width = probs[1],
      bw = bw,
      adjust = adjust,
      kernel = kernel,
      n_dens = n_dens) %>%
    mutate(interval = "inner")

  data_outer <- data_long %>%
    compute_column_density(
      group_vars = "parameter",
      value_var = "value",
      interval_width = probs[2],
      bw = bw,
      adjust = adjust,
      kernel = kernel,
      n_dens = n_dens) %>%
    mutate(interval = "outer")

  # Point estimates will be intervals that take up .8% of the x-axis
  x_lim <- range(data_outer$x)
  x_range <- diff(x_lim)
  x_lim[1] <- x_lim[1] - 0.05 * x_range
  x_lim[2] <- x_lim[2] + 0.05 * x_range
  half_point_width <- .004 * diff(x_lim)

  # Find the density values closest to the point estimate
  point_ests <- intervals %>%
    select(one_of("parameter", "m"))

  point_centers <- data_inner %>%
    inner_join(point_ests, by = "parameter") %>%
    group_by(.data$parameter) %>%
    mutate(diff = abs(.data$m - .data$x)) %>%
    dplyr::top_n(1, -.data$diff) %>%
    select(one_of("parameter", "x", "m")) %>%
    rename(center = "x") %>%
    ungroup()

  # Keep density values that are within +/- .4% of x-axis of the point estimate
  points <- point_centers %>%
    left_join(data_inner, by = "parameter") %>%
    group_by(.data$parameter) %>%
    dplyr::filter(abs(.data$center - .data$x) <= half_point_width) %>%
    mutate(
      interval_width = 0,
      interval = "point"
    ) %>%
    select(-c("center"), "m") %>%
    ungroup()

  # Ignore points calculcation if no point estimate was requested
  if (point_est == "none") {
    points <- dplyr::filter(points, FALSE)
  }

  data <- dplyr::bind_rows(data_inner, data_outer, points) %>%
    select(one_of("parameter", "interval", "interval_width",
                  "x", "density", "scaled_density")) %>%
    # Density scaled so the highest in entire dataframe has height 1
    mutate(plotting_density = .data$density / max(.data$density))

  if (rlang::has_name(intervals, "rhat_value")) {
    rhat_info <- intervals %>%
      select(one_of("parameter", "rhat_value",
                    "rhat_rating", "rhat_description"))
    data <- inner_join(data, rhat_info, by = "parameter")
  }
  data
}


#' @rdname MCMC-intervals
#' @export
mcmc_areas_ridges_data <- function(x,
                                   pars = character(),
                                   regex_pars = character(),
                                   transformations = list(),
                                   ...,
                                   prob_outer = 1,
                                   prob = 1,
                                   bw = NULL,
                                   adjust = NULL, kernel = NULL,
                                   n_dens = NULL) {
  check_ignored_arguments(...)
  mcmc_areas_data(x, pars = pars, regex_pars = regex_pars,
                  transformations = transformations,
                  prob = prob, prob_outer = prob_outer, point_est = "none",
                  bw = bw, adjust = adjust, kernel = kernel, n_dens = n_dens)
}




# internal ----------------------------------------------------------------

#' Compute density for a dataframe column.
#'
#' @param df a dataframe of posterior samples
#' @param group_vars columns to group by. e.g., `c(Parameter, Chain)`
#' @param value_var column containing posterior samples
#' @param ... arguments passed onto density calculation
#' @noRd
compute_column_density <- function(df, group_vars, value_var, ...) {
  value_var <- enquo(value_var)
  group_vars <- enquos(group_vars)

  # Convert the vector of bare column names to a list of symbols
  group_cols <- df %>%
    dplyr::select(!!! group_vars) %>%
    names() %>%
    syms()

  # Tuck away the subgroups to compute densities on into nested dataframes
  sub_df <- dplyr::select(df, !!! group_cols, !! value_var)

  group_df <- df %>%
    dplyr::select(!!! group_cols, !! value_var) %>%
    group_by(!!! group_cols)

  by_group <- group_df %>%
    split(dplyr::group_indices(group_df)) %>%
    lapply(pull, !! value_var)

  nested <- df %>%
    dplyr::distinct(!!! group_cols) %>%
    mutate(data = by_group)

  nested$density <- lapply(nested$data, compute_interval_density, ...)
  nested$data <- NULL

  # Manually unnest the data
  reconstructed <- as.list(seq_len(nrow(nested)))
  for (df_i in seq_along(nested$density)) {
    row <- nested[df_i, ]
    parent <- row %>% select(-c("density"))
    groups <- rep(list(parent), nrow(row$density[[1]])) %>% dplyr::bind_rows()

    reconstructed[[df_i]] <- dplyr::bind_cols(groups, row$density[[1]])
  }

  dplyr::bind_rows(reconstructed)
}


# Given a vector of values, compute a density dataframe.
compute_interval_density <- function(x, interval_width = 1, n_dens = 1024,
                                     bw = NULL, adjust = NULL, kernel = NULL) {
  n_dens <- n_dens %||% 1024

  tail_width <- (1 - interval_width) / 2
  qs <- quantile(x, probs = c(tail_width, 1 - tail_width))

  args <- c(
    # can't be null
    list(x = x, from = min(qs), to = max(qs), n = n_dens),
    # might be null
    bw = bw, adjust = adjust, kernel = kernel)

  dens <- do.call(stats::density, args)

  data.frame(
    interval_width = interval_width,
    x = dens$x,
    density = dens$y,
    scaled_density =  dens$y / max(dens$y, na.rm = TRUE)
  )
}

check_interval_widths <- function(prob, prob_outer) {
  if (!(is.numeric(prob) && is.numeric(prob_outer)))
    abort("`prob` and `prob_outer` must be numeric")
  if (prob < 0 || prob > 1 || prob_outer < 0 || prob_outer > 1)
    abort("`prob` and `prob_outer` must be in [0,1].")
  if (prob_outer < prob) {
    x <- sprintf(
      "`prob_outer` (%s) is less than `prob` (%s)\n... %s",
      prob_outer,
      prob,
      "Swapping the values of `prob_outer` and `prob`"
    )
    warn(x)
  }
  sort(c(prob, prob_outer))
}
