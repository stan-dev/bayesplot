#' Plot interval estimates from MCMC draws
#'
#' Plot central (quantile-based) posterior interval estimates from MCMC draws.
#' See the \strong{Plot Descriptions} section, below, for details.
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
#'   \code{mcmc_intervals}) or in the shaded region (for \code{mcmc_areas}). The
#'   default is \code{0.5} (50\% interval).
#' @param prob_outer The probability mass to include in the outer interval. The
#'   default is \code{0.9} for \code{mcmc_intervals} (90\% interval)
#'   and \code{1} for \code{mcmc_areas}.
#' @param point_est The point estimate to show. Either \code{"median"} (the
#'   default), \code{"mean"}, or \code{"none"}.
#' @param rhat An optional numeric vector of \eqn{\hat{R}}{Rhat} estimates, with
#'   one element per parameter included in \code{x}. If \code{rhat} is provided,
#'   the intervals/areas and point estimates in the resulting plot are colored
#'   based on \eqn{\hat{R}}{Rhat} value. See \code{\link{rhat}} for methods for
#'   extracting \eqn{\hat{R}}{Rhat} estimates.
#' @param bw,adjust,kernel For \code{mcmc_areas}, optional arguments passed to
#'   \code{\link[stats]{density}} to override default kernel density estimation
#'   parameters.
#'
#' @template return-ggplot
#'
#' @section Plot Descriptions:
#' \describe{
#'   \item{\code{mcmc_intervals}}{
#'    Plots of uncertainty intervals computed from posterior draws with all
#'    chains merged.
#'   }
#'   \item{\code{mcmc_areas}}{
#'    Density plots computed from posterior draws with all chains merged,
#'    with uncertainty intervals shown as shaded areas under the curves.
#'   }
#' }
#'
#' @examples
#' # some parameter draws to use for demonstration
#' x <- example_mcmc_draws(params = 6)
#' dim(x)
#' dimnames(x)
#'
#' color_scheme_set("brightblue")
#' mcmc_intervals(x)
#' mcmc_intervals(x, pars = c("beta[1]", "beta[2]"))
#' mcmc_areas(x, regex_pars = "beta\\[[1-3]", prob = 0.8) +
#'  ggplot2::labs(
#'    title = "Posterior distributions",
#'    subtitle = "with medians and 80% intervals"
#'  )
#'
#' color_scheme_set("red")
#' mcmc_areas(
#'    x,
#'    pars = c("alpha", "beta[4]"),
#'    prob = 2/3,
#'    prob_outer = 0.9,
#'    point_est = "mean"
#' )
#'
#' # color by rhat value
#' color_scheme_set("blue")
#' fake_rhat_values <- c(1, 1.07, 1.3, 1.01, 1.15, 1.005)
#' mcmc_intervals(x, rhat = fake_rhat_values)
#'
#' color_scheme_set("gray")
#' p <- mcmc_areas(x, pars = c("alpha", "beta[4]"), rhat = c(1, 1.1))
#' p + legend_move("bottom")
#' p + legend_move("none") # or p + legend_none()
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
#'  iter = 500
#' )
#' x <- as.matrix(fit)
#'
#' color_scheme_set("teal")
#' mcmc_intervals(x, point_est = "mean", prob = 0.8, prob_outer = 0.95)
#' mcmc_areas(x, regex_pars = "cyl", bw = "SJ",
#'            rhat = rhat(fit, regex_pars = "cyl"))
#' }
#'
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
                           rhat = numeric()) {
  check_ignored_arguments(...)
  stopifnot(prob_outer >= prob)

  data <- mcmc_intervals_data(x, pars, regex_pars, transformations,
                              prob, prob_outer, point_est, rhat)

  no_point_est <- identical(data$point_est, "none")
  color_by_rhat <- rlang::has_name(data, "rhat_rating")

  x_lim <- range(c(data$ll, data$hh))
  x_range <- diff(x_lim)
  x_lim[1] <- x_lim[1] - 0.05 * x_range
  x_lim[2] <- x_lim[2] + 0.05 * x_range

  # faint vertical line at zero if zero is within x_lim
  layer_vertical_line <- if (0 > x_lim[1] && 0 < x_lim[2]) {
    vline_0(color = "gray90", size = 0.5)
  } else {
    geom_blank()
  }

  args_outer <- list(
    mapping = aes_(x = ~ ll, xend = ~ hh, y = ~ parameter, yend = ~ parameter),
    color = get_color("mid")
  )
  args_inner <- list(
    mapping = aes_(x = ~ l, xend = ~ h, y = ~ parameter, yend = ~ parameter),
    size = 2,
    show.legend = FALSE
    )
  args_point <- list(
    mapping = aes_(x = ~ m, y = ~ parameter),
    size = 4,
    shape = 21)

  if (color_by_rhat) {
    args_inner$mapping <- args_inner$mapping %>%
      modify_aes_(color = ~ rhat_rating)
    args_point$mapping <- args_point$mapping %>%
      modify_aes_(color = ~ rhat_rating, fill = ~ rhat_rating)
  } else {
    args_inner$color <- get_color("dark")
    args_point$color <- get_color("dark_highlight")
    args_point$fill <- get_color("light")
  }

  layer_outer <- do.call(geom_segment, args_outer)
  layer_inner <- do.call(geom_segment, args_inner)

  # Do something or add an invisible layer
  point_func <- if (!no_point_est) geom_point else geom_ignore
  layer_maybe_points <- do.call(point_func, args_point)
  scale_color <- if (color_by_rhat) scale_color_diagnostic("rhat") else NULL
  scale_fill <- if (color_by_rhat) scale_fill_diagnostic("rhat") else NULL

  ggplot(data) +
    layer_vertical_line +
    layer_outer +
    layer_inner +
    layer_maybe_points +
    scale_color +
    scale_fill +
    scale_y_discrete(limits = unique(rev(data$parameter))) +
    xlim(x_lim) +
    legend_move(ifelse(color_by_rhat, "top", "none")) +
    yaxis_text(face = "bold") +
    yaxis_title(FALSE) +
    yaxis_ticks(size = 1) +
    xaxis_title(FALSE)
}


#' @rdname MCMC-intervals
#' @export
mcmc_areas <- function(x,
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
                       kernel = NULL) {
  check_ignored_arguments(...)
  stopifnot(prob_outer >= prob)
  data <- mcmc_areas_data(x, pars, regex_pars, transformations,
                          prob = prob, prob_outer = prob_outer,
                          point_est = point_est, rhat = rhat,
                          bw = bw, adjust = adjust, kernel = kernel)
  datas <- split(data, data$interval)
  color_by_rhat <- rlang::has_name(data, "rhat_rating")

  # faint vertical line at zero if zero is within x_lim
  x_lim <- range(datas$outer$x)
  x_range <- diff(x_lim)
  x_lim[1] <- x_lim[1] - 0.05 * x_range
  x_lim[2] <- x_lim[2] + 0.05 * x_range

  layer_vertical_line <- if (0 > x_lim[1] && 0 < x_lim[2]) {
    vline_0(color = "gray90", size = 0.5)
  } else {
    geom_blank()
  }

  groups <- if (color_by_rhat) {
    rlang::syms(c("parameter", "rhat_rating"))
  } else {
    rlang::syms(c("parameter"))
  }

  datas$bottom <- datas$outer %>%
    group_by(!!! groups) %>%
    summarise(ll = min(.data$x), hh = max(.data$x)) %>%
    ungroup()

  args_bottom <- list(
    mapping = aes_(x = ~ ll, xend = ~ hh, yend = ~ parameter),
    data = datas$bottom
  )
  args_inner <- list(
    mapping = aes_(height = ~ density),
    data = datas$inner
  )
  args_point <- list(
    # Use group to draw a vertical line for each x value. This creates a
    # fill using the color scale
    mapping = aes_(height = ~ density),
    data = datas$point
  )
  args_outer <- list(
    mapping = aes_(height = ~ density),
    fill = NA
  )

  if (color_by_rhat) {
    args_bottom$mapping <- args_bottom$mapping %>%
      modify_aes_(color = ~ rhat_rating)
    args_inner$mapping <- args_inner$mapping %>%
      modify_aes_(color = ~ rhat_rating,
                  fill = ~ rhat_rating)
    args_point$mapping <- args_point$mapping %>%
      modify_aes_(color = ~ rhat_rating)
    args_outer$mapping <- args_outer$mapping %>%
      modify_aes_(color = ~ rhat_rating)
    # dc <- diagnostic_colors("rhat", "color")[["values"]]

  } else {
    args_bottom$color <- get_color("dark")
    args_inner$color <- get_color("dark")
    args_inner$fill <- get_color("light")
    args_outer$color <- get_color("dark")
    args_point$fill <- get_color("mid_highlight")
    args_point$color <- NA
  }

  layer_bottom <- do.call(geom_segment, args_bottom)
  layer_inner <- do.call(geom_area_ridges, args_inner)
  layer_point <- do.call(geom_area_ridges2, args_point)
  layer_outer <- do.call(geom_area_ridges, args_outer)

  # Do something or add an invisible layer
  scale_color <- if (color_by_rhat) scale_color_diagnostic("rhat") else NULL
  scale_fill <- if (color_by_rhat) scale_fill_diagnostic("rhat") else NULL

  ggplot(datas$outer) +
    aes_(x = ~ x, y = ~ parameter) +
    layer_vertical_line +
    layer_inner +
    layer_point +
    layer_outer +
    layer_bottom +
    scale_color +
    scale_fill +
    scale_y_discrete(limits = unique(rev(data$parameter))) +
    xlim(x_lim) +
    legend_move(ifelse(color_by_rhat, "top", "none")) +
    yaxis_text(face = "bold") +
    yaxis_title(FALSE) +
    yaxis_ticks(size = 1) +
    xaxis_title(FALSE)
}

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
  stopifnot(prob_outer >= prob)

  x <- prepare_mcmc_array(x, pars, regex_pars, transformations)
  x <- merge_chains(x)

  n_param <- ncol(x)
  parnames <- colnames(x)

  probs <- c(0.5 - prob_outer / 2,
             0.5 - prob / 2,
             0.5,
             0.5 + prob / 2,
             0.5 + prob_outer / 2)

  quantiles <- t(apply(x, 2, quantile, probs = probs))

  data <- dplyr::data_frame(parnames) %>%
    dplyr::bind_cols(dplyr::as_data_frame(quantiles)) %>%
    rlang::set_names(c("parameter", "ll", "l", "m", "h", "hh"))
  point_est <- match.arg(point_est)
  data$point_est <- point_est

  if (point_est == "mean") {
    data$m <- unname(colMeans(x))
  }

  color_by_rhat <- isTRUE(length(rhat) > 0)

  if (color_by_rhat) {
    if (length(rhat) != nrow(data)) {
      stop("'rhat' has length ", length(rhat),
           " but 'x' has ", nrow(data), " parameters.",
           call. = FALSE)
    }
    ## todo check if NAs need to be dropped here
    rhat <- new_rhat(rhat)
    rhat <- setNames(rhat, data$parameter)

    rhat_tbl <- rhat %>%
      mcmc_rhat_data() %>%
      select(parameter,
             rhat_value = .data$value,
             rhat_rating = .data$rating,
             rhat_description = .data$description) %>%
      mutate(parameter = as.character(.data$parameter))

    data <- dplyr::inner_join(data, rhat_tbl, by = "parameter")
  }

  data
}

# Don't import `filter`: otherwise, you get a warning when using
# `devtools::load_all(".")` because stats also a `filter` function

#' @importFrom dplyr inner_join one_of top_n
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
                            kernel = NULL) {
  probs <- sort(c(prob, prob_outer))
  intervals <- mcmc_intervals_data(x, pars, regex_pars, transformations,
                                   prob = probs[1],  prob_outer = probs[2],
                                   point_est = point_est, rhat = rhat)

  x <- prepare_mcmc_array(x, pars, regex_pars, transformations)
  x <- merge_chains(x)

  data_long <- melt_mcmc(x) %>%
    dplyr::as_tibble() %>%
    rlang::set_names(tolower)

  # Compute the density intervals
  data_inner <- data_long %>%
    compute_column_density(parameter, value, interval_width = probs[1]) %>%
    mutate(interval = "inner",
           parameter = as.character(.data$parameter))

  data_outer <- data_long %>%
    compute_column_density(parameter, value, interval_width = probs[2]) %>%
    mutate(interval = "outer",
           parameter = as.character(.data$parameter))

  # Point estimates will be intervals that take up .5% of the x-axis
  x_lim <- range(data_outer$x)
  x_range <- diff(x_lim)
  x_lim[1] <- x_lim[1] - 0.05 * x_range
  x_lim[2] <- x_lim[2] + 0.05 * x_range
  point_width <- .005 * diff(x_lim)

  # Find the density values closest to the point estimate
  point_ests <- intervals %>%
    select(one_of("parameter", "m"))

  point_centers <- data_inner %>%
    inner_join(point_ests, by = "parameter") %>%
    group_by(.data$parameter) %>%
    mutate(diff = abs(.data$m - .data$x)) %>%
    dplyr::top_n(1, -.data$diff) %>%
    select(one_of("parameter", "x", "m")) %>%
    rename(center = .data$x) %>%
    ungroup()

  # Keep density values that are within .5% of the x-axis of the point estimate
  points <- point_centers %>%
    left_join(data_inner, by = "parameter") %>%
    group_by(.data$parameter) %>%
    dplyr::filter(abs(.data$center - .data$x) <= point_width) %>%
    mutate(interval_width = 0,
           interval = "point") %>%
    select(-.data$center, .data$m) %>%
    ungroup()

  data <- dplyr::bind_rows(data_inner, data_outer, points) %>%
    select(one_of("parameter", "interval", "interval_width", "x", "density"))

  if (rlang::has_name(intervals, "rhat_value")) {
    rhat_info <- intervals %>%
      select(one_of("parameter", "rhat_value",
                    "rhat_rating", "rhat_description"))
    data <- inner_join(data, rhat_info, by = "parameter")
  }
  data
}

# internal ----------------------------------------------------------------

# @param x A matrix (not a 3-D array) created by merge_chains()
.mcmc_intervals <- function(data,
                            prob_inner = 0.5,
                            prob_outer = 0.95,
                            point_est = c("median", "mean", "none"),
                            rhat = numeric(),
                            show_density = FALSE,
                            bw = NULL,
                            adjust = NULL,
                            kernel = NULL) {

  rhat <- runif(3, 1.0, 1.7)

  x <- example_mcmc_draws(params = 6)
  pars <- c("beta[1]", "beta[2]", "beta[3]")
  regex_pars <- character(0)
  transformations <- list()
  prob = 0.5
  prob_outer = .95
  point_est = c("median", "mean", "none")


}


geom_area_ridges <- function(...) {
  ggridges::geom_density_ridges(..., stat = "identity", scale = 1)
}

geom_area_ridges2 <- function(...) {
  ggridges::geom_density_ridges2(..., stat = "identity", scale = 1)
}

#' Add new aesthetic mappings to a list of aesthetic mappings
#'
#' @param mapping a list of `uneval` aesthetic mappings (created by `aes_()`)
#' @param ... additional mappings to add, e.g., `color = ~ parameter`
#' @return the updated list
#' @noRd
modify_aes_ <- function(mapping, ...) {
  modifyList(mapping, aes_(...))
}


#' Compute density for a dataframe column.
#'
#' @param df a dataframe of posterior samples
#' @param group_vars columns to group by. e.g., `c(Parameter, Chain)`
#' @param value_var column containing posterior samples
#' @param ... arguments passed onto density calculation
#' @importFrom tidyr nest unnest
#' @noRd
compute_column_density <- function(df, group_vars, value_var, ...) {
  value_var <- enquo(value_var)
  group_vars <- enquo(group_vars)

  # Tuck away the subgroups to compute densities on into nested dataframes
  sub_df <- dplyr::select(df, !!! group_vars, !! value_var)
  nested <- dplyr::as_tibble(tidyr::nest(sub_df, !! value_var))

  # Only one column should be nested
  ncols <- unlist(lapply(nested$data, ncol))
  stopifnot(ncols == 1)

  # Comp
  nested$data <- lapply(nested$data, unlist)
  nested$density <- lapply(nested$data, compute_interval_density, ...)
  nested$data <- NULL

  tidyr::unnest(nested)
}


# Given a vector of values, compute a density dataframe.
compute_interval_density <- function(x, interval_width = 1, n = 1024,
                                     bw = NULL, adjust = NULL, kernel = NULL) {
  nx <- length(x)
  tail_width <- (1 - interval_width) / 2
  qs <- quantile(x, probs = c(tail_width, 1 - tail_width))

  args <- c(
    # can't be null
    list(x = x, from = min(qs), to = max(qs), n = n),
    # might be null
    bw = bw, adjust = adjust, kernel = kernel)

  dens <- do.call("density", args)

  data.frame(
    interval_width = interval_width,
    x = dens$x,
    density = dens$y,
    scaled_density =  dens$y / max(dens$y, na.rm = TRUE)
  )
}


# compute kernel density estimates
# all arguments are passed to stats::density
compute_dens_i <- function(x, bw, adjust, kernel, n, from, to) {
  args <- c(
    # can't be null
    list(
      x = x,
      from = from,
      to = to,
      n = n
    ),
    # might be null
    bw = bw,
    adjust = adjust,
    kernel = kernel
  )
  do.call("density", args)
}

# A geom that ignores any input arguments
geom_ignore <- function(...) geom_blank()
