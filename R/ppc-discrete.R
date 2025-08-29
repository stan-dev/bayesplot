#' PPCs for discrete outcomes
#'
#' Many of the [PPC][PPC-overview] functions in **bayesplot** can
#' be used with discrete data. The small subset of these functions that can
#' *only* be used if `y` and `yrep` are discrete are documented
#' on this page. Currently these include rootograms for count outcomes and bar
#' plots for ordinal, categorical, and multinomial outcomes. See the
#' **Plot Descriptions** section below.
#'
#' @name PPC-discrete
#' @family PPCs
#'
#' @template args-y-yrep
#' @param ... Currently unused.
#' @param prob A value between `0` and `1` indicating the desired probability
#'   mass to include in the `yrep` intervals. Set `prob=0` to remove the
#'   intervals. (Note: for rootograms these are intervals of the *square roots*
#'   of the expected counts.)
#' @param width For bar plots only, passed to [ggplot2::geom_bar()] to control
#'   the bar width.
#' @param size,fatten,linewidth For bar plots, `size`, `fatten`, and `linewidth`
#'   are passed to [ggplot2::geom_pointrange()] to control the appearance of the
#'   `yrep` points and intervals. For rootograms `size` is passed to
#'   [ggplot2::geom_line()] and [ggplot2::geom_pointrange()].
#' @param freq For bar plots only, if `TRUE` (the default) the y-axis will
#'   display counts. Setting `freq=FALSE` will put proportions on the y-axis.
#' @param bound_distinct For `ppc_rootogram(style = "discrete)`,
#'  if `TRUE` then the observed counts will be plotted with different shapes
#'  depending on whether they are within the bounds of the `y` quantiles.
#'
#' @template return-ggplot-or-data
#'
#' @details For all of these plots `y` and `yrep` must be integers, although
#'   they need not be integers in the strict sense of \R's
#'   [integer][base::integer] type. For rootogram plots `y` and `yrep` must also
#'   be non-negative.
#'
#' @section Plot Descriptions:
#' \describe{
#' \item{`ppc_bars()`}{
#'   Bar plot of `y` with `yrep` medians and uncertainty intervals
#'   superimposed on the bars.
#' }
#' \item{`ppc_bars_grouped()`}{
#'   Same as `ppc_bars()` but a separate plot (facet) is generated for each
#'   level of a grouping variable.
#' }
#' \item{`ppc_rootogram()`}{
#'   Rootograms allow for diagnosing problems in count data models such as
#'   overdispersion or excess zeros. In `standing`, `hanging`, and `suspended`
#'   styles, they consist of a histogram of `y` with the expected counts based on
#'   `yrep` overlaid as a line along with uncertainty intervals.
#'
#'   Meanwhile, in `discrete` style, median counts based on `yrep` are laid
#'   as a point range with uncertainty intervals along with dots
#'   representing the `y`.
#'
#'   The y-axis represents the square roots of the counts to approximately
#'   adjust for scale differences and thus ease comparison between observed
#'   and expected counts. Using the `style` argument, the rootogram can be
#'   adjusted to focus on different aspects of the data:
#'   * _Standing_: basic histogram of observed counts with curve
#'    showing expected counts.
#'   * _Hanging_: observed counts hanging from the curve
#'    representing expected counts.
#'   * _Suspended_: histogram of the differences between expected and
#'    observed counts.
#'    * _Discrete_: a dot-and-whisker plot of the median counts and
#'    dots representing observed counts.
#'
#'    As it emphasizes the discrete nature of the count data,
#'    using `discrete` style is suggested.
#'
#'   **All of the rootograms are plotted on the square root scale**. See Kleiber
#'   and Zeileis (2016) for advice on interpreting rootograms and selecting
#'   among the different styles.
#' }
#' }
#'
#' @section Related functions:
#' In addition to the functions on this page that are restricted to discrete
#' outcomes, some general PPC/PPD functions also support discrete data
#' when requested:
#' - [ppc_stat()] and [ppc_stat_grouped()] can visualize discrete test statistics
#'    with predictive checks when `discrete = TRUE`.
#' - [ppd_stat()] and [ppd_stat_grouped()] can visualize discrete test statistics
#'    from predictive draws when `discrete = TRUE`.
#' - [ppc_ecdf_overlay] can visualize empirical CDFs for discrete statistics
#'    with `discrete = TRUE`.
#' - [ppc_pit_ecdf()] and [ppc_pit_ecdf_grouped()] can also handle discrete
#'   variables to plot PIT-ECDF of the empirical PIT values.
#'
#' These functions are not limited to discrete outcomes, but offer discrete-friendly
#' displays for integer-valued statistics.
#'
#' @examples
#' set.seed(9222017)
#'
#' # bar plots
#' f <- function(N) {
#'   sample(1:4, size = N, replace = TRUE, prob = c(0.25, 0.4, 0.1, 0.25))
#' }
#' y <- f(100)
#' yrep <- t(replicate(500, f(100)))
#' dim(yrep)
#' group <- gl(2, 50, length = 100, labels = c("GroupA", "GroupB"))
#'
#' color_scheme_set("mix-pink-blue")
#' ppc_bars(y, yrep)
#'
#' # split by group, change interval width, and display proportion
#' # instead of count on y-axis
#' color_scheme_set("mix-blue-pink")
#' ppc_bars_grouped(y, yrep, group, prob = 0.5, freq = FALSE)
#'
#' \dontrun{
#' # example for ordinal regression using rstanarm
#' library(rstanarm)
#' fit <- stan_polr(
#'   tobgp ~ agegp,
#'   data = esoph,
#'   method = "probit",
#'   prior = R2(0.2, "mean"),
#'   init_r = 0.1,
#'   seed = 12345,
#'   # cores = 4,
#'   refresh = 0
#'  )
#'
#' # coded as character, so convert to integer
#' yrep_char <- posterior_predict(fit)
#' print(yrep_char[1, 1:4])
#'
#' yrep_int <- sapply(data.frame(yrep_char, stringsAsFactors = TRUE), as.integer)
#' y_int <- as.integer(esoph$tobgp)
#'
#' ppc_bars(y_int, yrep_int)
#'
#' ppc_bars_grouped(
#'   y = y_int,
#'   yrep = yrep_int,
#'   group = esoph$agegp,
#'   freq=FALSE,
#'   prob = 0.5,
#'   fatten = 1,
#'   size = 1.5
#' )
#' }
#'
NULL

#' @rdname PPC-discrete
#' @export
ppc_bars <-
  function(y,
           yrep,
           ...,
           prob = 0.9,
           width = 0.9,
           size = 1,
           fatten = 2.5,
           linewidth = 1,
           freq = TRUE) {

    dots <- list(...)
    if (!from_grouped(dots)) {
      check_ignored_arguments(...)
      dots$group <- NULL
    }

    data <- ppc_bars_data(
      y = y,
      yrep = yrep,
      group = dots$group,
      prob = prob,
      freq = freq
    )

    if (!is.null(dots$group)) {
      limits <- geom_ignore()
    } else {
      limits <- expand_limits(y = 1.05 * max(data[["h"]], na.rm = TRUE))
    }

    ggplot(data) +
      geom_col(
        data = dplyr::filter(data, !is.na(.data$y_obs)),
        mapping = aes(x = .data$x, y = .data$y_obs, fill = "y"),
        color = get_color("lh"),
        width = width
      ) +
      geom_pointrange(
        mapping = intervals_inner_aes(needs_y = TRUE, color = "yrep"),
        size = size,
        fatten = fatten,
        linewidth = linewidth,
        na.rm = TRUE
      ) +
      scale_color_ppc(
        values = get_color("d"),
        labels = yrep_label(),
        guide = guide_legend(order = 1, override.aes = list(size = .75 * size))
      ) +
      scale_fill_ppc(values = get_color("l"), labels = y_label()) +
      scale_x_continuous(breaks = pretty) +
      labs(x = NULL, y = if (freq) "Count" else "Proportion") +
      dont_expand_y_axis() +
      bayesplot_theme_get() +
      limits +
      reduce_legend_spacing(0.25)
  }


#' @rdname PPC-discrete
#' @export
#' @template args-group
#' @param facet_args An optional list of  arguments (other than `facets`)
#'   passed to [ggplot2::facet_wrap()] to control faceting.
ppc_bars_grouped <-
  function(y,
           yrep,
           group,
           ...,
           facet_args = list(),
           prob = 0.9,
           width = 0.9,
           size = 1,
           fatten = 2.5,
           linewidth = 1,
           freq = TRUE) {
    check_ignored_arguments(...)
    call <- match.call(expand.dots = FALSE)
    g <- eval(ungroup_call("ppc_bars", call), parent.frame())
    if (fixed_y(facet_args)) {
      g <- g + expand_limits(y = 1.05 * max(g$data[["h"]], na.rm = TRUE))
    }
    g +
      bars_group_facets(facet_args) +
      force_axes_in_facets()
  }


#' @rdname PPC-discrete
#' @export
#' @param style For `ppc_rootogram`, a string specifying the rootogram
#'   style. The options are `"discrete"`, `"standing"`, `"hanging"`, and
#'   `"suspended"`. See the **Plot Descriptions** section, below, for
#'   details on the different styles.
#'
#' @references
#' Kleiber, C. and Zeileis, A. (2016).
#' Visualizing count data regressions using rootograms.
#' *The American Statistician*. 70(3): 296--303.
#' <https://arxiv.org/abs/1605.01311>.
#'
#' @examples
#' # rootograms for counts
#' y <- rpois(100, 20)
#' yrep <- matrix(rpois(10000, 20), ncol = 100)
#'
#' color_scheme_set("brightblue")
#' ppc_rootogram(y, yrep)
#' ppc_rootogram(y, yrep, prob = 0)
#'
#' ppc_rootogram(y, yrep, style = "hanging", prob = 0.8)
#' ppc_rootogram(y, yrep, style = "suspended")
#' ppc_rootogram(y, yrep, style = "discrete")
#'
ppc_rootogram <- function(y,
                          yrep,
                          style = c("standing", "hanging", "suspended", "discrete"),
                          ...,
                          prob = 0.9,
                          size = 1,
                          bound_distinct = TRUE) {
  check_ignored_arguments(...)
  style <- match.arg(style)

  data <- .ppc_rootogram_data(
    y = y,
    yrep = yrep,
    style = style,
    prob = prob,
    bound_distinct = bound_distinct
  )

  # Building geoms for y and y_rep
  geom_y <- if (style == "discrete") {
    geom_point(
      aes(y = .data$obs, shape = .data$obs_shape),
      size = size * 1.5,
      color = get_color("d"),
      fill = get_color("d"))
  } else {
    geom_tile(
      aes(
        x = .data$xpos,
        y = .data$ypos,
        fill = "Observed"
      ),
      color = get_color("lh"),
      linewidth = 0.25,
      width = 1
    )
  }

  geom_yrep <- if (style == "discrete") {
    geom_pointrange(
      aes(y = .data$pred_median, ymin = .data$lower, ymax = .data$upper, color = "y_rep"),
      fill = get_color("lh"),
      linewidth = size,
      size = size,
      fatten = 2,
      alpha = 1
    )
  } else {
    geom_smooth(
      aes(x = .data$xpos, y = .data$tyexp, color = "Expected"),
      fill = get_color("d"),
      linewidth = size,
      stat = "identity"
    )
  }

  # Creating the graph
  graph <- ggplot(data)

  if (style == "discrete") {
    graph <- graph +
      geom_yrep +
      geom_y +
      aes(x = .data$xpos) +
      scale_y_sqrt() +
      scale_fill_manual("", values = get_color("d"), guide = "none") +
      scale_color_manual("", values = get_color("lh"), labels = yrep_label()) +
      labs(x = expression(italic(y)), y = "Count") +
      bayesplot_theme_get() +
      reduce_legend_spacing(0.25) +
      scale_shape_manual(values = c("In" = 22, "Out" = 23, "y" = 22), guide = "legend", labels = c("y" = expression(italic(y))))
    if (bound_distinct) {
      graph <- graph + guides(shape = guide_legend(expression(italic(y)~within~bounds)))
    } else {
      graph <- graph + guides(shape = guide_legend(" "))
    }
  } else {
    graph <- graph +
      geom_y +
      geom_yrep +
      aes(
        ymin = .data$tylower,
        ymax = .data$tyupper,
        height = .data$ty
      ) +
      scale_fill_manual("", values = get_color("l")) +
      scale_color_manual("", values = get_color("dh")) +
      labs(x = expression(italic(y)), y = expression(sqrt(Count))) +
      bayesplot_theme_get() +
      reduce_legend_spacing(0.25)
    if (style == "standing") {
      graph <- graph + dont_expand_y_axis()
    } else {
      graph <- graph + hline_0(size = 0.4)
    }
  }

  return(graph)
}


#' @rdname PPC-discrete
#' @export
ppc_bars_data <-
  function(y,
           yrep,
           group = NULL,
           prob = 0.9,
           freq = TRUE) {
    stopifnot(0 <= prob && prob <= 1, is.logical(freq))
    y <- validate_y(y)
    yrep <- validate_predictions(yrep, length(y))
    if (!all_whole_number(y)) {
      abort("ppc_bars expects 'y' to be discrete.")
    }
    if (!all_whole_number(yrep)) {
      abort("ppc_bars expects 'yrep' to be discrete.")
    }
    if (!is.null(group)) {
      group <- validate_group(group, length(y))
    }
    .ppc_bars_data(
      y = y,
      yrep = yrep,
      group = group,
      prob = prob,
      freq = freq
    )
  }


# internal ----------------------------------------------------------------

#' Internal function for `ppc_bars_data()`
#'
#' @noRd
#' @param y,yrep,group User's already validated `y`, `yrep`, and (if applicable)
#'   `group` arguments.
#' @param prob,freq User's `prob` and `freq` arguments.
#' @importFrom dplyr %>% ungroup count arrange mutate summarise across full_join rename all_of
.ppc_bars_data <- function(y, yrep, group = NULL, prob = 0.9, freq = TRUE) {
  alpha <- (1 - prob) / 2
  probs <- sort(c(alpha, 0.5, 1 - alpha))

  # Prepare for final summary
  lo  <- function(x) quantile(x, probs[1])
  mid <- function(x) quantile(x, probs[2])
  hi  <- function(x) quantile(x, probs[3])
  summary_var <- ifelse(freq, "n", "proportion")
  summary_funs <- list(l = lo, m = mid, h = hi) # use l,m,h like in our intervals data

  # Set a dummy group for ungrouped data
  if (is.null(group)) {
    was_null_group <- TRUE
    group <- 1
  } else{
    was_null_group <- FALSE
  }

  tmp_data <- data.frame(
    group = factor(group),
    y = y,
    yrep = t(yrep)
  )
  data <-
    reshape2::melt(tmp_data, id.vars = "group") %>%
    count(.data$group, .data$value, .data$variable) %>%
    tidyr::complete(.data$group, .data$value, .data$variable, fill = list(n = 0)) %>%
    group_by(.data$variable, .data$group) %>%
    mutate(proportion = .data$n / sum(.data$n)) %>%
    ungroup() %>%
    group_by(.data$group, .data$value)

  yrep_summary <- data %>%
    dplyr::filter(!.data$variable == "y") %>%
    summarise(across(all_of(summary_var), summary_funs, .names = "{.fn}")) %>%
    ungroup() %>%
    arrange(.data$group, .data$value)

  y_summary <- data %>%
    dplyr::filter(.data$variable == "y") %>%
    ungroup() %>%
    rename(y_obs = all_of(summary_var)) %>%
    arrange(.data$group, .data$value)

  cols <- syms(c(if (!was_null_group) "group", "x", "y_obs", "l", "m", "h"))

  # full join to keep empty cells
  full_join(yrep_summary, y_summary, by = c("group", "value")) %>%
    rename(x = "value") %>%
    arrange(.data$x) %>%
    select(!!!cols)
}


#' Create the facet layer for grouped bar plots
#' @param facet_args User's `facet_args` argument.
#' @param scales_default String to use for `scales` argument to `facet_wrap()`
#'   if not specified by user. The default is `"fixed"` for bar plots. This is
#'   the same as `ggplot2::facet_wrap()` but different than
#'   `bayesplot::intervals_group_facets()`, which has a default of `"free"`.
#' @return Object returned by `facet_wrap()`.
#' @noRd
bars_group_facets <- function(facet_args, scales_default = "fixed") {
  facet_args[["facets"]] <- "group"
  facet_args[["scales"]] <- facet_args[["scales"]] %||% scales_default
  do.call("facet_wrap", facet_args)
}

fixed_y <- function(facet_args) {
  !isTRUE(facet_args[["scales"]] %in% c("free", "free_y"))
}

#' Internal function for `ppc_rootogram()`
#' @param y,yrep User's `y` and `yrep` arguments.
#' @param style,prob,bound_distinct User's `style`, `prob`, and
#' (if applicable) `bound_distinct` arguments.
#' @noRd
.ppc_rootogram_data <- function(y,
                              yrep,
                              style = c("standing", "hanging", "suspended", "discrete"),
                              prob = 0.9,
                              bound_distinct) {

  y <- validate_y(y)
  yrep <- validate_predictions(yrep, length(y))
  if (!all_counts(y)) {
    abort("ppc_rootogram expects counts as inputs to 'y'.")
  }
  if (!all_counts(yrep)) {
    abort("ppc_rootogram expects counts as inputs to 'yrep'.")
  }

  alpha <- (1 - prob) / 2
  probs <- c(alpha, 1 - alpha)
  ymax <- max(y, yrep)
  xpos <- 0L:ymax

  # prepare a table for yrep
  tyrep <- as.list(rep(NA, nrow(yrep)))
  for (i in seq_along(tyrep)) {
    tyrep[[i]] <- table(yrep[i,])
    matches <- match(xpos, rownames(tyrep[[i]]))
    tyrep[[i]] <- as.numeric(tyrep[[i]][matches])
  }
  tyrep <- do.call(rbind, tyrep)
  tyrep[is.na(tyrep)] <- 0

  # discrete style
  if (style == "discrete"){
    pred_median <- apply(tyrep, 2, median)
    pred_quantile <- t(apply(tyrep, 2, quantile, probs = probs))
    colnames(pred_quantile) <- c("lower", "upper")

    # prepare a table for y
    ty <- table(y)
    y_count <- as.numeric(ty[match(xpos, rownames(ty))])
    y_count[is.na(y_count)] <- 0

    if (bound_distinct) {
      # If the observed count is within the bounds of the predicted quantiles,
      # use a different shape for the point
      obs_shape <- obs_shape <- ifelse(y_count >= pred_quantile[, "lower"] & y_count <= pred_quantile[, "upper"], "In", "Out")
    } else {
      obs_shape <- rep("y", length(y_count)) # all points are the same shape for observed
    }

    data <- data.frame(
      xpos = xpos,
      obs = y_count,
      pred_median = pred_median,
      lower = pred_quantile[, "lower"],
      upper = pred_quantile[, "upper"],
      obs_shape = obs_shape
    )
  }
  # standing, hanging, suspended styles
  else {
    tyexp <- sqrt(colMeans(tyrep))
    tyquantile <- sqrt(t(apply(tyrep, 2, quantile, probs = probs)))
    colnames(tyquantile) <- c("tylower", "tyupper")

    # prepare a table for y
    ty <- table(y)
    ty <- sqrt(as.numeric(ty[match(xpos, rownames(ty))]))
    if (style == "suspended") {
      ty <- tyexp - ty
    }
    ty[is.na(ty)] <- 0
    ypos <- ty / 2
    if (style == "hanging") {
      ypos <- tyexp - ypos
    }

    data <- data.frame(xpos, ypos, ty, tyexp, tyquantile)
  }

  return(data)
}
