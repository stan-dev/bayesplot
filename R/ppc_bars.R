#' Bar plots for discrete outcomes (ordinal, multinomial, count)
#'
#' Bar plots for discrete outcomes (ordinal, multinomial, count). See the
#' \strong{Plot Descriptions} section below.
#'
#' @export
#' @template args-y-yrep
#' @param prob A value between 0 and 1 indicating the desired probability mass
#'   to include in the \code{yrep} intervals. The default is 0.9.
#' @param width Passed to \code{\link[ggplot2]{geom_bar}} to control the bar
#'   width.
#' @param size,fatten Passed to \code{\link[ggplot2]{geom_pointrange}} to
#' control the appearance of the \code{yrep} points and intervals.
#'
#' @details For \code{ppc_bars}, the observations \code{y} and predictons
#'   \code{yrep} must be non-negative integers. \pkg{bayesplot} will validate
#'   that both \code{y} and \code{yrep} contain only non-negative integer
#'   values, although they need not be integers in the strict sense of \R's
#'   \code{\link{integer}} type.
#'
#' @section Plot Descriptions:
#' \describe{
#' \item{\code{ppc_bars}}{
#'   Bar plot of a discrete variable \code{y} with \code{yrep} medians and
#'   uncertainty intervals superimposed on the bars.
#' }
#' }
#'
ppc_bars <-
  function(y,
           yrep,
           ...,
           prob = 0.9,
           width = 0.9,
           size = 1,
           fatten = 3) {

    check_ignored_arguments(...)
    y <- validate_y(y)
    yrep <- validate_yrep(yrep, y)
    if (!all_counts(y))
      stop("ppc_bars expects only non-negative integers in 'y'.")
    if (!all_counts(yrep))
      stop("ppc_bars expects only non-negative integers in 'yrep'.")

    alpha <- (1 - prob) / 2
    probs <- sort(c(alpha, 0.5, 1 - alpha))

    y_data <- data.frame(y = y)

    yrep_tab <- t(apply(yrep, 1, table)) # draws x number of categories
    yrep_intervals <- apply(yrep_tab, 2, quantile, probs = probs)
    yrep_data <- data.frame(
      x = 1:length(unique(y_data$y)),
      lo = yrep_intervals[1, ],
      mid = yrep_intervals[2, ],
      hi = yrep_intervals[3, ]
    )

    ggplot() +
      geom_bar(
        data = y_data,
        mapping = aes_(x = ~ y, fill = "y"),
        color = get_color("lh"),
        width = width
      ) +
      geom_pointrange(
        data = yrep_data,
        mapping = aes_(
          x = ~ x,
          y = ~ mid,
          ymin = ~ lo,
          ymax = ~ hi,
          color = "yrep"
        ),
        size = size,
        fatten = fatten
      ) +
      scale_fill_manual("", values = get_color("l"),
                        labels = y_label()) +
      scale_color_manual("", values = get_color("dh"),
                         labels = yrep_label()) +
      guides(color = guide_legend(order = 1),
             fill = guide_legend(order = 2)) +
      labs(x = y_label(), y = "Count") +
      dont_expand_y_axis() +
      theme_default() +
      theme(legend.spacing.y = unit(-0.25, "cm"))
  }
