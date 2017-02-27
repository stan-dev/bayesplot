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
    yrep_data <- ppc_bars_yrep_data(y, yrep, group = NULL, probs = probs)
    .ppc_bars(
      y_data = data.frame(y = y),
      yrep_data,
      grouped = FALSE,
      facet_args = list(),
      width = width,
      size = size,
      fatten = fatten
    )
  }


#' @rdname ppc_bars
#' @export
#' @template args-group
#' @param facet_args An optional list of  arguments (other than \code{facets})
#'   passed to \code{\link[ggplot2]{facet_wrap}} to control faceting.
ppc_bars_grouped <-
  function(y,
           yrep,
           group,
           facet_args = list(),
           ...,
           prob = 0.9,
           width = 0.9,
           size = 1,
           fatten = 3) {

    check_ignored_arguments(...)
    y <- validate_y(y)
    yrep <- validate_yrep(yrep, y)
    group <- validate_group(group, y)
    if (!all_counts(y))
      stop("ppc_bars expects only non-negative integers in 'y'.")
    if (!all_counts(yrep))
      stop("ppc_bars expects only non-negative integers in 'yrep'.")

    alpha <- (1 - prob) / 2
    probs <- sort(c(alpha, 0.5, 1 - alpha))
    yrep_data <- ppc_bars_yrep_data(y, yrep, group, probs)
    .ppc_bars(
      y_data = data.frame(y, group),
      yrep_data,
      grouped = TRUE,
      facet_args = facet_args,
      width = width,
      size = size,
      fatten = fatten
    )
  }



# internal ----------------------------------------------------------------

#' @importFrom dplyr "%>%" ungroup count_ arrange_
ppc_bars_yrep_data <- function(y, yrep, group = NULL, probs) {
  if (is.null(group)) {
    tab <-
      melt_yrep(yrep, label = FALSE) %>%
      count_(vars = c("rep_id", "value")) %>%
      ungroup() %>%
      select_(.dots = list(~ value, ~ n)) %>%
      arrange_(~ value) %>%
      group_by_(~value)

    return(
      data.frame(
        x = unique(tab$value),
        lo = summarise_(tab, lo = ~ quantile(n, probs = probs[1]))$lo,
        mid = summarise_(tab, mid = ~ quantile(n, probs = 0.5))$mid,
        hi = summarise_(tab, hi = ~ quantile(n, probs = probs[3]))$hi
      )
    )
  }

  # FIXME: double check and make sure that levels with zero counts are still plotted
  yrep_data <-
    ppc_group_data(y, yrep, group = group, stat = NULL) %>%
    filter_(~ variable != "y") %>%
    ungroup() %>%
    count_(vars = c("group", "value", "variable")) %>%
    group_by_(.dots = list(~ group, ~ value)) %>%
    summarise_(
      lo = ~ quantile(n, probs = probs[1]),
      mid = ~ median(n),
      hi = ~ quantile(n, probs = probs[3])
    )

  colnames(yrep_data)[colnames(yrep_data) == "value"] <- "x"
  return(yrep_data)
}

.ppc_bars <- function(y_data,
                      yrep_data,
                      facet_args = list(),
                      grouped = FALSE,
                      width = 0.9,
                      size = 1,
                      fatten = 3) {
  graph <- ggplot() +
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
    labs(x = y_label(), y = "Count")

  if (grouped) {
    facet_args[["facets"]] <- "group"
    if (is.null(facet_args[["scales"]]))
      facet_args[["scales"]] <- "free"
    graph <- graph + do.call("facet_wrap", facet_args)
  }

  graph +
    scale_x_continuous(breaks = pretty) +
    dont_expand_y_axis() +
    theme_default() +
    theme(legend.spacing.y = unit(-0.25, "cm"))
}

