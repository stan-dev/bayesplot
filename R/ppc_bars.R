#' Bar plots for ordinal, categorical and multinomial outcomes
#'
#' Bar plots for ordinal, categorical, and multinomial outcomes. See the
#' \strong{Plot Descriptions} section below.
#'
#' @export
#' @template args-y-yrep
#' @param ... Currently unused.
#' @param prob A value between 0 and 1 indicating the desired probability mass
#'   to include in the \code{yrep} intervals. The default is 0.9.
#' @param width Passed to \code{\link[ggplot2]{geom_bar}} to control the bar
#'   width.
#' @param size,fatten Passed to \code{\link[ggplot2]{geom_pointrange}} to
#' control the appearance of the \code{yrep} points and intervals.
#' @param freq If \code{TRUE} (the default) the y-axis will display counts.
#'   Setting \code{freq=FALSE} will put proportions on the y-axis.
#'
#' @details For \code{ppc_bars}, the observations \code{y} and predictions
#'   \code{yrep} must be non-negative integers. \pkg{bayesplot} will validate
#'   that both \code{y} and \code{yrep} contain only non-negative integer
#'   values, although they need not be integers in the strict sense of \R's
#'   \code{\link{integer}} type.
#'
#' @section Plot Descriptions:
#' \describe{
#' \item{\code{ppc_bars}}{
#'   Bar plot of \code{y} with \code{yrep} medians and uncertainty intervals
#'   superimposed on the bars.
#' }
#' \item{\code{ppc_bars_grouped}}{
#'   Same as \code{ppc_bars} but a separate plot (facet) is generated for each
#'   level of a grouping variable.
#' }
#' }
#'
#' @examples
#' f <- function(N) {
#'   sample(1:4, size = N, replace = T, prob = c(0.25, 0.4, 0.1, 0.25))
#' }
#' y <- f(100)
#' yrep <- t(replicate(500, f(100)))
#' dim(yrep)
#'
#' ppc_bars(y, yrep)
#'
#' group <- gl(2, 50, length = 100, labels = c("GroupA", "GroupB"))
#' ppc_bars_grouped(y, yrep, group, prob = 0.5, freq = FALSE)
#'
ppc_bars <-
  function(y,
           yrep,
           ...,
           prob = 0.9,
           width = 0.9,
           size = 1,
           fatten = 3,
           freq = TRUE) {

    check_ignored_arguments(...)
    y <- validate_y(y)
    yrep <- validate_yrep(yrep, y)
    if (!all_counts(y))
      stop("ppc_bars expects only non-negative integers in 'y'.")
    if (!all_counts(yrep))
      stop("ppc_bars expects only non-negative integers in 'yrep'.")

    alpha <- (1 - prob) / 2
    probs <- sort(c(alpha, 0.5, 1 - alpha))
    yrep_data <- ppc_bars_yrep_data(y, yrep, probs = probs, freq = freq, group = NULL)
    .ppc_bars(
      y_data = data.frame(y = y),
      yrep_data,
      grouped = FALSE,
      facet_args = list(),
      width = width,
      size = size,
      fatten = fatten,
      freq = freq
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
           fatten = 3,
           freq = TRUE) {

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
    yrep_data <- ppc_bars_yrep_data(y, yrep, probs, freq = freq, group = group)
    .ppc_bars(
      y_data = data.frame(y, group),
      yrep_data,
      grouped = TRUE,
      facet_args = facet_args,
      width = width,
      size = size,
      fatten = fatten,
      freq = freq
    )
  }



# internal ----------------------------------------------------------------

#' @importFrom dplyr "%>%" ungroup count_ arrange_ mutate_
ppc_bars_yrep_data <- function(y, yrep, probs, freq = TRUE, group = NULL) {
  if (is.null(group)) {
    tab <-
      melt_yrep(yrep, label = FALSE) %>%
      count_(vars = c("rep_id", "value")) %>%
      group_by_(~ rep_id) %>%
      mutate_(proportion = ~ n / sum(n)) %>%
      ungroup() %>%
      select_(.dots = list(~ value, ~ n, ~ proportion)) %>%
      arrange_(~ value) %>%
      group_by_(~ value)

    sel <- ifelse(freq, "n", "proportion")
    colnames(tab)[colnames(tab) == sel] <- "xxx"
    return(
      data.frame(
        x = unique(tab$value),
        lo = summarise_(tab, lo = ~ quantile(xxx, probs = probs[1]))$lo,
        mid = summarise_(tab, mid = ~ quantile(xxx, probs = 0.5))$mid,
        hi = summarise_(tab, hi = ~ quantile(xxx, probs = probs[3]))$hi
      )
    )
  }

  # FIXME: double check and make sure that levels with zero counts are still plotted
  yrep_data <-
    ppc_group_data(y, yrep, group = group, stat = NULL) %>%
    filter_(~ variable != "y") %>%
    ungroup() %>%
    count_(vars = c("group", "value", "variable")) %>%
    group_by_(.dots = list(~ variable, ~ group)) %>%
    mutate_(proportion = ~ n / sum(n)) %>%
    group_by_(.dots = list(~ group, ~ value))

  sel <- ifelse(freq, "n", "proportion")
  colnames(yrep_data)[colnames(yrep_data) == sel] <- "xxx"

  out <- summarise_(
    yrep_data,
    lo = ~ quantile(xxx, probs = probs[1]),
    mid = ~ median(xxx),
    hi = ~ quantile(xxx, probs = probs[3])
  )
  colnames(out)[colnames(out) == "value"] <- "x"
  return(out)
}

.ppc_bars <- function(y_data,
                      yrep_data,
                      facet_args = list(),
                      grouped = FALSE,
                      width = 0.9,
                      size = 1,
                      fatten = 3,
                      freq = TRUE) {

  graph <- ggplot() +
    geom_bar(
      data = y_data,
      mapping =
        if (freq)
          aes_(x = ~ y, fill = "y")
        else
          aes_(x = ~ y, y = ~ ..prop.., fill = "y"),
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
    labs(x = NULL, y = if (freq) "Count" else "Proportion")

  if (grouped) {
    facet_args[["facets"]] <- "group"
    graph <- graph + do.call("facet_wrap", facet_args)
  }

  graph <- graph +
    scale_x_continuous(breaks = pretty) +
    dont_expand_y_axis()


  # add a little space between the max value plotted and the top of the plot
  if (!grouped || !(isTRUE(facet_args[["scales"]] %in% c("free", "free_y")))) {
    g <- ggplot_build(graph)
    y_axis_max <- max(g$data[[1]][["ymax"]], g$data[[2]][["ymax"]])
    graph <- graph + expand_limits(y = 1.05 * y_axis_max)
  }

  graph +
    theme_default() +
    theme(legend.spacing.y = unit(-0.25, "cm"))
}

