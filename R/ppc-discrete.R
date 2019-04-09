#' PPCs for discrete outcomes
#'
#' Many of the \link{PPC} functions in \pkg{bayesplot} can
#' be used with discrete data. The small subset of these functions that can
#' \emph{only} be used if \code{y} and \code{yrep} are discrete are documented
#' on this page. Currently these include rootograms for count outcomes and bar
#' plots for ordinal, categorical, and multinomial outcomes. See the
#' \strong{Plot Descriptions} section below.
#'
#' @name PPC-discrete
#' @family PPCs
#'
#' @template args-y-yrep
#' @param ... Currently unused.
#' @param prob A value between 0 and 1 indicating the desired probability mass
#'   to include in the \code{yrep} intervals. Set \code{prob=0} to
#'   remove the intervals. For \code{ppc_rootogram} these are intervals
#'   of the \emph{square roots} of the expected counts.
#' @param width For \code{ppc_bars} and \code{ppc_bars_grouped},
#' passed to \code{\link[ggplot2]{geom_bar}} to control the bar width.
#' @param size,fatten For \code{ppc_bars} and \code{ppc_bars_grouped},
#'   \code{size} and \code{fatten} are passed to
#'   \code{\link[ggplot2]{geom_pointrange}} to control the appearance of the
#'   \code{yrep} points and intervals. For \code{ppc_rootogram} \code{size} is
#'   passed to \code{\link[ggplot2]{geom_line}}.
#' @param freq For \code{ppc_bars} and \code{ppc_bars_grouped}, if \code{TRUE}
#'   (the default) the y-axis will display counts. Setting \code{freq=FALSE}
#'   will put proportions on the y-axis.
#'
#'
#' @template return-ggplot
#'
#' @details For all of these plots \code{y} and \code{yrep} must be
#'   integers, although they need not be integers in the strict sense
#'   of \R's \code{\link{integer}} type. For rootogram plots \code{y}
#'   and \code{yrep} must also be non-negative.
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
#' \item{\code{ppc_rootogram}}{
#'   Rootograms allow for diagnosing problems in count data models such as
#'   overdispersion or excess zeros. They consist of a histogram of \code{y}
#'   with the expected counts based on \code{yrep} overlaid as a line along with
#'   uncertainty intervals. The y-axis represents the square roots of the counts
#'   to approximately adjust for scale differences and thus ease comparison
#'   between observed and expected counts. Using the \code{style} argument, the
#'   histogram style can be adjusted to focus on different aspects of the data:
#'   \itemize{
#'    \item \emph{Standing}: basic histogram of observed counts with curve
#'    showing expected counts.
#'    \item \emph{Hanging}: observed counts counts hanging from the curve
#'    representing expected counts.
#'   \item \emph{Suspended}: histogram of the differences between expected and
#'    observed counts.
#'   }
#'   \strong{All of these are plotted on the square root scale}. See Kleiber and
#'   Zeileis (2016) for advice on interpreting rootograms and selecting among
#'   the different styles.
#' }
#' }
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
           fatten = 3,
           freq = TRUE) {

  check_ignored_arguments(...)
  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)
  if (!all_whole_number(y)) {
    stop("ppc_bars expects 'y' to be discrete.")
  }
  if (!all_whole_number(yrep)) {
    stop("ppc_bars expects 'yrep' to be discrete.")
  }

  alpha <- (1 - prob) / 2
  probs <- sort(c(alpha, 0.5, 1 - alpha))
  yrep_data <- ppc_bars_yrep_data(
    y,
    yrep,
    probs = probs,
    freq = freq,
    group = NULL
  )

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


#' @rdname PPC-discrete
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
  if (!all_whole_number(y)) {
    stop("ppc_bars_grouped expects 'y' to be discrete.")
  }
  if (!all_whole_number(yrep)) {
    stop("ppc_bars_grouped expects 'yrep' to be discrete.")
  }

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


#' @rdname PPC-discrete
#' @export
#' @param style For \code{ppc_rootogram}, a string specifying the rootogram
#'   style. The options are \code{"standing"}, \code{"hanging"}, and
#'   \code{"suspended"}. See the \strong{Plot Descriptions} section, below, for
#'   details on the different styles.
#'
#' @references
#' Kleiber, C. and Zeileis, A. (2016). Visualizing count data regressions using
#' rootograms. \emph{The American Statistician}. 70(3): 296--303.
#' \url{https://arxiv.org/abs/1605.01311}.
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
#'
ppc_rootogram <- function(y,
                          yrep,
                          style = c("standing", "hanging", "suspended"),
                          ...,
                          prob = 0.9,
                          size = 1) {
  check_ignored_arguments(...)
  style <- match.arg(style)
  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)
  if (!all_counts(y))
    stop("ppc_rootogram expects counts as inputs to 'y'.")
  if (!all_counts(yrep))
    stop("ppc_rootogram expects counts as inputs to 'yrep'.")

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
  if (style == "hanging")
    ypos <- tyexp - ypos

  data <- data.frame(xpos, ypos, ty, tyexp, tyquantile)
  graph <- ggplot(data) +
    aes_(
      ymin = ~ tylower,
      ymax = ~ tyupper,
      height = ~ ty
    ) +
    geom_tile(
      aes_(
        x = ~ xpos,
        y = ~ ypos,
        fill = "Observed"
      ),
      color = get_color("lh"),
      size = 0.25,
      width = 1
    ) +
    bayesplot_theme_get()

  if (style != "standing")
    graph <- graph + hline_0(size = 0.4)

  graph <- graph +
    geom_smooth(
      aes_(
        x = ~ xpos,
        y = ~ tyexp,
        color = "Expected"
      ),
      fill = get_color("d"),
      size = size,
      stat = "identity"
    ) +
    scale_fill_manual("", values = get_color("l")) +
    scale_color_manual("", values = get_color("dh")) +
    labs(x = expression(italic(y)),
         y = expression(sqrt(Count)))

  if (style == "standing")
    graph <- graph + dont_expand_y_axis()

  graph + reduce_legend_spacing(0.25)
}





# internal ----------------------------------------------------------------

#' @importFrom dplyr "%>%" ungroup count arrange mutate
ppc_bars_yrep_data <- function(y, yrep, probs, freq = TRUE, group = NULL) {
  # Prepare for final summary
  sel <- ifelse(freq, "n", "proportion")
  lo  <- function(x) quantile(x, probs[1])
  mid <- function(x) quantile(x, probs[2])
  hi  <- function(x) quantile(x, probs[3])
  fs <- dplyr::funs(lo, mid, hi)

  # Set a dummy group for ungrouped data
  if (is.null(group)) {
    was_null_group <- TRUE
    group <- 1
  } else{
    was_null_group <- FALSE
  }

  # FIXME: double check and make sure that levels with zero counts are still plotted
  yrep_data <- ppc_group_data(y, yrep, group = group, stat = NULL) %>%
    dplyr::filter(.data$variable != "y") %>%
    ungroup() %>%
    count(.data$group, .data$value, .data$variable) %>%
    group_by(.data$variable, .data$group) %>%
    mutate(proportion = .data$n / sum(.data$n)) %>%
    ungroup() %>%
    group_by(.data$group, .data$value)

  summary_stats <- yrep_data %>%
    dplyr::summarise_at(sel, fs) %>%
    ungroup()

  # Drop dummy group
  if (was_null_group) {
    summary_stats$group <- NULL
  }

  summary_stats %>%
    rename(x = .data$value)
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
    labs(x = NULL, y = if (freq) "Count" else "Proportion") +
    bayesplot_theme_get()

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

  graph + reduce_legend_spacing(0.25)
}

