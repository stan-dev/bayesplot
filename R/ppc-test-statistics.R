#' PPC test statistics
#'
#' The distribution of a test statistic \code{T(yrep)}, or a
#' pair of test statistics, over the simulated datasets in \code{yrep},
#' compared to the observed value \code{T(y)}. See the \strong{Plot
#' Descriptions} and \strong{Details} sections, below.
#'
#' @name PPC-test-statistics
#' @family PPCs
#'
#' @template args-y-yrep
#' @template args-hist
#' @param stat A character vector of function names of length 1 (for
#'   \code{ppc_stat} and \code{ppc_stat_grouped}) or length 2 (for
#'   \code{ppc_stat_2d}). The function(s) should take a vector input and return
#'   a scalar test statistic.
#' @param ... Currently unused.
#'
#' @template details-binomial
#' @template return-ggplot
#'
#' @templateVar bdaRef (Ch. 6)
#' @template reference-bda
#'
#' @section Plot Descriptions:
#' \describe{
#'   \item{\code{ppc_stat}}{
#'    A histogram of the distribution of a test statistic computed by applying
#'    \code{stat} to each dataset (row) in \code{yrep}. The value of the
#'    statistic in the observed data, \code{stat(y)}, is overlaid as a vertical
#'    line.
#'   }
#'   \item{\code{ppc_stat_grouped}}{
#'    The same as \code{ppc_stat}, but a separate plot is generated for
#'    each level of a grouping variable.
#'   }
#'   \item{\code{ppc_stat_2d}}{
#'    A scatterplot showing the joint distribution of two test statistics
#'    computed over the datasets (rows) in \code{yrep}. The value of the
#'    statistics in the observed data is overlaid as large point.
#'   }
#' }
#'
#' @examples
#' y <- example_y_data()
#' yrep <- example_yrep_draws()
#' ppc_stat(y, yrep)
#' ppc_stat(y, yrep, stat = "sd") + no_legend()
#' ppc_stat_2d(y, yrep)
#' ppc_stat_2d(y, yrep, stat = c("median", "mean")) + no_legend()
#'
#' group <- example_group_data()
#' ppc_stat_grouped(y, yrep, group)
#'
#' # use your own function to compute test statistics
#' q25 <- function(y) quantile(y, 0.25)
#' ppc_stat(y, yrep, stat = "q25")
#'
NULL

#' @export
#' @rdname PPC-test-statistics
#'
ppc_stat <- function(y, yrep, stat = "mean", ..., binwidth = NULL) {
  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)
  stat <- validate_stat(stat, 1)

  stat1 <- match.fun(stat)
  T_y <- stat1(y)
  T_yrep <- apply(yrep, 1, stat1)

  ggplot(data.frame(x = T_yrep),
         aes_(x = ~ x, y = ~ ..density..)) +
    geom_histogram(
      aes_(fill = "yrep"),
      color = get_color("lh"),
      size = .25,
      na.rm = TRUE,
      binwidth = binwidth
    ) +
    geom_vline(
      data = data.frame(Ty = T_y),
      mapping = aes_(xintercept = ~ Ty, color = "y"),
      size = 1.5
    ) +
    scale_fill_manual(values = get_color("l"), labels = Tyrep_label()) +
    scale_color_manual(values = get_color("dh"), labels = Ty_label()) +
    guides(
      fill = guide_legend(title = bquote(italic(T) %==% .(stat)), order = 1),
      color = guide_legend(title = NULL)
    ) +
    dont_expand_y_axis() +
    theme_default() +
    theme(legend.spacing.y = unit(0, "cm")) +
    xaxis_title(FALSE) +
    yaxis_text(FALSE) +
    yaxis_ticks(FALSE) +
    yaxis_title(FALSE)
}

#' @export
#' @rdname PPC-test-statistics
#' @template args-group
#'
ppc_stat_grouped <-
  function(y,
           yrep,
           group,
           stat = "mean",
           ...,
           binwidth = NULL) {
    y <- validate_y(y)
    yrep <- validate_yrep(yrep, y)
    group <- validate_group(group, y)
    stat <- validate_stat(stat, 1)
    plot_data <- ppc_group_data(y, yrep, group, stat = stat)
    is_y <- plot_data$variable == "y"

    ggplot(
      data = plot_data[!is_y, , drop = FALSE],
      mapping = aes_(x = ~ value, y = ~ ..density..)
    ) +
      geom_histogram(
        aes_(fill = "yrep"),
        color = get_color("lh"),
        size = .25,
        na.rm = TRUE,
        binwidth = binwidth
      ) +
      geom_vline(
        data = plot_data[is_y, , drop = FALSE],
        mapping = aes_(xintercept = ~ value, color = "y"),
        size = 1.5
      ) +
      facet_wrap("group", scales = "free") +
      scale_fill_manual(values = get_color("l"), labels = Tyrep_label()) +
      scale_color_manual(values = get_color("dh"), labels = Ty_label()) +
      guides(
        fill = guide_legend(title = bquote(italic(T) %==% .(stat)), order = 1),
        color = guide_legend(title = NULL)
      ) +
      dont_expand_y_axis() +
      theme_default() +
      theme(legend.spacing.y = unit(0, "cm")) +
      facet_bg(FALSE) +
      facet_text(face = "bold") +
      xaxis_title(FALSE) +
      yaxis_text(FALSE) +
      yaxis_ticks(FALSE) +
      yaxis_title(FALSE)
  }


#' @rdname PPC-test-statistics
#' @export
#' @param size,alpha Arguments passed to \code{\link[ggplot2]{geom_point}} to
#'   control the appearance of scatterplot points.
ppc_stat_2d <- function(y, yrep, stat = c("mean", "sd"), ...,
                        size = 2.5, alpha = 0.7) {
  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)
  stat <- validate_stat(stat, 2)

  stat1 <- match.fun(stat[1])
  stat2 <- match.fun(stat[2])
  T_y1 <- stat1(y)
  T_y2 <- stat2(y)
  T_yrep1 <- apply(yrep, 1, stat1)
  T_yrep2 <- apply(yrep, 1, stat2)
  lgnd_title <- bquote(italic(T) %==% (list(.(stat[1]), .(stat[2]))))
  ggplot(
    data = data.frame(x = T_yrep1, y = T_yrep2),
    mapping = aes_(x = ~ x, y = ~ y)
  ) +
    geom_point(
      aes_(fill = "yrep", color = "yrep"),
      shape = 21,
      # fill = get_color("l"),
      # color = get_color("lh"),
      size = size,
      alpha = alpha
    ) +
    annotate(
      geom = "segment",
      x = c(T_y1, -Inf), xend = c(T_y1, T_y1),
      y = c(-Inf, T_y2), yend = c(T_y2, T_y2),
      linetype = 2,
      size = 0.4,
      color = get_color("dh")
    ) +
    geom_point(
      data = data.frame(x = T_y1, y = T_y2),
      mapping = aes_(x = ~ x, y = ~ y, fill = "y", color = "y"),
      size = size * 1.5,
      shape = 21,
      stroke = 0.75
    ) +
    scale_fill_manual(
      name = lgnd_title,
      values = setNames(get_color(c("d", "l")), c("y", "yrep")),
      labels = c(y = Ty_label(), yrep = Tyrep_label())
    ) +
    scale_color_manual(
      name = lgnd_title,
      values = setNames(get_color(c("dh", "lh")), c("y", "yrep")),
      labels = c(y = Ty_label(), yrep = Tyrep_label())
    ) +
    # labs(
    #   x = Tyrep_label_2d(1),
    #   y = Tyrep_label_2d(2),
    #   subtitle = paste0("(Stat1 = ", stat[1], ", Stat2 = ", stat[2], ")")
    # ) +
    theme_default() +
    xaxis_title(FALSE) +
    yaxis_title(FALSE)
}
