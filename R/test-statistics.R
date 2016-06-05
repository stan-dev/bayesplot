#' Test statistics
#'
#' The distribution of a test statistic \code{T(yrep)}, or a
#' pair of test statistics, over the simulated datasets in \code{yrep},
#' compared to the observed value \code{T(y)}.
#'
#' @name TestStatistics
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
#' @template seealso-color-scheme
#'
#' @section Plot Descriptions:
#' \describe{
#'   \item{\code{ppc_stat}}{
#'    A histogram of the distribution of a test statistic computed by applying
#'    \code{stat} to each dataset (row) in \code{yrep}. The value of the
#'    statistic in the observed data, \code{stat(y)}, is overlaid as a vertical
#'    line.
#'
#'    \if{html}{\figure{stat.png}{options: width="30\%" alt="Figure: stat.png"}}
#'    \if{latex}{\figure{stat.png}{options: width=4cm}}
#'   }
#'   \item{\code{ppc_stat_grouped}}{
#'    The same as \code{ppc_stat}, but a separate plot is generated for
#'    each level of a grouping variable.
#'
#'    \if{html}{\figure{statGrouped.png}{options: width="30\%" alt="Figure: stat_grouped.png"}}
#'    \if{latex}{\figure{statGrouped.png}{options: width=4cm}}
#'   }
#'   \item{\code{ppc_stat_2d}}{
#'    A scatterplot showing the joint distribution of two test statistics
#'    computed over the datasets (rows) in \code{yrep}. The value of the
#'    statistics in the observed data is overlaid as large point.
#'
#'    \if{html}{\figure{stat2d.png}{options: width="30\%" alt="Figure: stat_2d.png"}}
#'    \if{latex}{\figure{stat2d.png}{options: width=4cm}}
#'   }
#' }
#'
#' @examples
#' y <- rnorm(500)
#' yrep <- matrix(rnorm(1e5), ncol = 500)
#' ppc_stat(y, yrep)
#' ppc_stat(y, yrep, stat = "var", binwidth = 0.1)
#' ppc_stat_2d(y, yrep)
#' ppc_stat_2d(y, yrep, stat = c("median", "mean"))
#'
#' group <- gl(3, 10, length = length(y), labels = LETTERS[1:3])
#' ppc_stat_grouped(y, yrep, group)
#'
#' # use your own function to compute test statistics
#' q25 <- function(y) quantile(y, 0.25)
#' ppc_stat(y, yrep, stat = "q25")
#'
NULL

#' @export
#' @rdname TestStatistics
#'
ppc_stat <- function(y, yrep, stat = "mean", ..., binwidth = NULL) {
  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)
  stat <- validate_stat(stat, 1)

  stat1 <- match.fun(stat)
  T_y <- stat1(y)
  T_yrep <- apply(yrep, 1, stat1)

  ggplot(
    data = data.frame(x = T_yrep),
    mapping = aes_string(
      x = "x",
      y = "..density..",
      color = "'A'"
    )
  ) +
    .ppc_stat_histogram(binwidth) +
    geom_vline(
      data = data.frame(t = T_y),
      mapping = aes_string(xintercept = "t", color = "factor(t)"),
      size = 2,
      show.legend = TRUE
    ) +
    scale_color_manual(
      name = "",
      values = ppc_color(c("dark", "light")),
      labels = c(Ty_label(), Tyrep_label())
    ) +
    xlab(paste("Stat =", stat)) +
    dont_expand_y_axis() +
    theme_ppc(
      y_text = FALSE,
      legend_position = "right"
    )
}

#' @export
#' @rdname TestStatistics
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
      mapping = aes_string(x = "value", y = "..density..")
    ) +
      .ppc_stat_histogram(binwidth) +
      geom_vline(
        data = plot_data[is_y, , drop = FALSE],
        mapping = aes_string(xintercept = "value"),
        color = ppc_color("dark"),
        size = 2
      ) +
      facet_wrap("group", scales = "free") +
      xlab(paste("Stat =", stat)) +
      dont_expand_y_axis() +
      theme_ppc(y_text = FALSE)
  }


#' @export
#' @rdname TestStatistics
#'
ppc_stat_2d <- function(y, yrep, stat = c("mean", "sd"), ...) {
  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)
  stat <- validate_stat(stat, 2)

  stat1 <- match.fun(stat[1])
  stat2 <- match.fun(stat[2])
  T_y1 <- stat1(y)
  T_y2 <- stat2(y)
  T_yrep1 <- apply(yrep, 1, stat1)
  T_yrep2 <- apply(yrep, 1, stat2)

  ggplot(
    data = data.frame(x = T_yrep1, y = T_yrep2),
    mapping = aes_string(x = "x", y = "y")
  ) +
    geom_point(
      shape = 21,
      size = 2,
      fill = ppc_color("light"),
      color = ppc_color("light_highlight")
    ) +
    annotate(
      geom = "segment",
      x = c(T_y1, -Inf),
      xend = c(T_y1, T_y1),
      y = c(-Inf, T_y2),
      yend = c(T_y2, T_y2),
      linetype = 2,
      size = 0.4,
      color = ppc_color("dark_highlight")
    ) +
    geom_point(
      data = data.frame(x = T_y1, y = T_y2),
      mapping = aes_string(
        x = "x",
        y = "y",
        fill = "'Ty'",
        color = "'Ty'"
      ),
      size = 4,
      shape = 21,
      stroke = 1
    ) +
    scale_fill_manual(
      name = "",
      values = c('Ty' = ppc_color("dark")),
      labels = c('Ty' = Ty_label())
    ) +
    scale_color_manual(
      name = "",
      values = c('Ty' = ppc_color("dark_highlight")),
      labels = c('Ty' = Ty_label())
    ) +
    labs(
      x = paste("Stat =", stat[1]),
      y = paste("Stat =", stat[2])
    ) +
    theme_ppc(
      y_text = TRUE,
      legend_position = "right"
    )
}


# helpers -----------------------------------------------------------------
.ppc_stat_histogram <- function(binwidth) {
  geom_histogram(
    fill = ppc_color("light"),
    color = ppc_color("light_highlight"),
    size = .25,
    na.rm = TRUE,
    binwidth = binwidth
  )
}
