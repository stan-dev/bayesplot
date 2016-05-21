#' Test statistics
#'
#' The distribution of a single test statistic \eqn{{T(y^{rep})}}{T(yrep)}, or a
#' pair of test statistics over the \code{nreps} simulated datasets, compared
#' to the observed value \eqn{T(y)}.
#'
#' @export
#' @family PPCs
#'
#' @template args-ppc
#' @param stat A character vector (of length 1 or 2) naming a single function or
#'   a pair of functions. The function(s) should take a vector input and return
#'   a scalar test statistic. If \code{stat} specifies a single function then
#'   the resulting plot is a histogram of \eqn{{T(y^{rep})}}{T(yrep)} and the
#'   value of the test statistic in the observed data, \eqn{T(y)}, is shown in
#'   the plot as a vertical line. If two functions are specified then the plot
#'   is a scatterplot and \eqn{T(y)} is shown as a large point.
#' @param ... Optional arguments to \code{\link[ggplot2]{geom_histogram}} (if
#'   \code{stat} is a single function) or \code{\link[ggplot2]{geom_point}} (if
#'   \code{stat} specifies two functions).
#'
#' @template details-ppc
#' @template return-ggplot
#' @templateVar bdaRef (Ch. 6)
#' @template reference-bda
#'
#' @examples
#' y <- rnorm(30)
#' yrep <- matrix(rnorm(3000), ncol = 30)
#' ppc_stat(y, yrep)
#' ppc_stat(y, yrep, stat = "var")
#' ppc_stat(y, yrep, stat = c("mean", "sd"))
#'
#' # define a custom test statistic
#' q25 <- function(y) quantile(y, 0.25)
#' ppc_stat(y, yrep, stat = "q25")
#'
ppc_stat <- function(y, yrep, stat = "mean", ...) {
  validate_y_and_yrep(y, yrep)

  vline_color <- .PP_DARK
  fill_color <- .PP_LIGHT
  outline_color <- .PP_LIGHT_highlight

  if (!length(stat) || length(stat) > 2)
    stop("'stat' should have length 1 or 2.")
  if (!is.character(stat))
    stop("'stat' should be a character vector.")

  if (length(stat) == 1) {
    defaults <- list(fill = fill_color, color = outline_color,
                     size = .25, na.rm = TRUE)
    geom_args <- set_geom_args(defaults, ...)
    geom_args$mapping <- aes_string(y = "..density..")
    geom_args$show.legend <- FALSE

    stat1 <- match.fun(stat)
    T_y <- stat1(y)
    T_yrep <- apply(yrep, 1, stat1)
    base <- ggplot(data.frame(x = T_yrep), aes_string(x = "x", color = "'A'"))
    graph <- base +
      call_geom("histogram", geom_args) +
      geom_vline(
        data = data.frame(t = T_y),
        mapping = aes_string(xintercept = "t", color = "factor(t)"),
        size = 2,
        show.legend = TRUE
      ) +
      scale_color_manual(
        name = "",
        values = c(vline_color, fill_color),
        labels = c("T(y)", "T(yrep)")
      ) +
      xlab(paste("Stat =", stat))

    thm <- theme_ppc() %+replace% theme(legend.position = "right")

  } else { # length(stat) == 2
    defaults <- list(
      shape = 21,
      size = 2,
      color = .PP_LIGHT_highlight,
      fill = .PP_LIGHT
    )
    geom_args <- set_geom_args(defaults, ...)

    if (is.character(stat[1]))
      stat1 <- match.fun(stat[1])
    if (is.character(stat[2]))
      stat2 <- match.fun(stat[2])
    T_y1 <- stat1(y)
    T_y2 <- stat2(y)
    T_yrep1 <- apply(yrep, 1, stat1)
    T_yrep2 <- apply(yrep, 1, stat2)

    base <- ggplot(
      data = data.frame(x = T_yrep1, y = T_yrep2),
      mapping = aes_string(x = "x", y = "y")
    )
    graph <- base +
      call_geom("point", geom_args) +
      annotate(
        geom = "segment",
        x = c(T_y1, -Inf),
        xend = c(T_y1, T_y1),
        y = c(-Inf, T_y2),
        yend = c(T_y2, T_y2),
        color = .PP_DARK_highlight,
        linetype = 2,
        size = 0.5
      ) +
      geom_point(
        data = data.frame(x = T_y1, y = T_y2),
        mapping = aes_string(x = "x", y = "y", fill = "'Ty'", color = "'Ty'"),
        size = 4,
        shape = 21,
        stroke = 1
      ) +
      scale_fill_manual(
        name = "",
        values = c('Ty' = .PP_DARK),
        labels = c('Ty' = "T(y)")
      ) +
      scale_color_manual(
        name = "",
        values = c('Ty' = .PP_DARK_highlight),
        labels = c('Ty' = "T(y)")
      ) +
      labs(
        x = paste("Stat =", stat[1]),
        y = paste("Stat =", stat[2])
      )

    thm <- theme_ppc(y_text = TRUE, legend_position = "right")
  }

  graph + thm
}
