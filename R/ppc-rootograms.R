#' PPC rootograms
#'
#' Rootograms allow for diagnosing problems in count data models such as
#' overdispersion or excess zeros. They consist of a histogram of \code{y} with
#' the expected counts based on \code{yrep} overlaid as a line along with
#' uncertainty intervals. The histogram style may be adjusted to focus on
#' different aspects of the data. The y-axis represents the square roots of the
#' counts to approximately adjust for scale differences and thus ease comparison
#' between observed and expected counts. For a detailed introduction to
#' rootograms see Kleiber and Zeileis (2016).
#'
#' @name PPC-rootograms
#' @family PPCs
#'
#' @template args-y-yrep
#' @param style The rootogram style. The options are \code{"standing"},
#'   \code{"hanging"}, and \code{"suspended"}. See the \strong{Plot
#'   Descriptions} section, below, for details on the different styles.
#' @param ... Currently unused.
#' @param prob The probability mass to include in the uncertainty interval
#'   around the square roots of the expected counts. Defaults to \code{0.9}. Set
#'   \code{prob=0} to remove the uncertainty interval.
#' @param size Passed to \code{\link[ggplot2]{geom_line}}.
#'
#' @details For rootograms, the observations \code{y} and predictons \code{yrep}
#'   must be counts. \pkg{bayesplot} will validate that both \code{y} and
#'   \code{yrep} contain only non-negative integer values (although they need
#'   not be integers in the sense of \R's \code{\link{integer}} type).
#'
#' @section Plot Descriptions:
#' \describe{
#' \item{\code{ppc_rootogram}}{}
#'  \itemize{
#'   \item \emph{Standing}: basic histogram of observed counts with curve
#'   showing expected counts.
#'   \item \emph{Hanging}: observed counts counts hanging from the curve
#'   representing expected counts.
#'   \item \emph{Suspended}: histogram of the differences between expected and
#'   observed counts.
#'  }
#' \strong{All of these are plotted on the square root scale}. See Kleiber and
#' Zeileis (2016) for advice on interpreting rootograms and selecting among the
#' different styles.
#' }
#'
#' @template return-ggplot
#'
#' @references
#' Kleiber, C. and Zeileis, A. (2016). Visualizing count data regressions using
#' rootograms. \emph{The American Statistician}. 70(3): 296--303.
#' \url{https://arxiv.org/abs/1605.01311}.
#'
#' @examples
#' y <- rpois(100, 20)
#' yrep <- matrix(rpois(10000, 20), ncol = 100)
#'
#' ppc_rootogram(y, yrep)
#' ppc_rootogram(y, yrep, style = "hanging", prob = 0.8)
#' ppc_rootogram(y, yrep, style = "suspended")
#'
NULL

#' @rdname PPC-rootograms
#' @export
ppc_rootogram <- function(y, yrep,
                          style = c("standing", "hanging", "suspended"),
                          ..., prob = 0.9, size = 1) {
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
    tyrep[[i]] <- table(yrep[i, ])
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
    aes_(ymin = ~ tylower, ymax = ~ tyupper, height = ~ ty) +
    geom_tile(
      aes_(x = ~ xpos, y = ~ ypos, fill = "Observed"),
      color = get_color("lh"),
      size = 0.25,
      width = 1
    )

  if (style != "standing")
    graph <- graph + hline_0(size = 0.4)

  graph <- graph +
    geom_smooth(
      aes_(x = ~ xpos, y = ~ tyexp, color = "Expected"),
      fill = get_color("d"),
      size = size,
      stat = "identity"
    ) +
    scale_fill_manual("", values = get_color("l")) +
    scale_color_manual("", values = get_color("dh")) +
    labs(
      x = expression(italic(y)),
      y = expression(sqrt(Count))
    )

  if (style == "standing")
    graph <- graph + dont_expand_y_axis()

  graph +
    theme_default() +
    no_legend_spacing()
}
