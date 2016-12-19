#' PPC rootograms
#'
#' FIXME: add description
#'
#' @name PPC-rootograms
#' @family PPCs
#'
#' @template args-y-yrep
#' @param style Rootogram style. Either \code{"standing"} or \code{"suspended"}.
#' @param ... Currently unused.
#' @param size Passed to \code{\link[ggplot2]{geom_line}}.
#'
NULL

#' @rdname PPC-rootograms
#' @export
ppc_rootogram <- function(y, yrep, style = c("standing", "suspended"), ..., size = 1) {
  suspended <- match.arg(style) == "suspended"
  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)

  ymax <- max(y, yrep)
  x <- 0L:ymax

  # prepare a table for yrep
  tyrep <- apply(yrep, 1, table)
  for (i in seq_along(tyrep)) {
    tyrep[[i]] <- as.numeric(tyrep[[i]][match(x, rownames(tyrep[[i]]))])
  }
  tyrep <- do.call(rbind, tyrep)
  tyrep[is.na(tyrep)] <- 0
  tyexp <- sqrt(colMeans(tyrep))

  # prepare a table for y
  ty <- table(y)
  ty <- sqrt(as.numeric(ty[match(x, rownames(ty))]))
  if (suspended) {
    ty <- ty - tyexp
  }
  ty[is.na(ty)] <- 0

  graph <- ggplot(data.frame(x, tyexp, ty)) +
    geom_col(
      aes_(x = ~ x, y = ~ ty, fill = "Observed"),
      color = get_color("lh"),
      size = 0.25,
      width = 1
    ) +

  if (suspended)
    graph <- graph + hline_0(size = 0.4)

  graph <- graph +
    geom_line(
      aes_(x = ~ x, y = ~ tyexp, color = "Expected"),
      size = size
    ) +
    scale_fill_manual("", values = get_color("l")) +
    scale_color_manual("", values = get_color("dh")) +
    labs(
      x = expression(italic(y)),
      y = expression(sqrt(Count))
    )

  if (!suspended)
    graph <- graph + dont_expand_y_axis()

  graph +
    bayesplot::theme_default() +
    no_legend_spacing()
}
