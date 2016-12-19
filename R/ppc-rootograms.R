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
#' @param prob The probability mass to include in the uncertainty interval
#'   around the expected (root) counts. Defaults to \code{0.9}.
#' @param size Passed to \code{\link[ggplot2]{geom_line}}.
#'
NULL

#' @rdname PPC-rootograms
#' @export
ppc_rootogram <- function(y, yrep, style = c("standing", "suspended"),
                          ..., prob = 0.9, size = 1) {
  suspended <- match.arg(style) == "suspended"
  y <- validate_y(y)
  if (any(!is.wholenumber(y)) || min(y) < 0L) {
    stop("ppc_rootogram expects counts as inputs to 'y'.")
  }
  yrep <- validate_yrep(yrep, y)
  if (any(!is.wholenumber(yrep)) || min(yrep) < 0L) {
    stop("ppc_rootogram expects counts as inputs to 'yrep'.")
  }
  alpha <- (1 - prob) / 2
  probs <- c(alpha, 1 - alpha)
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
  tyquantile <- sqrt(t(apply(tyrep, 2, quantile, probs = probs)))
  colnames(tyquantile) <- c("tylower", "tyupper")

  # prepare a table for y
  ty <- table(y)
  ty <- sqrt(as.numeric(ty[match(x, rownames(ty))]))
  if (suspended) {
    ty <- ty - tyexp
  }
  ty[is.na(ty)] <- 0

  graph <- ggplot(data.frame(x, ty, tyexp, tyquantile)) +
    geom_col(
      aes_(x = ~ x, y = ~ ty, fill = "Observed"),
      color = get_color("lh"),
      size = 0.25,
      width = 1
    )

  if (suspended)
    graph <- graph + hline_0(size = 0.4)

  graph <- graph +
    geom_smooth(
      aes_(x = ~ x, y = ~ tyexp, color = "Expected",
           ymin = ~ tylower, ymax = ~ tyupper),
      fill = get_color("m"),
      size = size,
      stat = "identity"
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

# TODO: move to another file?
is.wholenumber <- function(x, tol = .Machine$double.eps) {
  # check if x consists of whole numbers (very close to integers)
  if (!is.numeric(x)) {
    FALSE
  } else {
    abs(x - round(x)) < tol
  }
}
