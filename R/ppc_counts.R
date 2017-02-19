#' Bar plots for count, multinomial, and ordinal models
#'
#' @export
#' @template args-y-yrep
#' @param prob The probability mass to include in the uncertainty interval
#'   around the median predicted counts. Defaults to \code{0.9}.
#'
#' @details For \code{ppc_counts}, the observations \code{y} and predictons
#'   \code{yrep} must be non-negative integers. \pkg{bayesplot} will validate
#'   that both \code{y} and \code{yrep} contain only non-negative integer values
#'   (although they need not be integers in the strict sense of \R's
#'   \code{\link{integer}} type).
#'
ppc_counts <- function(y, yrep, ..., prob = 0.9) {
  check_ignored_arguments(...)
  y <- validate_y(y)
  yrep <- validate_yrep(yrep, y)
  if (!all_counts(y))
    stop("ppc_counts expects counts as inputs to 'y'.")
  if (!all_counts(yrep))
    stop("ppc_counts expects counts as inputs to 'yrep'.")

 alpha <- (1 - prob) / 2
 probs <- sort(c(alpha, 0.5, 1 - alpha))

 y_data <- data.frame(y = y)

 yrep_tab <- t(apply(yrep, 1, table)) # draws by number of categories
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
     mapping = aes(x = y, fill = "y")
   ) +
   geom_pointrange(
     data = yrep_data,
     mapping = aes(x = x, y = mid, ymin = lo, ymax = hi, color = "yrep")
   ) +
   scale_fill_manual("", values = get_color("l"), labels = y_label()) +
   scale_color_manual("", values = get_color("d"), labels = yrep_label()) +
   guides(
     color = guide_legend(order = 1),
     fill = guide_legend(order = 2)
   ) +
   theme_default() +
   yaxis_title(FALSE)
}
