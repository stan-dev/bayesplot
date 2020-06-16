#' PPD vs predictors
#'
#' Plot posterior (or prior) predictive draws vs a predictor (or time)
#' variable `x`.
#'
#' @name PPD-vs-predictors
#' @family PPDs
#'
#' @template args-ypred
#' @template args-group
#' @template args-facet_args
#' @param size,alpha Passed to [ggplot2::geom_line()].
#' @param x A numeric vector to use as the x-axis
#'   variable. For example, `x` could be a predictor variable from a
#'   regression model, a time variable for time-series models, etc. If `x`
#'   is missing or `NULL` then the observation index is used for the x-axis.
#'
#' @template return-ggplot-or-data
#'
#' @examples
#'
#' \dontrun{
#' # example using rstanarm package
#' library(rstanarm)
#' library(dplyr)
#' library(ggplot2)
#'
#' color_scheme_set("brightblue")
#'
#' # data to use for model fitting
#' mtcars2 <- mutate(
#'   mtcars,
#'   log_mpg = log(mpg),
#'   log_wt1000 = log(wt), # log(wt in 1000s of lbs), see ?mtcars
#'   transmission = factor(am, labels = c("automatic", "manual"))
#' )
#'
#' fit <- stan_glm(log_mpg ~ log_wt1000 + transmission, data = mtcars2,
#'                 refresh = 0)
#' print(fit)
#'
#' # data frame of new data we'll use to make predictions for log_mpg
#' new_data <- expand.grid(
#'   log_wt1000 = log(seq(1, 6, by = 0.1)),
#'   transmission = c("automatic", "manual")
#' )
#'
#' # plot expected log_mpg vs log_wt for manual transmissions
#' # first we'll do it by creating some intermediate objects and then we'll
#' # show the same thing using the pipe operator (%>%)
#'
#' xy_labs <- labs(x = "log_wt1000", y = "Expected log_mpg",
#'                 subtitle = "For transmission='manual'")
#' new_data_manual <- subset(new_data, transmission == "manual")
#'
#' # use posterior_linpred() instead of posterior_predict() for demonstration
#' # to show (smooth) expected prediction and not (noiser) predictions.
#' # see later in example for posterior_predict()
#' set.seed(101)
#' log_mpg_manual <-
#'   posterior_linpred(fit,
#'                     newdata = new_data_manual,
#'                     draws = 200)
#'
#' ppd_curve_overlay(log_mpg_manual,
#'                   x = new_data_manual$log_wt,
#'                   alpha = 0.25) + xy_labs
#'
#' # all in one go with the piping
#' set.seed(101)
#' new_data %>%
#'   dplyr::filter(transmission == "manual") %>%
#'   posterior_linpred(fit, newdata = ., draws = 200) %>%
#'   ppd_curve_overlay(x = unique(new_data$log_wt), alpha = 0.25) +
#'   xy_labs
#'
#'
#' # same plot but not on the log scale (will be nonlinear)
#' ppd_curve_overlay(
#'   exp(log_mpg_manual),
#'   x = exp(new_data_manual$log_wt1000),
#'   size = 0.1,
#'   alpha = 0.33
#'  )
#'
#'
#' # group by transmission type (using full new_data, not just for manual)
#' new_data %>%
#'   posterior_linpred(fit, newdata = ., draws = 200) %>%
#'   ppd_curve_overlay_grouped(
#'     ypred = exp(.),
#'     x = exp(new_data$log_wt),
#'     group = new_data$transmission,
#'     alpha = 0.1
#'   )
#'
#' # contrast that plot with ppd_ribbon(), which plots uncertainty intervals instead
#' # of the underlying draws
#' new_data %>%
#'   posterior_linpred(fit, newdata = ., draws = 200) %>%
#'   ppd_ribbon_grouped(
#'     ypred = exp(.),
#'     x = exp(new_data$log_wt),
#'     group = new_data$transmission,
#'     facet_args = list(scales = "fixed")
#'   )
#'
#'
#' # now try with posterior_predict() instead of posterior_linpred(),
#' # which also takes sigma into account
#' new_data %>%
#'   posterior_predict(fit, newdata = ., draws = 200) %>%
#'   ppd_curve_overlay_grouped(
#'     ypred = exp(.),
#'     x = exp(new_data$log_wt),
#'     group = new_data$transmission,
#'     alpha = 0.1
#'   )
#'
#' # compare to ppd_ribbon()
#' new_data %>%
#'   posterior_predict(fit, newdata = ., draws = 200) %>%
#'   ppd_ribbon_grouped(
#'     ypred = exp(.),
#'     x = exp(new_data$log_wt),
#'     group = new_data$transmission,
#'     alpha = 0.1,
#'     facet_args = list(scales = "fixed")
#'   )
#'
#' }
#'
NULL

#' @rdname PPD-vs-predictors
#' @export
ppd_curve_overlay <-
  function(ypred,
           x = NULL,
           ...,
           size = 0.25,
           alpha = 0.5) {

    dots <- list(...)
    if (!from_grouped(dots)) {
      check_ignored_arguments(...)
      dots$group <- NULL
    }

    data <- ppd_curve_data(ypred, x = x, group = dots$group)
    ggplot(data, aes_(x = ~ x, y = ~ value)) +
      geom_line(
        mapping = aes_(group = ~ rep_label, color = "ypred"),
        size = size,
        alpha = alpha
      ) +
      scale_color_ppd() +
      labs(y = ypred_label(), x = expression(italic(x))) +
      bayesplot_theme_get() +
      legend_none()
  }

#' @rdname PPD-vs-predictors
#' @export
ppd_curve_overlay_grouped <-
  function(ypred,
           x = NULL,
           group,
           ...,
           facet_args = list(),
           size = 0.25,
           alpha = 0.5) {
    check_ignored_arguments(...)
    call <- match.call(expand.dots = FALSE)
    g <- eval(ungroup_call("ppd_curve_overlay", call), parent.frame())

    facet_args[["facets"]] <- "group"
    g +
      do.call("facet_wrap", facet_args) +
      force_axes_in_facets()
  }


#' @rdname PPD-vs-predictors
#' @export
ppd_curve_data <- function(ypred, x, group = NULL) {
  ypred <- validate_predictions(ypred)
  x <- validate_x(x, y = ypred[1, ])
  if (!is.null(group)) {
    group <- validate_group(group, ncol(ypred))
  }
  .ppd_curve_data(ypred, y = NULL, x = x, group = group)
}

#' @export
ppc_curve_data <- function(y, yrep, x, group = NULL) {
  y <- validate_y(y)
  yrep <- validate_predictions(yrep, length(y))
  x <- validate_x(x, y)
  if (!is.null(group)) {
    group <- validate_group(group, length(y))
  }
  .ppd_curve_data(yrep, y = y, x = x, group = group)
}



# internal ----------------------------------------------------------------

#' Back end for both `ppd_curve_data()` and `ppc_curve_data()`
#'
#' @noRd
#' @param predictions SxN matrix of predictions (`ypred` or `yrep`) already validated.
#' @param y `NULL` or user's `y` argument already validated.
#' @param group `NULL` or user's `group` argument, already validated.
#' @param x User's `x` argument, already validated.
.ppd_curve_data <- function(predictions, y = NULL, x, group = NULL) {
  data <- .ppd_data(predictions, y = y, group = group)
  data <- tibble::add_column(data, x = x[data$y_id], .before = "y_id")
  data
}

