#' PPC censoring
#'
#' @description Compare the empirical distribution of censored data `y` to the
#'   distributions of simulated/replicated data `yrep` from the posterior
#'   predictive distribution. See the **Plot Descriptions** section, below, for
#'   details.
#'
#'   Although some of the other \pkg{bayesplot} plots can be used with censored
#'   data, `ppc_km_overlay()` is currently the only plotting function designed
#'   *specifically* for censored data. We encourage you to suggest or contribute
#'   additional plots at
#'   [github.com/stan-dev/bayesplot](https://github.com/stan-dev/bayesplot).
#'
#' @name PPC-censoring
#' @family PPCs
#'
#' @template args-y-yrep
#' @param size,alpha Passed to the appropriate geom to control the appearance of
#'   the `yrep` distributions.
#' @param ... Currently only used internally.
#'
#' @template return-ggplot
#'
#' @section Plot Descriptions:
#' \describe{
#'   \item{`ppc_km_overlay()`}{
#'    Empirical CCDF estimates of each dataset (row) in `yrep` are overlaid,
#'    with the Kaplan-Meier estimate (Kaplan and Meier, 1958) for `y` itself on
#'    top (and in a darker shade). This is a PPC suitable for right-censored
#'    `y`. Note that the replicated data from `yrep` is assumed to be
#'    uncensored.
#'   }
#'   \item{`ppc_km_overlay_grouped()`}{
#'    The same as `ppc_km_overlay()`, but with separate facets by `group`.
#'   }
#' }
#'
#' @templateVar bdaRef (Ch. 6)
#' @template reference-bda
#' @template reference-km
#'
#' @examples
#' color_scheme_set("brightblue")
#' y <- example_y_data()
#' # For illustrative purposes, (right-)censor values y > 110:
#' status_y <- as.numeric(y <= 110)
#' y <- pmin(y, 110)
#' # In reality, the replicated data (yrep) would be obtained from a
#' # model which takes the censoring of y properly into account. Here,
#' # for illustrative purposes, we simply use example_yrep_draws():
#' yrep <- example_yrep_draws()
#' dim(yrep)
#' \donttest{
#' ppc_km_overlay(y, yrep[1:25, ], status_y = status_y)
#' }
#' # With separate facets by group:
#' group <- example_group_data()
#' \donttest{
#' ppc_km_overlay_grouped(y, yrep[1:25, ], group = group, status_y = status_y)
#' }
NULL

#' @export
#' @rdname PPC-censoring
#' @param status_y The status indicator for the observations from `y`. This must
#'   be a numeric vector of the same length as `y` with values in \{0, 1\} (0 =
#'   right censored, 1 = event).
ppc_km_overlay <- function(
  y,
  yrep,
  ...,
  status_y,
  size = 0.25,
  alpha = 0.7
) {
  check_ignored_arguments(..., ok_args = "add_group")
  add_group <- list(...)$add_group

  if(!requireNamespace("survival", quietly = TRUE)){
    abort("Package 'survival' required.")
  }
  if(!requireNamespace("ggfortify", quietly = TRUE)){
    abort("Package 'ggfortify' required.")
  }

  stopifnot(is.numeric(status_y))
  stopifnot(all(status_y %in% c(0, 1)))

  data <- ppc_data(y, yrep, group = status_y)

  # Modify the status indicator:
  #   * For the observed data ("y"), convert the status indicator back to
  #     a numeric.
  #   * For the replicated data ("yrep"), set the status indicator
  #     to 1 ("event"). This way, the Kaplan-Meier estimator reduces
  #     to "1 - ECDF" with ECDF denoting the ordinary empirical cumulative
  #     distribution function.
  data <- data %>%
    dplyr::mutate(group = ifelse(.data$is_y,
                                 as.numeric(as.character(.data$group)),
                                 1))

  sf_form <- survival::Surv(value, group) ~ rep_label
  if(!is.null(add_group)){
    data <- dplyr::inner_join(data,
                              tibble::tibble(y_id = seq_along(y),
                                             add_group = add_group),
                              by = "y_id")
    sf_form <- update(sf_form, . ~ . + add_group)
  }
  sf <- survival::survfit(
    sf_form,
    data = data
  )
  names(sf$strata) <- sub("add_group=", "add_group:", names(sf$strata)) # Needed to split the strata names in ggfortify:::fortify.survfit() properly.
  fsf <- fortify(sf)
  if(any(grepl("add_group", levels(fsf$strata)))){
    strata_split <- strsplit(as.character(fsf$strata), split = ", add_group:")
    fsf$strata <- as.factor(sapply(strata_split, "[[", 1))
    fsf$group <- as.factor(sapply(strata_split, "[[", 2))
  }

  fsf$is_y_color <- as.factor(sub("\\[rep\\] \\(.*$", "rep", sub("^italic\\(y\\)", "y", fsf$strata)))
  fsf$is_y_size <- ifelse(fsf$is_y_color == "yrep", size, 1)
  fsf$is_y_alpha <- ifelse(fsf$is_y_color == "yrep", alpha, 1)

  # Ensure that the observed data gets plotted last by reordering the
  # levels of the factor "strata"
  fsf$strata <- factor(fsf$strata, levels = rev(levels(fsf$strata)))

  ggplot(data = fsf,
         mapping = aes_(x = ~ time,
                        y = ~ surv,
                        color = ~ is_y_color,
                        group = ~ strata,
                        size = ~ is_y_size,
                        alpha = ~ is_y_alpha)) +
    geom_step() +
    hline_at(
      0.5,
      size = 0.1,
      linetype = 2,
      color = get_color("dh")
    ) +
    hline_at(
      c(0, 1),
      size = 0.2,
      linetype = 2,
      color = get_color("dh")
    ) +
    scale_size_identity() +
    scale_alpha_identity() +
    scale_color_ppc_dist() +
    scale_y_continuous(breaks = c(0, 0.5, 1)) +
    xlab(y_label()) +
    yaxis_title(FALSE) +
    xaxis_title(FALSE) +
    yaxis_ticks(FALSE) +
    bayesplot_theme_get()
}

#' @export
#' @rdname PPC-censoring
#' @template args-group
ppc_km_overlay_grouped <- function(
  y,
  yrep,
  group,
  ...,
  status_y,
  size = 0.25,
  alpha = 0.7
) {
  check_ignored_arguments(...)

  p_overlay <- ppc_km_overlay(
    y = y,
    yrep = yrep,
    add_group = group,
    ...,
    status_y = status_y,
    size = size,
    alpha = alpha
  )

  p_overlay +
    facet_wrap("group") +
    force_axes_in_facets()
}
