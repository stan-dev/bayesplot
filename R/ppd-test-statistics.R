#' PPD test statistics
#'
#' The distribution of a (test) statistic `T(ypred)`, or a pair of (test)
#' statistics, over the simulations from the posterior or prior predictive
#' distribution. Each of these functions makes the same plot as the
#' corresponding [`ppc_`][PPC-test-statistics] function but without comparing to
#' any observed data `y`. The **Plot Descriptions** section at
#' [PPC-test-statistics] has details on the individual plots.
#'
#' @name PPD-test-statistics
#' @aliases PPD-statistics
#' @family PPDs
#'
#' @template args-ypred
#' @inheritParams PPC-test-statistics
#'
#' @template details-binomial
#' @template return-ggplot-or-data
#'
#' @template reference-vis-paper
#' @examples
#' yrep <- example_yrep_draws()
#' ppd_stat(yrep)
#' ppd_stat(yrep, stat = "sd") + legend_none()
#'
#' # use your own function for the 'stat' argument
#' color_scheme_set("brightblue")
#' q25 <- function(y) quantile(y, 0.25)
#' ppd_stat(yrep, stat = "q25") # legend includes function name
NULL

#' @rdname PPD-test-statistics
#' @export
ppd_stat <-
  function(ypred,
           stat = "mean",
           ...,
           discrete = FALSE,
           binwidth = NULL,
           bins = NULL,
           breaks = NULL,
           freq = TRUE) {
    stopifnot(length(stat) == 1)
    dots <- list(...)
    if (!from_grouped(dots)) {
      check_ignored_arguments(...)
      dots$group <- NULL
    }

    data <- ppd_stat_data(
      ypred = ypred,
      group = dots$group,
      stat = match.fun(stat)
    )
    graph <- ggplot(data, mapping = set_hist_aes(
      freq,
      color = "ypred",
      fill = "ypred"
    ))
    graph <- graph + if (discrete) {
      geom_bar(
        color = get_color("lh"),
        linewidth = 0.25,
        na.rm = TRUE,
        position = "identity",
      )
    }
    else {
      geom_histogram(
        linewidth = 0.25,
        na.rm = TRUE,
        binwidth = binwidth,
        bins = bins,
        breaks = breaks
      ) }
      graph +
      scale_color_ppd(guide = "none") +
      scale_fill_ppd(labels = Typred_label(), guide = guide_legend(
        title = stat_legend_title(stat, deparse(substitute(stat)))
      )) +
      bayesplot_theme_get() +
      dont_expand_y_axis() +
      xaxis_title(FALSE) +
      yaxis_text(FALSE) +
      yaxis_ticks(FALSE) +
      yaxis_title(FALSE)
  }


#' @rdname PPD-test-statistics
#' @export
ppd_stat_grouped <-
  function(ypred,
           group,
           stat = "mean",
           ...,
           discrete = FALSE,
           facet_args = list(),
           binwidth = NULL,
           bins = NULL,
           breaks = NULL,
           freq = TRUE) {
    check_ignored_arguments(...)
    call <- match.call(expand.dots = FALSE)
    g <- eval(ungroup_call("ppd_stat", call), parent.frame())
    g +
      stat_group_facets(facet_args) +
      force_axes_in_facets()
  }


#' @rdname PPD-test-statistics
#' @export
ppd_stat_freqpoly <-
  function(ypred,
           stat = "mean",
           ...,
           facet_args = list(),
           binwidth = NULL,
           bins = NULL,
           freq = TRUE) {
    stopifnot(length(stat) == 1)
    dots <- list(...)
    if (!from_grouped(dots)) {
      check_ignored_arguments(...)
      dots$group <- NULL
    }

    data <- ppd_stat_data(
      ypred = ypred,
      group = dots$group,
      stat = match.fun(stat)
    )
    ggplot(data, mapping = set_hist_aes(freq)) +
      geom_freqpoly(
        aes(color = "ypred"),
        linewidth = 0.5,
        na.rm = TRUE,
        binwidth = binwidth,
        bins = bins
      ) +
      scale_color_ppd(
        name = stat_legend_title(stat, deparse(substitute(stat))),
        labels = Typred_label()
      ) +
      dont_expand_y_axis(c(0.005, 0)) +
      bayesplot_theme_get() +
      xaxis_title(FALSE) +
      yaxis_text(FALSE) +
      yaxis_ticks(FALSE) +
      yaxis_title(FALSE)
  }


#' @rdname PPD-test-statistics
#' @export
ppd_stat_freqpoly_grouped <-
  function(ypred,
           group,
           stat = "mean",
           ...,
           facet_args = list(),
           binwidth = NULL,
           bins = NULL,
           freq = TRUE) {
    check_ignored_arguments(...)
    call <- match.call(expand.dots = FALSE)
    g <- eval(ungroup_call("ppd_stat_freqpoly", call), parent.frame())
    g +
      stat_group_facets(facet_args) +
      force_axes_in_facets()
  }


#' @rdname PPD-test-statistics
#' @export
ppd_stat_2d <-
  function(ypred,
           stat = c("mean", "sd"),
           ...,
           size = 2.5,
           alpha = 0.7) {
    check_ignored_arguments(...)
    if (length(stat) != 2) {
      abort("For ppd_stat_2d the 'stat' argument must have length 2.")
    }

    if (is.character(stat)) {
      lgnd_title <- bquote(italic(T) == (list(.(stat[1]), .(stat[2]))))
      stat_labs <- stat
    } else {
      lgnd_title <- expression(italic(T) == (list(italic(T)[1], italic(T)[2])))
      stat_labs <- expression(italic(T)[1], italic(T)[2])
    }

    data <- ppd_stat_data(
      ypred = ypred,
      group = NULL,
      stat = c(match.fun(stat[[1]]), match.fun(stat[[2]]))
    )
    ggplot(data) +
      geom_point(
        mapping = aes(
          x = .data$value,
          y = .data$value2,
          fill = "ypred",
          color = "ypred"
        ),
        shape = 21,
        size = size,
        alpha = alpha
      ) +
      scale_fill_ppd(lgnd_title, labels = Typred_label()) +
      scale_color_ppd(lgnd_title, labels = Typred_label()) +
      labs(x = stat_labs[1], y = stat_labs[2]) +
      bayesplot_theme_get()
  }


#' @rdname PPD-test-statistics
#' @export
ppd_stat_data <- function(ypred, group = NULL, stat) {
  if (!(length(stat) %in% 1:2)) {
    abort("'stat' must have length 1 or 2.")
  }

  ypred <- validate_predictions(ypred)
  if (!is.null(group)) {
    group <- validate_group(group, ncol(ypred))
  }

  if (length(stat) == 1) {
    stat <- match.fun(stat)
  } else {
    stat <- list(match.fun(stat[[1]]), match.fun(stat[[2]]))
  }

  .ppd_stat_data(
    predictions = ypred,
    y = NULL,
    group = group,
    stat = stat
  )
}

# internal ----------------------------------------------------------------

#' Back end for both `ppd_stat_data()` and `ppc_stat_data()`.
#'
#' @noRd
#' @param predictions,y,group Already validated `y`, `yrep` or `ypred`, and
#'   `group` objects.
#' @param stat A function already validated and returned by `match.fun()`, or a
#'   list of two such functions.
#' @return A data frame with columns `group` (if not `NULL`), `variable`,
#'   `value`, and `value2` (if `stat` contains two functions).
#'
#' @examples
#' y <- example_y_data()
#' yrep <- example_yrep_draws()
#' group <- example_group_data()
#' ppd_stat_data(yrep, group, stat = "median")
#' ppc_stat_data(y, yrep, group, stat = "median")
#'
#' @importFrom dplyr group_by ungroup summarise rename
.ppd_stat_data <- function(predictions, y = NULL, group = NULL, stat) {
  stopifnot(length(stat) %in% c(1,2))
  if (length(stat) == 1) {
    stopifnot(is.function(stat)) # sanity check, should already be validated
    stat1 <- stat
    stat2 <- NULL
  } else { # two stats
    stopifnot(is.function(stat[[1]]), is.function(stat[[2]]))
    stat1 <- stat[[1]]
    stat2 <- stat[[2]]
  }

  has_group <- !is.null(group)
  has_y <- !is.null(y)

  if (!has_group) {
    group <- 1
  }
  if (!has_y) {
    y <- 1
  }

  d <- data.frame(
    y = y,
    group = factor(group),
    ypred = t(predictions)
  )
  colnames(d) <- gsub(".", "_", colnames(d), fixed = TRUE)
  molten_d <- reshape2::melt(d, id.vars = "group")
  molten_d <- group_by(molten_d, .data$group, .data$variable)

  data <-
    molten_d %>%
    summarise(
      value1 = stat1(.data$value),
      value2 = if (!is.null(stat2))
        stat2(.data$value) else NA
    ) %>%
    rename(value = "value1") %>%
    ungroup()

  if (is.null(stat2)) {
    data$value2 <- NULL
  }
  if (!has_group) {
    data$group <- NULL
  }
  if (!has_y) {
    data <- dplyr::filter(data, .data$variable != "y")
    data$variable <- droplevels(data$variable)
  } else {
    levels(data$variable) <- gsub("ypred", "yrep", levels(data$variable))
  }

  data
}


# Create the facet layer for grouped stat plots
stat_group_facets <- function(facet_args, scales_default = "free") {
  facet_args[["facets"]] <- "group"
  facet_args[["scales"]] <- facet_args[["scales"]] %||% scales_default
  do.call("facet_wrap", facet_args)
}

Typred_label <- function() expression(italic(T)(italic(y)[pred]))
