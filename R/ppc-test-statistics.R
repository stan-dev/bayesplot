#' PPC test statistics
#'
#' The distribution of a (test) statistic `T(yrep)`, or a pair of (test)
#' statistics, over the simulated datasets in `yrep`, compared to the
#' observed value `T(y)` computed from the data `y`. See the
#' **Plot Descriptions** and **Details** sections, below, as
#' well as [Gabry et al. (2019)](https://github.com/jgabry/bayes-vis-paper#readme).
#'
#' @name PPC-test-statistics
#' @aliases PPC-statistics
#' @family PPCs
#'
#' @template args-y-yrep
#' @template args-group
#' @template args-facet_args
#' @template args-hist
#' @template args-hist-freq
#' @param stat A single function or a string naming a function, except for the
#'   2D plot which requires a vector of exactly two names or functions. In all
#'   cases the function(s) should take a vector input and return a scalar
#'   statistic. If specified as a string (or strings) then the legend will
#'   display the function name(s). If specified as a function (or functions)
#'   then generic naming is used in the legend.
#' @param ... Currently unused.
#'
#' @template details-binomial
#' @template return-ggplot-or-data
#'
#' @template reference-vis-paper
#' @templateVar bdaRef (Ch. 6)
#' @template reference-bda
#'
#' @section Plot Descriptions:
#' \describe{
#'   \item{`ppc_stat()`, `ppc_stat_freqpoly()`}{
#'    A histogram or frequency polygon of the distribution of a statistic
#'    computed by applying `stat` to each dataset (row) in `yrep`. The value of
#'    the statistic in the observed data, `stat(y)`, is overlaid as a vertical
#'    line. More details and example usage of `ppc_stat()` can be found in Gabry
#'    et al. (2019).
#'   }
#'   \item{`ppc_stat_grouped()`,`ppc_stat_freqpoly_grouped()`}{
#'    The same as `ppc_stat()` and `ppc_stat_freqpoly()`, but a separate plot is
#'    generated for each level of a grouping variable. More details and example
#'    usage of `ppc_stat_grouped()` can be found in Gabry et al. (2019).
#'   }
#'   \item{`ppc_stat_2d()`}{
#'    A scatterplot showing the joint distribution of two statistics
#'    computed over the datasets (rows) in `yrep`. The value of the
#'    statistics in the observed data is overlaid as large point.
#'   }
#' }
#'
#' @examples
#' y <- example_y_data()
#' yrep <- example_yrep_draws()
#' ppc_stat(y, yrep)
#' ppc_stat(y, yrep, stat = "sd") + legend_none()
#'
#' # use your own function for the 'stat' argument
#' color_scheme_set("brightblue")
#' q25 <- function(y) quantile(y, 0.25)
#' ppc_stat(y, yrep, stat = "q25") # legend includes function name
#'
#' # can define the function in the 'stat' argument instead of
#' # using its name but then the legend doesn't include the function name
#' ppc_stat(y, yrep, stat = function(y) quantile(y, 0.25))
#'
#' # plots by group
#' color_scheme_set("teal")
#' group <- example_group_data()
#' ppc_stat_grouped(y, yrep, group)
#' ppc_stat_grouped(y, yrep, group) + yaxis_text()
#'
#' # force y-axes to have same scales, allow x axis to vary
#' ppc_stat_grouped(y, yrep, group, facet_args = list(scales = "free_x")) + yaxis_text()
#'
#' # the freqpoly plots use frequency polygons instead of histograms
#' ppc_stat_freqpoly(y, yrep, stat = "median")
#' ppc_stat_freqpoly_grouped(y, yrep, group, stat = "median", facet_args = list(nrow = 2))
#'
#' # ppc_stat_2d allows 2 statistics and makes a scatterplot
#' bayesplot_theme_set(ggplot2::theme_linedraw())
#' color_scheme_set("viridisE")
#' ppc_stat_2d(y, yrep, stat = c("mean", "sd"))
#'
#' bayesplot_theme_set(ggplot2::theme_grey())
#' color_scheme_set("brewer-Paired")
#' ppc_stat_2d(y, yrep, stat = c("median", "mad"))
#'
#' # reset aesthetics
#' color_scheme_set()
#' bayesplot_theme_set()
#'
NULL

#' @rdname PPC-test-statistics
#' @export
ppc_stat <-
  function(y,
           yrep,
           stat = "mean",
           ...,
           binwidth = NULL,
           breaks = NULL,
           freq = TRUE) {
    stopifnot(length(stat) == 1)
    dots <- list(...)
    if (!from_grouped(dots)) {
      check_ignored_arguments(...)
      dots$group <- NULL
    }

    data <- ppc_stat_data(
      y = y,
      yrep = yrep,
      group = dots$group,
      stat = match.fun(stat)
    )
    ggplot(
      data = dplyr::filter(data, .data$variable != "y"),
      mapping = set_hist_aes(freq)
    ) +
      geom_histogram(
        aes_(fill = "yrep"),
        color = get_color("lh"),
        size = .25,
        na.rm = TRUE,
        binwidth = binwidth,
        breaks = breaks
      ) +
      geom_vline(
        data = dplyr::filter(data, .data$variable == "y"),
        mapping = aes_(xintercept = ~ value, color = "y"),
        size = 1.5
      ) +
      scale_color_ppc(values = get_color("dh"), labels = Ty_label()) +
      scale_fill_ppc(values = get_color("l"), labels = Tyrep_label()) +
      guides(
        color = guide_legend(title = NULL),
        fill = guide_legend(
          order = 1,
          title = stat_legend_title(stat, deparse(substitute(stat)))
        )
      ) +
      dont_expand_y_axis() +
      bayesplot_theme_get() +
      no_legend_spacing() +
      xaxis_title(FALSE) +
      yaxis_text(FALSE) +
      yaxis_ticks(FALSE) +
      yaxis_title(FALSE)
  }


#' @rdname PPC-test-statistics
#' @export
ppc_stat_grouped <-
  function(y,
           yrep,
           group,
           stat = "mean",
           ...,
           facet_args = list(),
           binwidth = NULL,
           breaks = NULL,
           freq = TRUE) {
    check_ignored_arguments(...)
    call <- match.call(expand.dots = FALSE)
    g <- eval(ungroup_call("ppc_stat", call), parent.frame())
    g +
      stat_group_facets(facet_args) +
      force_axes_in_facets()
  }


#' @rdname PPC-test-statistics
#' @export
ppc_stat_freqpoly <-
  function(y,
           yrep,
           stat = "mean",
           ...,
           facet_args = list(),
           binwidth = NULL,
           freq = TRUE) {
    stopifnot(length(stat) == 1)
    dots <- list(...)
    if (!from_grouped(dots)) {
      check_ignored_arguments(...)
      dots$group <- NULL
    }

    data <- ppc_stat_data(
      y = y,
      yrep = yrep,
      group = dots$group,
      stat = match.fun(stat)
    )

    ggplot(
      data = dplyr::filter(data, .data$variable != "y"),
      mapping = set_hist_aes(freq)
    ) +
      geom_freqpoly(
        aes_(color = "yrep"),
        size = .5,
        na.rm = TRUE,
        binwidth = binwidth
      ) +
      geom_vline(
        data = dplyr::filter(data, .data$variable == "y"),
        mapping = aes_(xintercept = ~ value, color = "y"),
        show.legend = FALSE,
        size = 1
      ) +
      scale_color_ppc(
        name = stat_legend_title(stat, deparse(substitute(stat))),
        values = set_names(get_color(c("m", "dh")), c("yrep", "y")),
        labels = c(yrep = Tyrep_label(), y = Ty_label())
      ) +
      dont_expand_y_axis(c(0.005, 0)) +
      bayesplot_theme_get() +
      xaxis_title(FALSE) +
      yaxis_text(FALSE) +
      yaxis_ticks(FALSE) +
      yaxis_title(FALSE)
  }


#' @rdname PPC-test-statistics
#' @export
ppc_stat_freqpoly_grouped <-
  function(y,
           yrep,
           group,
           stat = "mean",
           ...,
           facet_args = list(),
           binwidth = NULL,
           freq = TRUE) {
    check_ignored_arguments(...)
    call <- match.call(expand.dots = FALSE)
    g <- eval(ungroup_call("ppc_stat_freqpoly", call), parent.frame())
    g +
      stat_group_facets(facet_args) +
      force_axes_in_facets()
  }


#' @rdname PPC-test-statistics
#' @export
#' @param size,alpha For the 2D plot only, arguments passed to
#'   [ggplot2::geom_point()] to control the appearance of scatterplot points.
ppc_stat_2d <- function(y,
                        yrep,
                        stat = c("mean", "sd"),
                        ...,
                        size = 2.5,
                        alpha = 0.7) {

  check_ignored_arguments(...)

  if (length(stat) != 2) {
    abort("For ppc_stat_2d the 'stat' argument must have length 2.")
  }

  if (is.character(stat)) {
    lgnd_title <- bquote(italic(T) == (list(.(stat[1]), .(stat[2]))))
    stat_labs <- stat
  } else {
    lgnd_title <- expression(italic(T) == (list(italic(T)[1], italic(T)[2])))
    stat_labs <- expression(italic(T)[1], italic(T)[2])
  }

  data <- ppc_stat_data(
    y = y,
    yrep = yrep,
    group = NULL,
    stat = c(match.fun(stat[[1]]), match.fun(stat[[2]]))
  )
  y_segment_data <- stat_2d_segment_data(data)
  y_point_data <- data.frame(
    x = y_segment_data[1, "x"],
    y = y_segment_data[2, "y"]
  )

  ggplot(data) +
    geom_point(
      aes_(
        x = ~ value,
        y = ~ value2,
        fill = "yrep",
        color = "yrep"
      ),
      shape = 21,
      size = size,
      alpha = alpha
    ) +
    geom_segment(
      data = y_segment_data,
      aes_(
        x = ~ x,
        y = ~ y,
        xend = ~ xend,
        yend = ~ yend,
        color = "y"
      ),
      linetype = 2,
      size = 0.4,
      show.legend = FALSE
    ) +
    geom_point(
      data = y_point_data,
      mapping = aes_(
        x = ~ x,
        y = ~ y,
        fill = "y",
        color = "y"
      ),
      size = size * 1.5,
      shape = 21,
      stroke = 0.75
    ) +
    scale_fill_ppc(lgnd_title, labels = c(Ty_label(), Tyrep_label())) +
    scale_color_ppc(lgnd_title, labels = c(Ty_label(), Tyrep_label())) +
    labs(x = stat_labs[1], y = stat_labs[2]) +
    bayesplot_theme_get()
}


#' @rdname PPC-test-statistics
#' @export
ppc_stat_data <- function(y, yrep, group = NULL, stat) {
  if (!(length(stat) %in% 1:2)) {
    abort("'stat' must have length 1 or 2.")
  }

  y <- validate_y(y)
  yrep <- validate_predictions(yrep, length(y))
  if (!is.null(group)) {
    group <- validate_group(group, length(y))
  }

  if (length(stat) == 1) {
    stat <- match.fun(stat)
  } else {
    stat <- list(match.fun(stat[[1]]), match.fun(stat[[2]]))
  }

  .ppd_stat_data(
    predictions = yrep,
    y = y,
    group = group,
    stat = stat
  )
}


# internal ----------------------------------------------------------------

#' Make legend title for ppc_stat,ppc_stat_grouped,ppc_stat_freqpoly_grouped
#'
#' @param stat The user's `stat` argument.
#' @param stat_txt `deparse(substitute())` applied to users `stat` argument.
#' @return Either throws an error or returns a legend title (possibly `NULL`).
#' @noRd
stat_legend_title <- function(stat, stat_txt) {
  stopifnot(is.character(stat) || is.function(stat))
  if (is.character(stat)) {
    lgnd_txt <- stat
  } else {
    lgnd_txt <- if (length(stat_txt) == 1 && !grepl("^function", stat_txt))
      stat_txt else NA
  }
  if (is.na(lgnd_txt))
    return(NULL)

  bquote(italic(T) == .(lgnd_txt))
}


#' Make data frame for geom_segment() for ppc_stat_2d()
#' @param data Data frame from `ppc_stat_data()`.
#' @return Data frame with two rows and four columns (`x`,`xend`,`y`,`yend`).
#' @noRd
stat_2d_segment_data <- function(data) {
  y_data <- dplyr::filter(data, .data$variable == "y")
  stats <- c(y_data$value[1], y_data$value2[1])
  data.frame(
    x = c(stats[1], -Inf),
    xend = c(stats[1], stats[1]),
    y = c(-Inf, stats[2]),
    yend = c(stats[2], stats[2])
  )
}


Ty_label <- function() expression(italic(T(italic(y))))
Tyrep_label <- function() expression(italic(T)(italic(y)[rep]))

