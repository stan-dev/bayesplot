#' PPC test statistics
#'
#' The distribution of a (test) statistic `T(yrep)`, or a pair of (test)
#' statistics, over the simulated datasets in `yrep`, compared to the
#' observed value `T(y)` computed from the data `y`. See the
#' **Plot Descriptions** and **Details** sections, below, as
#' well as [Gabry et al. (2019)](https://github.com/jgabry/bayes-vis-paper#readme).
#'
#' @name PPC-test-statistics
#' @family PPCs
#'
#' @template args-y-yrep
#' @template args-facet_args
#' @param stat A single function or a string naming a function, except for
#'   `ppc_stat_2d()` which requires a vector of exactly two functions or
#'   function names. In all cases the function(s) should take a vector input and
#'   return a scalar statistic. If specified as a string (or strings) then
#'   the legend will display function names. If specified as a function (or
#'   functions) then generic naming is used in the legend.
#' @param ... Currently unused.
#'
#' @template details-binomial
#' @template return-ggplot
#'
#' @template reference-vis-paper
#' @templateVar bdaRef (Ch. 6)
#' @template reference-bda
#'
#' @section Plot Descriptions:
#' \describe{
#'   \item{`ppc_stat()`}{
#'    A histogram of the distribution of a test statistic computed by applying
#'    `stat` to each dataset (row) in `yrep`. The value of the statistic in the
#'    observed data, `stat(y)`, is overlaid as a vertical line. More details on
#'    `ppc_stat()` can be found in Gabry et al. (2019).
#'   }
#'   \item{`ppc_stat_grouped()`,`ppc_stat_freqpoly_grouped()`}{
#'    The same as `ppc_stat()`, but a separate plot is generated for each
#'    level of a grouping variable. In the case of
#'    `ppc_stat_freqpoly_grouped()` the plots are frequency polygons rather
#'    than histograms. More details on `ppc_stat_grouped()` can be found in
#'    Gabry et al. (2019).
#'   }
#'   \item{`ppc_stat_2d()`}{
#'    A scatterplot showing the joint distribution of two test statistics
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
#' ppc_stat_2d(y, yrep)
#' ppc_stat_2d(y, yrep, stat = c("median", "mean")) + legend_move("bottom")
#'
#' color_scheme_set("teal")
#' group <- example_group_data()
#' ppc_stat_grouped(y, yrep, group)
#'
#' color_scheme_set("mix-red-blue")
#' ppc_stat_freqpoly_grouped(y, yrep, group, facet_args = list(nrow = 2))
#'
#' # use your own function to compute test statistics
#' color_scheme_set("brightblue")
#' q25 <- function(y) quantile(y, 0.25)
#' ppc_stat(y, yrep, stat = "q25") # legend includes function name
#'
#' # can define the function in the 'stat' argument but then
#' # the legend doesn't include a function name
#' ppc_stat(y, yrep, stat = function(y) quantile(y, 0.25))
#'
NULL

#' @rdname PPC-test-statistics
#' @export
#' @template args-hist
#' @template args-hist-freq
#'
ppc_stat <-
  function(y,
           yrep,
           stat = "mean",
           ...,
           binwidth = NULL,
           breaks = NULL,
           freq = TRUE) {
    check_ignored_arguments(...)

    y <- validate_y(y)
    yrep <- validate_yrep(yrep, y)
    stat1 <- match.fun(stat)
    T_y <- stat1(y)
    T_yrep <- apply(yrep, 1, stat1)

    ggplot(data.frame(value = T_yrep),
           set_hist_aes(freq)) +
      geom_histogram(
        aes_(fill = "yrep"),
        color = get_color("lh"),
        size = .25,
        na.rm = TRUE,
        binwidth = binwidth,
        breaks = breaks
      ) +
      geom_vline(
        data = data.frame(Ty = T_y),
        mapping = aes_(xintercept = ~ Ty, color = "y"),
        size = 1.5
      ) +
      scale_fill_manual(values = get_color("l"), labels = Tyrep_label()) +
      scale_color_manual(values = get_color("dh"), labels = Ty_label()) +
      guides(
        color = guide_legend(title = NULL),
        fill = guide_legend(
          order = 1,
          title = stat_legend_title(stat, deparse(substitute(stat)))
        )
      ) +
      bayesplot_theme_get() +
      dont_expand_y_axis() +
      no_legend_spacing() +
      xaxis_title(FALSE) +
      yaxis_text(FALSE) +
      yaxis_ticks(FALSE) +
      yaxis_title(FALSE)
  }

#' @export
#' @rdname PPC-test-statistics
#' @template args-group
#'
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

    y <- validate_y(y)
    yrep <- validate_yrep(yrep, y)
    group <- validate_group(group, y)
    plot_data <- ppc_group_data(y, yrep, group, stat = match.fun(stat))
    is_y <- plot_data$variable == "y"

    facet_args[["facets"]] <- ~ group
    if (is.null(facet_args[["scales"]]))
      facet_args[["scales"]] <- "free"

    ggplot(plot_data[!is_y, , drop = FALSE],
           set_hist_aes(freq)) +
      geom_histogram(
        aes_(fill = "yrep"),
        color = get_color("lh"),
        size = .25,
        na.rm = TRUE,
        binwidth = binwidth,
        breaks = breaks
      ) +
      geom_vline(
        data = plot_data[is_y, , drop = FALSE],
        mapping = aes_(xintercept = ~ value, color = "y"),
        size = 1.5
      ) +
      do.call("facet_wrap", facet_args) +
      scale_fill_manual(values = get_color("l"), labels = Tyrep_label()) +
      scale_color_manual(values = get_color("dh"), labels = Ty_label()) +
      guides(
        color = guide_legend(title = NULL),
        fill = guide_legend(
          order = 1,
          title = stat_legend_title(stat, deparse(substitute(stat)))
        )
      ) +
      bayesplot_theme_get() +
      dont_expand_y_axis() +
      no_legend_spacing() +
      xaxis_title(FALSE) +
      yaxis_text(FALSE) +
      yaxis_ticks(FALSE) +
      yaxis_title(FALSE)
  }


#' @export
#' @rdname PPC-test-statistics
#'
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

    y <- validate_y(y)
    yrep <- validate_yrep(yrep, y)
    group <- validate_group(group, y)
    plot_data <- ppc_group_data(y, yrep, group, stat = match.fun(stat))
    is_y <- plot_data$variable == "y"

    facet_args[["facets"]] <- ~ group
    if (is.null(facet_args[["scales"]]))
      facet_args[["scales"]] <- "free"

    ggplot(plot_data[!is_y, , drop = FALSE],
           set_hist_aes(freq)) +
      geom_freqpoly(
        aes_(color = "yrep"),
        size = .5,
        na.rm = TRUE,
        binwidth = binwidth
      ) +
      geom_vline(
        data = plot_data[is_y, , drop = FALSE],
        mapping = aes_(xintercept = ~ value, color = "y"),
        show.legend = FALSE,
        size = 1
      ) +
      do.call("facet_wrap", facet_args) +
      scale_color_manual(
        name = stat_legend_title(stat, deparse(substitute(stat))),
        values = set_names(get_color(c("m", "dh")), c("yrep", "y")),
        labels = c(yrep = Tyrep_label(), y = Ty_label())
      ) +
      dont_expand_y_axis(c(0.005, 0)) +
      xaxis_title(FALSE) +
      yaxis_text(FALSE) +
      yaxis_ticks(FALSE) +
      yaxis_title(FALSE) +
      bayesplot_theme_get()
  }


#' @rdname PPC-test-statistics
#' @export
#' @param size,alpha Arguments passed to [ggplot2::geom_point()] to control the
#'   appearance of scatterplot points.
ppc_stat_2d <- function(y,
                        yrep,
                        stat = c("mean", "sd"),
                        ...,
                        size = 2.5,
                        alpha = 0.7) {

  check_ignored_arguments(...)

  if(class(y) != "list") y <- validate_y(y)
  if(class(y) != "list") yrep <- validate_yrep(yrep, y)
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

  stat1 <- match.fun(stat[[1]])
  stat2 <- match.fun(stat[[2]])
  if(class(y) != "list") T_y1 <- stat1(y)
  if(class(y) != "list") T_y2 <- stat2(y)
  if(class(y) == "list") T_y1 <- unlist(lapply(y, stat1))
  if(class(y) == "list") T_y2 <- unlist(lapply(y, stat2))
  T_yrep1 <- apply(yrep, 1, stat1)
  T_yrep2 <- apply(yrep, 1, stat2)

p1 <-  ggplot(
    data = data.frame(x = T_yrep1, y = T_yrep2),
    mapping = aes_(x = ~ x, y = ~ y)
  ) +
    geom_point(
      aes_(fill = "yrep", color = "yrep"),
      shape = 21,
      size = size,
      alpha = alpha
    )  +
    geom_point(
      data = data.frame(x = T_y1, y = T_y2),
      mapping = aes_(x = ~ x, y = ~ y, fill = "y", color = "y"),
      size = size * 1.5,
      shape = 21,
      stroke = 0.75
    ) +
    scale_fill_manual(
      name = lgnd_title,
      values = set_names(get_color(c("d", "l")), c("y", "yrep")),
      labels = c(y = Ty_label(), yrep = Tyrep_label())
    ) +
    scale_color_manual(
      name = lgnd_title,
      values = set_names(get_color(c("dh", "lh")), c("y", "yrep")),
      labels = c(y = Ty_label(), yrep = Tyrep_label())
    ) +
    labs(x = stat_labs[1], y = stat_labs[2]) +
    bayesplot_theme_get()

if(class(y) != "list") p1 <- p1 +
               annotate(
                 geom = "segment",
                 x = c(T_y1, -Inf), xend = c(T_y1, T_y1),
                 y = c(-Inf, T_y2), yend = c(T_y2, T_y2),
                 linetype = 2,
                 size = 0.4,
                 color = get_color("dh")
               )

print(p1)


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

