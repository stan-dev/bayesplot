#' Diagnostic plots for the No-U-Turn-Sampler (NUTS)
#'
#' Diagnostic plots for the No-U-Turn-Sampler (NUTS), the default MCMC algorithm
#' used by [Stan](https://mc-stan.org). See the **Plot Descriptions** section,
#' below.
#'
#' @name MCMC-nuts
#' @aliases NUTS
#' @family MCMC
#'
#' @param x A molten data frame of NUTS sampler parameters, either created by
#'   [nuts_params()] or in the same form as the object returned by
#'   [nuts_params()].
#' @param lp A molten data frame of draws of the log-posterior or, more
#'   commonly, of a quantity equal to the log-posterior up to a constant.
#'   `lp` should either be created via [log_posterior()] or be an
#'   object with the same form as the object returned by
#'   [log_posterior()].
#' @param chain A positive integer for selecting a particular chain. The default
#'   (`NULL`) is to merge the chains before plotting. If `chain = k`
#'   then the plot for chain `k` is overlaid (in a darker shade but with
#'   transparency) on top of the plot for all chains. The `chain` argument
#'   is not used by `mcmc_nuts_energy()`.
#' @param ... Currently ignored.
#'
#' @return A gtable object (the result of calling
#'   [gridExtra::arrangeGrob()]) created from several ggplot objects,
#'   except for `mcmc_nuts_energy()`, which returns a ggplot object.
#'
#' @section Quick Definitions:
#' For more details see Stan Development Team (2016) and Betancourt (2017).
#' * `accept_stat__`: the average acceptance probabilities of all
#'   possible samples in the proposed tree.
#' * `divergent__`: the number of leapfrog transitions with diverging
#'   error. Because NUTS terminates at the first divergence this will be either
#'   0 or 1 for each iteration.
#' * `stepsize__`: the step size used by NUTS in its Hamiltonian
#'   simulation.
#' * `treedepth__`: the depth of tree used by NUTS, which is the log
#'   (base 2) of the number of leapfrog steps taken during the Hamiltonian
#'   simulation.
#' * `energy__`: the value of the Hamiltonian (up to an additive
#'   constant) at each iteration.
#'
#' @section Plot Descriptions:
#' \describe{
#'   \item{`mcmc_nuts_acceptance()`}{
#'   Three plots:
#'   * Histogram of `accept_stat__` with vertical lines indicating the
#'     mean (solid line) and median (dashed line).
#'   * Histogram of `lp__` with vertical
#'     lines indicating the mean (solid line) and median (dashed line).
#'   * Scatterplot of `accept_stat__` vs `lp__`.
#'   }
#'
#'   \item{`mcmc_nuts_divergence()`}{
#'   Two plots:
#'   * Violin plots of `lp__|divergent__=1` and `lp__|divergent__=0`.
#'   * Violin plots of `accept_stat__|divergent__=1` and
#'     `accept_stat__|divergent__=0`.
#'   }
#'
#'   \item{`mcmc_nuts_stepsize()`}{
#'   Two plots:
#'   * Violin plots of `lp__` by chain ordered by `stepsize__` value.
#'   * Violin plots of `accept_stat__` by chain ordered by `stepsize__` value.
#'   }
#'
#'   \item{`mcmc_nuts_treedepth()`}{
#'   Three plots:
#'   * Violin plots of `lp__` by value of `treedepth__`.
#'   * Violin plots of `accept_stat__` by value of `treedepth__`.
#'   * Histogram of `treedepth__`.
#'   }
#'
#'   \item{`mcmc_nuts_energy()`}{
#'   Overlaid histograms showing `energy__` vs the change in
#'   `energy__`. See Betancourt (2016) for details.
#'   }
#' }
#'
#' @template reference-betancourt
#' @template reference-nuts
#' @template reference-stan-manual
#'
#' @seealso
#' * The [Visual MCMC Diagnostics](https://mc-stan.org/bayesplot/articles/visual-mcmc-diagnostics.html)
#'   vignette.
#' * Several other plotting functions are not NUTS-specific but take optional
#'   extra arguments if the model was fit using NUTS:
#'   * [mcmc_trace()]: show divergences as tick marks below the
#'     trace plot.
#'   * [mcmc_parcoord()]: change the color/size/transparency of lines
#'     corresponding to divergences.
#'   * [mcmc_scatter()]: change the color/size/shape of points
#'     corresponding to divergences.
#'   * [mcmc_pairs()]: change the color/size/shape of points
#'     corresponding divergences and/or max treedepth saturation.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(rstanarm)
#' fit <- stan_glm(mpg ~ wt + am, data = mtcars, iter = 1000, refresh = 0)
#' np <- nuts_params(fit)
#' lp <- log_posterior(fit)
#'
#' color_scheme_set("brightblue")
#' mcmc_nuts_acceptance(np, lp)
#' mcmc_nuts_acceptance(np, lp, chain = 2)
#'
#' mcmc_nuts_divergence(np, lp)
#' mcmc_nuts_stepsize(np, lp)
#' mcmc_nuts_treedepth(np, lp)
#'
#' color_scheme_set("red")
#' mcmc_nuts_energy(np)
#' mcmc_nuts_energy(np, merge_chains = TRUE, binwidth = .15)
#' mcmc_nuts_energy(np) +
#'  facet_wrap(vars(Chain), nrow = 1) +
#'  coord_fixed(ratio = 150) +
#'  ggtitle("NUTS Energy Diagnostic")
#' }
#'
NULL


#' @rdname MCMC-nuts
#' @export
#' @template args-hist
#'
mcmc_nuts_acceptance <-
  function(x,
           lp,
           chain = NULL,
           ...,
           binwidth = NULL,
           bins = NULL,
           breaks = NULL) {
    suggested_package("gridExtra")
    check_ignored_arguments(...)

    x <- validate_nuts_data_frame(x, lp)
    n_chain <- length(unique(lp$Chain))
    chain <- validate_enough_chains(chain, num_chains(x))
    overlay_chain <- !is.null(chain)

    accept_stat <- dplyr::filter(x, .data$Parameter == "accept_stat__")
    data <- suppressWarnings(
      dplyr::bind_rows(accept_stat, data.frame(lp, Parameter = "lp__"))
    )

    grp_par <- group_by(data, .data$Parameter)
    stats_par <- summarise(grp_par,
                           Mean = mean(.data$Value),
                           Median = median(.data$Value))

    hists <- ggplot(data, aes(x = .data$Value, y = after_stat(density))) +
      geom_histogram(
        fill = get_color("l"),
        color = get_color("lh"),
        linewidth = 0.25,
        na.rm = TRUE,
        binwidth = binwidth,
        bins = bins,
        breaks = breaks
      ) +
      bayesplot_theme_get()

    if (!overlay_chain) {
      hists <- hists +
        geom_vline(
          aes(xintercept = .data$Mean),
          data = stats_par,
          color = get_color("dh")
        ) +
        geom_vline(
          aes(xintercept = .data$Median),
          data = stats_par,
          color = get_color("d"),
          linetype = 2
        )
    }
    hists <- hists +
      dont_expand_y_axis(c(0.005, 0)) +
      facet_wrap(vars(.data$Parameter), scales = "free") +
      yaxis_text(FALSE) +
      yaxis_title(FALSE) +
      yaxis_ticks(FALSE) +
      xaxis_title(FALSE)

    scatter_data <- data.frame(
      x = accept_stat$Value,
      y = lp$Value
    )
    scatter <- ggplot(scatter_data) +
      geom_point(
        aes(x = .data$x, y = .data$y),
        alpha = 0.75,
        shape = 21,
        fill = get_color(ifelse(overlay_chain, "l", "m")),
        color = get_color(ifelse(overlay_chain, "lh", "mh"))
      ) +
      labs(x = "accept_stat__", y = "lp__") +
      bayesplot_theme_get()

    if (overlay_chain) {
      hists <- hists +
        geom_histogram(
          data = dplyr::filter(data, .data$Chain == chain),
          fill = get_color("d"),
          color = NA,
          alpha = 0.5,
          na.rm = TRUE,
          binwidth = binwidth,
          bins = bins,
          breaks = breaks
        )

      chain_scatter_data <- data.frame(
        x = accept_stat$Value[accept_stat$Chain == chain],
        y = lp$Value[lp$Chain == chain]
      )
      scatter <- scatter +
        geom_point(
          aes(x = .data$x, y = .data$y),
          color = get_color("d"),
          alpha = 0.5,
          data = chain_scatter_data
        )
    }
    nuts_plot <- gridExtra::arrangeGrob(hists, scatter, nrow = 2)
    as_bayesplot_grid(nuts_plot)
  }


#' @rdname MCMC-nuts
#' @export
mcmc_nuts_divergence <- function(x, lp, chain = NULL, ...) {
  suggested_package("gridExtra")
  check_ignored_arguments(...)

  x <- validate_nuts_data_frame(x, lp)
  chain <- validate_enough_chains(chain, num_chains(x))
  overlay_chain <- !is.null(chain)

  accept_stat <- dplyr::filter(x, .data$Parameter == "accept_stat__")
  divergent <- dplyr::filter(x, .data$Parameter == "divergent__")
  divergent$Value <- factor(divergent$Value, levels = c(0, 1),
                            labels = c("No divergence", "Divergence"))

  violin_lp_data <- data.frame(divergent, lp = lp$Value)
  violin_lp <- ggplot(violin_lp_data, aes(x = .data$Value, y = .data$lp)) +
    geom_violin(fill = get_color("l"), color = get_color("lh")) +
    ylab("lp__") +
    xaxis_title(FALSE) +
    bayesplot_theme_get()

  violin_accept_stat_data <- data.frame(divergent, as = accept_stat$Value)
  violin_accept_stat <- ggplot(violin_accept_stat_data, aes(x = .data$Value, y = .data$as)) +
    geom_violin(fill = get_color("l"), color = get_color("lh")) +
    ylab("accept_stat__") +
    scale_y_continuous(limits = c(NA, 1.05)) +
    xaxis_title(FALSE) +
    bayesplot_theme_get()

  div_count <- table(divergent$Value)[[2]]
  div_text <- ngettext(div_count, "divergence", "divergences")
  div_count_label <- paste(div_count, div_text)

  if (!is.null(chain)) {
    violin_lp <- violin_lp +
      chain_violin(violin_lp_data, chain)
    violin_accept_stat <- violin_accept_stat +
      chain_violin(violin_accept_stat_data, chain)

    div_count_by_chain <-
      table(divergent$Value, divergent$Chain)["Divergence", chain]
    div_count_label <-
      paste0(div_count_label, " (", div_count_by_chain,
             " from chain ", chain, ")")
  }
  violin_lp <- violin_lp + labs(subtitle = div_count_label)
  nuts_plot <- gridExtra::arrangeGrob(violin_lp, violin_accept_stat, nrow = 2)
  as_bayesplot_grid(nuts_plot)
}


#' @rdname MCMC-nuts
#' @export
mcmc_nuts_stepsize <- function(x, lp, chain = NULL, ...) {
  suggested_package("gridExtra")
  check_ignored_arguments(...)

  x <- validate_nuts_data_frame(x, lp)
  chain <- validate_enough_chains(chain, num_chains(x))
  overlay_chain <- !is.null(chain)

  stepsize <- dplyr::filter(x, .data$Parameter == "stepsize__")
  accept_stat <- dplyr::filter(x, .data$Parameter == "accept_stat__")

  stepsize_by_chain <- stepsize %>%
    group_by(.data$Chain) %>%
    summarise(ss = dplyr::first(.data$Value))

  stepsize_labels_text <- stepsize_by_chain %>%
    arrange(.data$ss) %>%
    mutate(value = format(round(.data$ss, 3), digits = 3),
           label = paste0(.data$value, "\n(chain ", .data$Chain, ")")) %>%
    pull()

  stepsize_labels <- scale_x_discrete(labels = stepsize_labels_text)

  violin_lp_data <- dplyr::left_join(lp, stepsize_by_chain, by = "Chain")
  violin_lp <- ggplot(violin_lp_data, aes(x = as.factor(.data$ss), y = .data$Value)) +
    geom_violin(fill = get_color("l"), color = get_color("lh")) +
    ylab("lp__") +
    stepsize_labels +
    xaxis_title(FALSE) +
    bayesplot_theme_get()

  violin_accept_stat_data <-
    dplyr::left_join(accept_stat, stepsize_by_chain, by = "Chain")
  violin_accept_stat <-
    ggplot(violin_accept_stat_data, aes(x = as.factor(.data$ss), y = .data$Value)) +
    geom_violin(fill = get_color("l"), color = get_color("lh")) +
    ylab("accept_stat__") +
    scale_y_continuous(limits = c(NA, 1.05)) +
    stepsize_labels +
    xaxis_title(FALSE) +
    bayesplot_theme_get()

  if (!is.null(chain)) {
    violin_lp <- violin_lp +
      chain_violin(violin_lp_data, chain)
    violin_accept_stat <- violin_accept_stat +
      chain_violin(violin_accept_stat_data, chain)
  }
  nuts_plot <- gridExtra::arrangeGrob(violin_lp, violin_accept_stat, nrow = 2)
  as_bayesplot_grid(nuts_plot)
}


#' @rdname MCMC-nuts
#' @export
mcmc_nuts_treedepth <- function(x, lp, chain = NULL, ...) {
  suggested_package("gridExtra")
  check_ignored_arguments(...)

  x <- validate_nuts_data_frame(x, lp)
  chain <- validate_enough_chains(chain, num_chains(x))
  overlay_chain <- !is.null(chain)

  treedepth <- dplyr::filter(x, .data$Parameter == "treedepth__")
  accept_stat <- dplyr::filter(x, .data$Parameter == "accept_stat__")

  hist_td <- ggplot(treedepth, aes(x = .data$Value, y = after_stat(density))) +
    geom_histogram(
      fill = get_color("l"),
      color = get_color("lh"),
      linewidth = 0.2,
      na.rm = TRUE,
      binwidth = 1
    ) +
    xlab("treedepth__")  +
    bayesplot_theme_get() +
    yaxis_text(FALSE) +
    yaxis_title(FALSE) +
    yaxis_ticks(FALSE)

  violin_lp_data <- data.frame(treedepth, lp = lp$Value)
  violin_lp <-
    ggplot(violin_lp_data, aes(x = factor(.data$Value), y = .data$lp)) +
    geom_violin(fill = get_color("l"), color = get_color("lh")) +
    labs(x = "treedepth__", y = "lp__") +
    bayesplot_theme_get()

  violin_accept_stat_data <- data.frame(treedepth, as = accept_stat$Value)
  violin_accept_stat <-
    ggplot(violin_accept_stat_data, aes(x = factor(.data$Value), y = .data$as)) +
    geom_violin(fill = get_color("l"), color = get_color("lh")) +
    labs(x = "treedepth__", y = "accept_stat__") +
    scale_y_continuous(breaks = c(0, 0.5, 1)) +
    bayesplot_theme_get()

  if (overlay_chain) {
    hist_td <- hist_td +
      geom_histogram(
        data = dplyr::filter(treedepth, .data$Chain == chain),
        fill = get_color("d"),
        color = NA,
        alpha = 0.5,
        na.rm = TRUE,
        binwidth = 1
      ) +
      dont_expand_y_axis()

    violin_lp <- violin_lp +
      chain_violin(violin_lp_data, chain)
    violin_accept_stat <- violin_accept_stat +
      chain_violin(violin_accept_stat_data, chain)
  }

  nuts_plot <- gridExtra::grid.arrange(
    gridExtra::arrangeGrob(violin_lp, violin_accept_stat, nrow = 1),
    hist_td,
    nrow = 2
  )
  as_bayesplot_grid(nuts_plot)
}


#' @rdname MCMC-nuts
#' @export
#' @param alpha For `mcmc_nuts_energy()` only, the transparency (alpha) level
#'   in `[0,1]` used for the overlaid histogram.
#' @param merge_chains For `mcmc_nuts_energy()` only, should all chains be
#'   merged or displayed separately? The default is `FALSE`, i.e., to show
#'   the chains separately.
#'
mcmc_nuts_energy <-
  function(x,
           ...,
           binwidth = NULL,
           bins = NULL,
           breaks = NULL,
           alpha = 0.5,
           merge_chains = FALSE) {
    check_ignored_arguments(...)

    x <- validate_nuts_data_frame(x)
    energy <- dplyr::filter(x, .data$Parameter == "energy__")

    # lag() (stats::lag()) here doesn't work, but dplyr::lag() does
    data <- energy %>%
      group_by(.data$Chain) %>%
      mutate(
        Ediff = .data$Value - dplyr::lag(.data$Value),
        E_centered = .data$Value - mean(.data$Value),
        Ediff_centered = .data$Ediff - mean(.data$Ediff, na.rm = TRUE)
      )

    fills <- set_names(get_color(c("l", "m")), c("E_fill", "Ediff_fill"))
    clrs <- set_names(get_color(c("lh", "mh")), c("E_fill", "Ediff_fill"))
    aes_labs <- c(expression(pi[E]), expression(pi[paste(Delta, E)]))

    graph <- ggplot(data, aes(y = after_stat(density))) +
      geom_histogram(
        aes(
          x = .data$Ediff_centered,
          fill = "Ediff_fill",
          color = "Ediff_fill"
        ),
        linewidth = 0.25,
        na.rm = TRUE,
        binwidth = binwidth,
        bins = bins,
        breaks = breaks
      ) +
      geom_histogram(
        aes(
          x = .data$E_centered,
          fill = "E_fill",
          color = "E_fill"
        ),
        linewidth = 0.25,
        na.rm = TRUE,
        alpha = alpha,
        binwidth = binwidth,
        bins = bins,
        breaks = breaks
      ) +
      scale_fill_manual("", values = fills, labels = aes_labs) +
      scale_color_manual("", values = clrs, labels = aes_labs) +
      dont_expand_y_axis(c(0.005, 0)) +
      scale_x_continuous(expand = c(0.2, 0)) +
      labs(y = NULL, x = expression(E - bar(E))) +
      bayesplot_theme_get() +
      space_legend_keys()  +
      theme(legend.text = element_text(size = rel(1.1))) +
      yaxis_text(FALSE) +
      yaxis_title(FALSE) +
      yaxis_ticks(FALSE)

    if (merge_chains) {
      return(graph)
    }

    graph +
      facet_wrap(vars(.data$Chain)) +
      force_axes_in_facets()
  }


# internal ----------------------------------------------------------------
validate_enough_chains <- function(chain = NULL, n_chain) {
  if (!is.null(chain)) {
    stopifnot(chain >= 1)
    if (!isTRUE(n_chain >= chain)) {
      abort(paste("'chain' is", chain, "but only", n_chain, "chains found."))
    }
  }
  chain
}

#' @param x data frame with nuts params
#' @param lp data frame with `lp__`
#' @noRd
validate_nuts_data_frame <- function(x, lp) {
  if (!is.data.frame(x)) {
    abort("NUTS parameters should be in a data frame.")
  }

  valid_cols <- sort(c("Iteration", "Parameter", "Value", "Chain"))
  if (!identical(sort(colnames(x)), valid_cols)) {
    abort(paste(
      "NUTS parameter data frame must have columns:",
      paste(valid_cols, collapse = ", ")
    ))
  }

  if (missing(lp)) {
    lp <- NULL
  }
  if (!is.null(lp)) {
    if (!is.data.frame(lp)) {
      abort("lp should be in a data frame.")
    }

    valid_lp_cols <- sort(c("Iteration", "Value", "Chain"))
    if (!identical(sort(colnames(lp)), valid_lp_cols)) {
      abort(paste(
        "lp data frame must have columns:",
        paste(valid_lp_cols, collapse = ", ")
      ))
    }

    n_chain <- num_chains(x)
    n_lp_chain <- num_chains(lp)
    if (n_chain != n_lp_chain) {
      abort(paste(
        "Number of chains for NUTS parameters is", n_chain,
        "but number of chains for lp is", n_lp_chain
      ))
    }
  }

  x
}

chain_violin <-
  function(df,
           chain,
           fill = "d",
           color = NA,
           alpha = 0.5) {
    geom_violin(
      data = dplyr::filter(df, .data$Chain == chain),
      fill = get_color(fill),
      color = color,
      alpha = alpha
    )
  }
