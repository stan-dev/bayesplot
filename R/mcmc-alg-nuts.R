#' Diagnostic plots for the No-U-Turn-Sampler (NUTS)
#'
#' Diagnostic plots for the No-U-Turn-Sampler (NUTS), the default MCMC algorithm
#' used by Stan. See the \strong{Plot Descriptions} section, below.
#'
#' @name MCMC-nuts
#' @aliases NUTS
#' @family MCMC
#'
#' @param x A molten data frame of NUTS sampler parameters, either created by
#'   \code{\link{nuts_params}} or in the same form as the object returned by
#'   \code{\link{nuts_params}}.
#' @param lp A molten data frame of draws of the log-posterior or, more
#'   commonly, of a quantity equal to the log-posterior up to a constant.
#'   \code{lp} should either be created via \code{\link{log_posterior}} or be an
#'   object with the same form as the object returned by
#'   \code{\link{log_posterior}}.
#' @param chain A positive integer for selecting a particular chain. The default
#'   (\code{NULL}) is to merge the chains before plotting. If \code{chain = k}
#'   then the plot for chain \code{k} is overlaid (in a darker shade but with
#'   transparency) on top of the plot for all chains. The \code{chain} argument
#'   is not used by \code{mcmc_nuts_energy}.
#' @param ... Currently ignored.
#'
#' @return A gtable object (the result of calling
#'   \code{\link[gridExtra]{arrangeGrob}}) created from several ggplot objects,
#'   except for \code{mcmc_nuts_energy}, which returns a ggplot object.
#'
#' @section Quick Definitions:
#' For more details see Stan Development Team (2016) and Betancourt (2017).
#' \itemize{
#'   \item \code{accept_stat__}: the average acceptance probabilities of all
#'   possible samples in the proposed tree.
#'   \item \code{divergent__}: the number of leapfrog transitions with diverging
#'   error. Because NUTS terminates at the first divergence this will be either
#'   0 or 1 for each iteration.
#'   \item \code{stepsize__}: the step size used by NUTS in its Hamiltonian
#'   simulation.
#'   \item \code{treedepth__}: the depth of tree used by NUTS, which is the log
#'   (base 2) of the number of leapfrog steps taken during the Hamiltonian
#'   simulation.
#'   \item \code{energy__}: the value of the Hamiltonian (up to an additive
#'   constant) at each iteration.
#' }
#'
#' @section Plot Descriptions:
#' \describe{
#'   \item{\code{mcmc_nuts_acceptance}}{
#'   Three plots:
#'   \itemize{
#'    \item Histogram of \code{accept_stat__} with vertical lines indicating the
#'    mean (solid line) and median (dashed line).
#'    \item Histogram of \code{lp__} with vertical
#'    lines indicating the mean (solid line) and median (dashed line).
#'    \item Scatterplot of \code{accept_stat__} vs \code{lp__}.
#'    }
#'   }
#'   \item{\code{mcmc_nuts_divergence}}{
#'   Two plots:
#'   \itemize{
#'    \item Violin plots of \code{lp__|divergent__=1} and
#'      \code{lp__|divergent__=0}.
#'    \item Violin plots of \code{accept_stat__|divergent__=1} and
#'      \code{accept_stat__|divergent__=0}.
#'    }
#'   }
#'   \item{\code{mcmc_nuts_stepsize}}{
#'   Two plots:
#'   \itemize{
#'    \item Violin plots of \code{lp__} by chain ordered by
#'    \code{stepsize__} value.
#'    \item Violin plots of \code{accept_stat__} by chain ordered by
#'    \code{stepsize__} value.
#'    }
#'   }
#'   \item{\code{mcmc_nuts_treedepth}}{
#'   Three plots:
#'   \itemize{
#'    \item Violin plots of \code{lp__} by value of \code{treedepth__}.
#'    \item Violin plots of \code{accept_stat__} by value of \code{treedepth__}.
#'    \item Histogram of \code{treedepth__}.
#'    }
#'   }
#'   \item{\code{mcmc_nuts_energy}}{
#'   Overlaid histograms showing \code{energy__} vs the change in
#'   \code{energy__}. See Betancourt (2016) for details.
#'   }
#' }
#'
#' @template reference-betancourt
#' @template reference-nuts
#' @template reference-stan-manual
#'
#' @seealso
#' \itemize{
#' \item The \emph{Visual MCMC Diagnostics} vignette.
#' \item Several other plotting functions in the \pkg{bayesplot}
#' package that aren't NUTS-specific but take optional extra arguments
#' if the model was fit using NUTS:
#' \itemize{
#'  \item \code{\link{mcmc_trace}} will plot divergences on the traceplot if the
#'  optional \code{divergences} argument is specified.
#'  \item \code{\link{mcmc_pairs}} will indicate which (if any) iterations
#'  encountered a divergent transition or hit the maximum treedepth (rather than
#'  terminated its evolution normally).
#'  }
#' }
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(rstanarm)
#' fit <- stan_glm(mpg ~ wt + am, data = mtcars, iter = 1000)
#' np <- nuts_params(fit)
#' lp <- log_posterior(fit)
#'
#' color_scheme_set("brightblue")
#' mcmc_nuts_acceptance(np, lp)
#' mcmc_nuts_acceptance(np, lp, chain = 2)
#'
#' color_scheme_set("red")
#' mcmc_nuts_energy(np)
#' mcmc_nuts_energy(np, merge_chains = TRUE, binwidth = .15)
#' mcmc_nuts_energy(np) +
#'  facet_wrap(~ Chain, nrow = 1) +
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
           binwidth = NULL) {
    suggested_package("gridExtra")
    check_ignored_arguments(...)

    x <- validate_nuts_data_frame(x, lp)
    n_chain <- length(unique(lp$Chain))
    chain <- validate_enough_chains(chain, n_chain)
    overlay_chain <- !is.null(chain)

    accept_stat <- filter_(x, ~ Parameter == "accept_stat__")
    data <- suppressWarnings(
      dplyr::bind_rows(accept_stat, data.frame(lp, Parameter = "lp__"))
    )

    grp_par <- group_by_(data, ~ Parameter)
    stats_par <- summarise_(grp_par,
                            Mean = ~ mean(Value),
                            Median = ~ median(Value))

    hists <- ggplot(data, aes_(x = ~ Value, y = ~ ..density..)) +
      geom_histogram(
        fill = get_color("l"),
        color = get_color("lh"),
        size = .25,
        na.rm = TRUE,
        binwidth = binwidth
      )

    if (!overlay_chain) {
      hists <- hists +
        geom_vline(
          aes_(xintercept = ~ Mean),
          data = stats_par,
          color = get_color("dh")
        ) +
        geom_vline(
          aes_(xintercept = ~ Median),
          data = stats_par,
          color = get_color("d"),
          linetype = 2
        )
    }
    hists <- hists +
      dont_expand_y_axis(c(0.005, 0)) +
      facet_wrap(~ Parameter, scales = "free") +
      yaxis_text(FALSE) +
      yaxis_title(FALSE) +
      yaxis_ticks(FALSE) +
      xaxis_title(FALSE)

    scatter <- ggplot(NULL) +
      geom_point(
        aes_(x = ~ accept_stat$Value, y = ~ lp$Value),
        alpha = 0.75,
        shape = 21,
        fill = get_color(ifelse(overlay_chain, "l", "m")),
        color = get_color(ifelse(overlay_chain, "lh", "mh"))
      ) +
      labs(x = "accept_stat__", y = "lp__")

    if (overlay_chain) {
      hists <- hists +
        geom_histogram(
          data = filter_(data, ~ Chain == chain),
          fill = get_color("d"),
          color = NA,
          alpha = 0.5,
          na.rm = TRUE,
          binwidth = binwidth
        )

      scatter <- scatter +
        geom_point(
          aes_(x = ~ accept_stat$Value[accept_stat$Chain == chain],
               y = ~ lp$Value[lp$Chain == chain]),
          color = get_color("d"),
          alpha = 0.5
        )
    }

    nuts_plot <- gridExtra::arrangeGrob(
      hists,
      gridExtra::arrangeGrob(empty_grob()),
      gridExtra::arrangeGrob(
        empty_grob(),
        scatter,
        empty_grob(),
        ncol = 3,
        widths = c(1, 3, 1)
      ),
      nrow = 3,
      heights = c(1, 0.1, 1)
    )
    as_bayesplot_grid(nuts_plot)
  }


#' @rdname MCMC-nuts
#' @export
mcmc_nuts_divergence <- function(x, lp, chain = NULL, ...) {
  suggested_package("gridExtra")
  check_ignored_arguments(...)

  x <- validate_nuts_data_frame(x, lp)
  n_chain <- length(unique(lp$Chain))
  chain <- validate_enough_chains(chain, n_chain)
  overlay_chain <- !is.null(chain)

  accept_stat <- filter_(x, ~ Parameter == "accept_stat__")
  divergent <- filter_(x, ~ Parameter == "divergent__")
  divergent$Value <- factor(divergent$Value, levels = c(0, 1),
                            labels = c("No divergence", "Divergence"))

  violin_lp_data <- data.frame(divergent, lp = lp$Value)
  violin_lp <- ggplot(violin_lp_data, aes_(x = ~ Value, y = ~ lp)) +
    geom_violin(fill = get_color("l"), color = get_color("lh")) +
    ylab("lp__") +
    xaxis_title(FALSE)

  violin_accept_stat_data <- data.frame(divergent, as = accept_stat$Value)
  violin_accept_stat <- ggplot(violin_accept_stat_data, aes_(x = ~ Value, y = ~ as)) +
    geom_violin(fill = get_color("l"), color = get_color("lh")) +
    ylab("accept_stat__") +
    scale_y_continuous(limits = c(NA, 1.05)) +
    xaxis_title(FALSE)

  div_count_label <- paste(table(divergent$Value)[[2]], "divergences")
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
  n_chain <- length(unique(lp$Chain))
  chain <- validate_enough_chains(chain, n_chain)
  overlay_chain <- !is.null(chain)

  stepsize <- filter_(x, ~ Parameter == "stepsize__")
  accept_stat <- filter_(x, ~ Parameter == "accept_stat__")

  stepsize_by_chain <- summarise_(group_by_(stepsize, ~Chain),
                                  ss = ~first(Value))
  stepsize_labels <-
    scale_x_discrete(labels = with(
      dplyr::arrange_(stepsize_by_chain, ~ ss),
      paste0(format(round(ss, 3), digits = 3), "\n(chain ", Chain,  ")")
    ))

  violin_lp_data <- dplyr::left_join(lp, stepsize_by_chain, by = "Chain")
  violin_lp <- ggplot(violin_lp_data, aes_(x = ~as.factor(ss), y = ~Value)) +
    geom_violin(fill = get_color("l"), color = get_color("lh")) +
    ylab("lp__") +
    stepsize_labels +
    xaxis_title(FALSE)

  violin_accept_stat_data <-
    dplyr::left_join(accept_stat, stepsize_by_chain, by = "Chain")
  violin_accept_stat <-
    ggplot(violin_accept_stat_data, aes_(x = ~as.factor(ss), y = ~Value)) +
    geom_violin(fill = get_color("l"), color = get_color("lh")) +
    ylab("accept_stat__") +
    scale_y_continuous(limits = c(NA, 1.05)) +
    stepsize_labels +
    xaxis_title(FALSE)

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
  n_chain <- length(unique(lp$Chain))
  chain <- validate_enough_chains(chain, n_chain)
  overlay_chain <- !is.null(chain)

  treedepth <- filter_(x, ~ Parameter == "treedepth__")
  accept_stat <- filter_(x, ~ Parameter == "accept_stat__")

  hist_td <- ggplot(treedepth, aes_(x = ~ Value, y = ~ ..density..)) +
    geom_histogram(
      fill = get_color("l"),
      color = get_color("lh"),
      size = .2,
      na.rm = TRUE,
      binwidth = 1
    ) +
    xlab("treedepth__") +
    yaxis_text(FALSE) +
    yaxis_title(FALSE) +
    yaxis_ticks(FALSE)

  violin_lp_data <- data.frame(treedepth, lp = lp$Value)
  violin_lp <-
    ggplot(violin_lp_data, aes_(x = ~ factor(Value), y = ~ lp)) +
    geom_violin(fill = get_color("l"), color = get_color("lh")) +
    labs(x = "treedepth__", y = "lp__")

  violin_accept_stat_data <- data.frame(treedepth, as = accept_stat$Value)
  violin_accept_stat <-
    ggplot(violin_accept_stat_data, aes_(x = ~ factor(Value), y = ~ as)) +
    geom_violin(fill = get_color("l"), color = get_color("lh")) +
    labs(x = "treedepth__", y = "accept_stat__") +
    scale_y_continuous(breaks = c(0, 0.5, 1))

  if (overlay_chain) {
    hist_td <- hist_td +
      geom_histogram(
        data = filter_(treedepth, ~Chain == chain),
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

  nuts_plot <- gridExtra::arrangeGrob(
    gridExtra::arrangeGrob(
      violin_lp, violin_accept_stat,
      nrow = 1
    ),
    gridExtra::arrangeGrob(
      empty_grob()
    ),
    gridExtra::arrangeGrob(
      empty_grob(), hist_td, empty_grob(),
      ncol = 3, widths = c(1, 3, 1)
    ),
    nrow = 3, heights = c(1, 0.1, 1)
  )
  as_bayesplot_grid(nuts_plot)
}


#' @rdname MCMC-nuts
#' @export
#' @param alpha For \code{mcmc_nuts_energy} only, the transparency (alpha) level
#'   in [0,1] used for the overlaid histogram.
#' @param merge_chains For \code{mcmc_nuts_energy} only, should all chains be
#'   merged or displayed separately? The default is \code{FALSE}, i.e., to show
#'   the chains separately.
#'
mcmc_nuts_energy <-
  function(x,
           ...,
           binwidth = NULL,
           alpha = 0.5,
           merge_chains = FALSE) {
    check_ignored_arguments(...)

    x <- validate_nuts_data_frame(x)
    energy <- dplyr::filter_(x, ~ Parameter == "energy__")
    dots <- setNames(
      list(
        ~ Value - lag(Value),
        ~ Value - mean(Value),
        ~ Ediff - mean(Ediff, na.rm = TRUE)
      ),
      c("Ediff", "E_centered", "Ediff_centered")
    )
    data <- dplyr::mutate_(dplyr::group_by_(energy, ~ Chain), .dots = dots)

    fills <- setNames(get_color(c("l", "m")), c("E_fill", "Ediff_fill"))
    clrs <- setNames(get_color(c("lh", "mh")), c("E_fill", "Ediff_fill"))
    aes_labs <- c(expression(pi[E]), expression(pi[paste(Delta, E)]))

    graph <- ggplot(data, aes_(y = ~ ..density..)) +
      geom_histogram(
        aes_(
          x = ~ Ediff_centered,
          fill = ~ "Ediff_fill",
          color = ~ "Ediff_fill"
        ),
        size = 0.25,
        na.rm = TRUE,
        binwidth = binwidth
      ) +
      geom_histogram(
        aes_(
          x = ~ E_centered,
          fill = ~ "E_fill",
          color = ~ "E_fill"
        ),
        size = 0.25,
        na.rm = TRUE,
        alpha = alpha,
        binwidth = binwidth
      ) +
      scale_fill_manual("", values = fills, labels = aes_labs) +
      scale_color_manual("", values = clrs, labels = aes_labs) +
      dont_expand_y_axis(c(0.005, 0)) +
      scale_x_continuous(expand = c(0.2, 0)) +
      labs(y = NULL, x = expression(E - bar(E))) +
      space_legend_keys() +
      theme(legend.text = element_text(size = rel(1.1))) +
      yaxis_text(FALSE) +
      yaxis_title(FALSE) +
      yaxis_ticks(FALSE)

    if (merge_chains)
      return(graph)

    graph +
      facet_wrap(~ Chain) +
      force_axes_in_facets()
  }


# internal ----------------------------------------------------------------
validate_enough_chains <- function(chain = NULL, n_chain) {
  if (!is.null(chain)) {
    stopifnot(chain >= 1)
    if (!isTRUE(n_chain >= chain))
      stop("'chain' is ", chain, " but only ", n_chain, " chains found.")
  }
  chain
}

# @param x data frame with nuts params
# @param lp data frame with lp__
validate_nuts_data_frame <- function(x, lp) {
  if (!is.data.frame(x))
    stop("NUTS parameters should be in a data frame.")

  valid_cols <- c("Iteration", "Parameter", "Value", "Chain")
  if (!identical(colnames(x), valid_cols))
    stop(
      "NUTS parameter data frame must have columns: ",
      paste(valid_cols, collapse = ", ")
    )

  if (missing(lp))
    lp <- NULL
  if (!is.null(lp)) {
    if (!is.data.frame(lp))
      stop("lp should be in a data frame.")

    valid_lp_cols <- c("Iteration", "Value", "Chain")
    if (!identical(colnames(lp), valid_lp_cols))
      stop(
        "lp data frame must have columns: ",
        paste(valid_lp_cols, collapse = ", ")
      )

    n_chain <- length(unique(x$Chain))
    n_lp_chain <- length(unique(lp$Chain))
    if (n_chain != n_lp_chain)
      stop(
        "Number of chains for NUTS parameters is ", n_chain,
        " but number of chains for lp is ", n_lp_chain, "."
      )
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
      data = dplyr::filter_(df, ~ Chain == chain),
      fill = get_color(fill),
      color = color,
      alpha = alpha
    )
  }

empty_grob <- function() {
  structure(list(), class = c("grob", "gDesc"))
}
