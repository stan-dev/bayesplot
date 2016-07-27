#' Plots for the No-U-Turn-Sampler (NUTS)
#'
#' @name MCMC-nuts
#' @aliases NUTS
#' @family MCMC
#'
#' @param x A molten data frame of NUTS sampler parameters, either created by
#'   \code{\link{nuts_params}} or in the same form as the object returned by
#'   \code{\link{nuts_params}}.
#' @param lp A molten data frame of log-posterior draws, either created by
#'   \code{\link{log_posterior}} or in the same form as the object returned
#'   by \code{\link{log_posterior}}.
#' @param chain A positive integer for selecting a particular chain. The default
#'   (\code{NULL}) is to merge the chains before plotting. If \code{chain = k}
#'   then the plot for chain \code{k} is overlaid (in a darker shade but with
#'   transparency) on top of the plot for all chains. The \code{chain} argument
#'   is not used by \code{mcmc_nuts_energy}.
#' @param ... Currently ignored.
#'
#' @return A gtable object (the result of calling
#'   \code{\link[gridExtra]{arrangeGrob}}) created from several ggplot objects.
#'
#' @template seealso-color-scheme
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(rstanarm)
#' fit <- stan_glm(mpg ~ wt + am, data = mtcars, iter = 1000)
#' np <- nuts_params(fit)
#' lp <- log_posterior(fit)
#' mcmc_nuts_acceptance(np, lp)
#' mcmc_nuts_acceptance(np, lp, chain = 2)
#'
#' set_color_scheme("blue")
#' mcmc_nuts_energy(np)
#' mcmc_nuts_energy(np, binwidth = .25, alpha = .8)
#' (energy_plot <- mcmc_nuts_energy(np, merge_chains = FALSE))
#' energy_plot +
#'  facet_wrap(~ Chain, nrow = 1) +
#'  coord_fixed(ratio = 150)
#' }
#'
NULL


#' @rdname MCMC-nuts
#' @export
#' @template args-hist
#'
mcmc_nuts_acceptance <- function(x,
                                 lp,
                                 chain = NULL,
                                 ...,
                                 binwidth = NULL) {
  x <- validate_nuts_data_frame(x, lp)
  n_chain <- length(unique(lp$Chain))
  chain <- validate_enough_chains(chain, n_chain)
  overlay_chain <- !is.null(chain)

  accept_stat <- dplyr::filter_(x, ~ Parameter == "accept_stat__")
  data <- suppressWarnings(dplyr::bind_rows(
    accept_stat,
    data.frame(lp, Parameter = "Log-posterior")
    ))

  grp_par <- dplyr::group_by_(data, ~ Parameter)
  stats_par <-
    dplyr::summarise_(grp_par,
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
    theme_default(y_text = FALSE, x_lab = FALSE)

  scatter <- ggplot(NULL) +
    geom_point(
      aes_(x = ~ accept_stat$Value, y = ~ lp$Value),
      shape = 21,
      fill = get_color(ifelse(overlay_chain, "l", "m")),
      color = get_color(ifelse(overlay_chain, "lh", "mh"))
    ) +
    labs(x = "accept_stat__",
         y = "Log-posterior") +
    theme_default()


  if (overlay_chain) {
    hists <- hists +
      geom_histogram(
        data = dplyr::filter_(data, ~Chain == chain),
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

  nuts_plot <- gridExtra::arrangeGrob(scatter, hists, nrow = 2)
  gridExtra::grid.arrange(nuts_plot)
  invisible(nuts_plot)
}


#' @rdname MCMC-nuts
#' @export
mcmc_nuts_divergence <- function(x,
                                lp,
                                chain = NULL,
                                ...) {

  x <- validate_nuts_data_frame(x, lp)
  n_chain <- length(unique(lp$Chain))
  chain <- validate_enough_chains(chain, n_chain)
  overlay_chain <- !is.null(chain)

  accept_stat <- dplyr::filter_(x, ~ Parameter == "accept_stat__")
  divergent <- dplyr::filter_(x, ~ Parameter == "divergent__")
  divergent$Value <- factor(divergent$Value, levels = c(0, 1),
                            labels = c("No divergence", "Divergence"))

  violin_lp_data <- data.frame(divergent, lp = lp$Value)
  violin_lp <- ggplot(violin_lp_data,
                      aes_(x = ~Value, y = ~lp)) +
    geom_violin(
      fill = get_color("l"),
      color = get_color("lh")
    ) +
    ylab("Log-posterior") +
    theme_default(x_lab = FALSE)

  violin_accept_stat_data <- data.frame(divergent, as = accept_stat$Value)
  violin_accept_stat <- ggplot(violin_accept_stat_data,
                               aes_(x = ~Value, y = ~as)) +
    geom_violin(
      fill = get_color("l"),
      color = get_color("lh")
    ) +
    ylab("accept_stat__") +
    theme_default(x_lab = FALSE)

  if (!is.null(chain)) {
    violin_lp <- violin_lp +
      chain_violin(violin_lp_data, chain)
    violin_accept_stat <- violin_accept_stat +
      chain_violin(violin_accept_stat_data, chain)
  }

  nuts_plot <- gridExtra::arrangeGrob(violin_lp, violin_accept_stat, nrow = 2)
  gridExtra::grid.arrange(nuts_plot)
  invisible(nuts_plot)
}



#' @rdname MCMC-nuts
#' @export
mcmc_nuts_stepsize <- function(x,
                               lp,
                               chain = NULL,
                               ...) {
  x <- validate_nuts_data_frame(x, lp)
  n_chain <- length(unique(lp$Chain))
  chain <- validate_enough_chains(chain, n_chain)
  overlay_chain <- !is.null(chain)

  stepsize <- dplyr::filter_(x, ~ Parameter == "stepsize__")
  accept_stat <- dplyr::filter_(x, ~ Parameter == "accept_stat__")

  stepsize_by_chain <-
    dplyr::summarise_(dplyr::group_by_(stepsize, ~Chain),
                      ss = ~first(Value))
  stepsize_labels <-
    scale_x_discrete(labels = with(
      dplyr::arrange_(stepsize_by_chain, ~ ss),
      paste0(format(round(ss, 3), digits = 3), "\n(chain ", Chain,  ")")
    ))

  violin_lp_data <- dplyr::left_join(lp, stepsize_by_chain, by = "Chain")
  violin_lp <- ggplot(violin_lp_data,
                      aes_(x = ~as.factor(ss), y = ~Value)) +
    geom_violin(
      fill = get_color("l"),
      color = get_color("lh")
    ) +
    ylab("Log-posterior") +
    stepsize_labels +
    theme_default(x_lab = FALSE)

  violin_accept_stat_data <-
    dplyr::left_join(accept_stat, stepsize_by_chain, by = "Chain")
  violin_accept_stat <- ggplot(violin_accept_stat_data,
                               aes_(x = ~as.factor(ss), y = ~Value)) +
    geom_violin(
      fill = get_color("l"),
      color = get_color("lh")
    ) +
    ylab("accept_stat__") +
    stepsize_labels +
    theme_default(x_lab = FALSE)

  if (!is.null(chain)) {
    violin_lp <- violin_lp +
      chain_violin(violin_lp_data, chain)
    violin_accept_stat <- violin_accept_stat +
      chain_violin(violin_accept_stat_data, chain)
  }

  nuts_plot <- gridExtra::arrangeGrob(violin_lp, violin_accept_stat, nrow = 2)
  gridExtra::grid.arrange(nuts_plot)
  invisible(nuts_plot)
}


#' @rdname MCMC-nuts
#' @export
mcmc_nuts_treedepth <- function(x,
                                lp,
                                chain = NULL,
                                ...) {

  x <- validate_nuts_data_frame(x, lp)
  n_chain <- length(unique(lp$Chain))
  chain <- validate_enough_chains(chain, n_chain)
  overlay_chain <- !is.null(chain)

  treedepth <- dplyr::filter_(x, ~ Parameter == "treedepth__")
  accept_stat <- dplyr::filter_(x, ~ Parameter == "accept_stat__")

  hist_td <- ggplot(treedepth, aes_(x = ~ Value, y = ~ ..density..)) +
    geom_histogram(
      fill = get_color("l"),
      color = get_color("lh"),
      size = .5,
      na.rm = TRUE,
      binwidth = 1
    ) +
    xlab("treedepth__") +
    theme_default(y_text = FALSE)

  violin_lp_data <- data.frame(treedepth, lp = lp$Value)
  violin_lp <-
    ggplot(violin_lp_data, aes_(x = ~ factor(Value), y = ~ lp)) +
    geom_violin(fill = get_color("l"),
                color = get_color("lh")) +
    labs(x = "treedepth__", y = "Log-posterior") +
    theme_default()

  violin_accept_stat_data <- data.frame(treedepth, as = accept_stat$Value)
  violin_accept_stat <-
    ggplot(violin_accept_stat_data, aes_(x = ~ factor(Value), y = ~ as)) +
    geom_violin(fill = get_color("l"),
                color = get_color("lh")) +
    labs(x = "treedepth__", y = "accept_stat__") +
    theme_default()

  if (overlay_chain) {
    hist_td <- hist_td +
      geom_histogram(
        data = dplyr::filter_(treedepth, ~Chain == chain),
        fill = get_color("d"),
        color = NA,
        alpha = 0.5,
        na.rm = TRUE,
        binwidth = 1
      )

    violin_lp <- violin_lp +
      chain_violin(violin_lp_data, chain)
    violin_accept_stat <- violin_accept_stat +
      chain_violin(violin_accept_stat_data, chain)
  }

  nuts_plot <- gridExtra::arrangeGrob(violin_lp, violin_accept_stat,
                                      hist_td, nrow = 3)
  gridExtra::grid.arrange(nuts_plot)
  invisible(nuts_plot)
}


#' @rdname MCMC-nuts
#' @export
#' @param alpha For \code{mcmc_nuts_energy} only, the transparency (alpha) level
#'   in [0,1] used for the overlaid histogram.
#' @param merge_chains For \code{mcmc_nuts_energy} only, should all chains be
#'   merged or displayed separately?
#'
mcmc_nuts_energy <-
  function(x,
           ...,
           binwidth = NULL,
           alpha = 0.5,
           merge_chains = TRUE) {
    x <- validate_nuts_data_frame(x)

    if ("chain" %in% names(list(...)))
      stop(
        "'mcmc_nuts_energy' does not accept a 'chain' argument. ",
        "Use 'merge_chains = FALSE' to view chains separately."
      )

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
      theme_default(
        y_text = FALSE,
        legend.text.align = 0,
        legend.text = element_text(size = rel(1.1)),
        legend_position = if (merge_chains) c(.8, .5) else "right"
      )

    if (merge_chains)
      return(graph)

    graph + facet_wrap( ~ Chain)
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

validate_nuts_data_frame <- function(x, lp) {
  stopifnot(is.data.frame(x))
  if (!missing(lp)) {
    stopifnot(
      is.data.frame(lp),
      length(unique(x$Chain)) == length(unique(lp$Chain))
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

