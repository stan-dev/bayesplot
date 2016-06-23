#' Plots for the No-U-Turn-Sampler (NUTS)
#'
#' @name MCMC-nuts
#' @aliases NUTS
#' @family MCMC
#'
#' @param lp Log-posterior. A list with one component (a vector) per chain or
#'   a matrix with one column per chain.
#' @param chain A positive integer for selecting a particular chain. The default
#'   (\code{NULL}) is to merge the chains before plotting. If \code{chain = k}
#'   then the plot for chain \code{k} is overlaid on top of the plot for all
#'   chains.
#' @param ... Currently ignored.
#'
#' @return A gtable object (the result of calling
#'   \code{\link[gridExtra]{arrangeGrob}}) created from several ggplot objects.
#'
#' @template seealso-color-scheme
#'
NULL

#' @rdname MCMC-nuts
#' @export
#' @template args-nuts-pars
#' @templateVar NUTSparam accept_stat
#' @template args-hist
#'
mcmc_nuts_accept_stat <- function(accept_stat, lp,
                                   chain = NULL,
                                   ...,
                                   binwidth = NULL) {

  accept_stat <- maybe_cbind_then_melt(accept_stat)
  lp <- maybe_cbind_then_melt(lp)
  stopifnot(dim(accept_stat) == dim(lp))
  chain <- validate_enough_chains(chain, length(unique(lp$Chain)))

  data <- rbind(accept_stat, lp)
  data$Parameter <-
    rep(c("accept_stat__", "Log-posterior"), each = nrow(lp))

  grp_par <- dplyr::group_by_(data, ~ Parameter)
  stats_par <-
    dplyr::summarise_(grp_par,
                      Mean = ~ mean(Value),
                      Median = ~ median(Value))

  hists <- ggplot(data, aes_(x = ~ Value, y = ~ ..density..)) +
    geom_histogram(
      fill = get_color("mid"),
      color = get_color("mid_highlight"),
      size = .25,
      na.rm = TRUE,
      binwidth = binwidth
    ) +
    geom_vline(aes_(xintercept = ~ Mean),
               data = stats_par,
               color = get_color("dark_highlight")) +
    geom_vline(
      aes_(xintercept = ~ Median),
      data = stats_par,
      color = get_color("dark"),
      linetype = 2
    ) +
    dont_expand_y_axis(c(0.005, 0)) +
    facet_wrap(~ Parameter, scales = "free") +
    theme_ppc(y_text = FALSE, x_lab = FALSE)

  scatter <- ggplot(NULL) +
    geom_point(
      aes_(x = ~ accept_stat$Value, y = ~ lp$Value),
      shape = 21,
      fill = get_color("mid"),
      color = get_color("mid_highlight")
    ) +
    labs(x = "accept_stat__",
         y = "Log-posterior") +
    theme_ppc()


  if (!is.null(chain)) {
    grp_par_chain <- dplyr::group_by_(data, .dots = list( ~ Parameter, ~ Chain))
    stats_par_chain <- dplyr::summarise_(grp_par_chain,
                                         Mean = ~ mean(Value),
                                         Median = ~ median(Value))
    chain_color <- color_vector_chain(length(unique(data$Chain)))[chain]
    hists <- hists +
      geom_histogram(
        data = dplyr::filter_(data, ~Chain == chain),
        fill = chain_color,
        color = NA,
        alpha = 0.5,
        na.rm = TRUE,
        binwidth = binwidth
      )

    line_data <- dplyr::filter_(stats_par_chain, ~ Chain == chain)
    hists <- hists +
      geom_vline(aes_(xintercept = ~ Mean),
                 data = line_data,
                 color = chain_color) +
      geom_vline(
        aes_(xintercept = ~ Median),
        data = line_data,
        color = chain_color,
        linetype = 2
      )

    scatter <- scatter +
      geom_point(
        aes_(x = ~ accept_stat$Value[accept_stat$Chain == chain],
             y = ~ lp$Value[lp$Chain == chain]),
        color = chain_color,
        alpha = 0.5
      )
  }

  nuts_plot <- gridExtra::arrangeGrob(scatter, hists, nrow = 2)
  gridExtra::grid.arrange(nuts_plot)
  invisible(nuts_plot)
}



#' @rdname MCMC-nuts
#' @export
#' @template args-nuts-pars
#' @templateVar NUTSparam treedepth
#'
mcmc_nuts_treedepth <- function(treedepth,
                                accept_stat,
                                lp,
                                chain = NULL,
                                ...) {

  accept_stat <- maybe_cbind_then_melt(accept_stat)
  treedepth <- maybe_cbind_then_melt(treedepth)
  lp <- maybe_cbind_then_melt(lp)
  stopifnot(dim(accept_stat) == dim(lp),
            dim(treedepth) == dim(lp))

  n_chain <- length(unique(lp$Chain))
  chain <- validate_enough_chains(chain, n_chain)

  hist_td <- ggplot(treedepth, aes_(x = ~ Value, y = ~ ..density..)) +
    geom_histogram(
      fill = get_color("mid"),
      color = get_color("mid_highlight"),
      size = .25,
      na.rm = TRUE,
      binwidth = 1
    ) +
    xlab("treedepth__") +
    theme_ppc(y_text = FALSE)

  violin_lp_data <- data.frame(treedepth, lp = lp$Value)
  violin_lp <- ggplot(violin_lp_data,
                      aes_(x = ~factor(Value), y = ~lp)) +
    geom_violin(
      fill = get_color("mid"),
      color = get_color("mid_highlight")
    ) +
    labs(x = "treedepth__", y = "Log-posterior") +
    theme_ppc()

  violin_accept_stat_data <- data.frame(treedepth, as = accept_stat$Value)
  violin_accept_stat <- ggplot(violin_accept_stat_data,
                               aes_(x = ~factor(Value), y = ~as)) +
    geom_violin(
      fill = get_color("mid"),
      color = get_color("mid_highlight")
    ) +
    labs(x = "treedepth__", y = "accept_stat__") +
    theme_ppc()

  if (!is.null(chain)) {
    chain_color <- color_vector_chain(n_chain)[chain]

    hist_td <- hist_td +
      geom_histogram(
        data = dplyr::filter_(treedepth, ~Chain == chain),
        fill = chain_color,
        color = NA,
        alpha = 0.5,
        na.rm = TRUE,
        binwidth = 1
      )

    violin_lp <- violin_lp +
      geom_violin(
        data = dplyr::filter_(violin_lp_data, ~Chain == chain),
        fill = chain_color,
        color = NA,
        alpha = 0.5
      )

    violin_accept_stat <- violin_accept_stat +
      geom_violin(
        data = dplyr::filter_(violin_accept_stat_data, ~Chain == chain),
        fill = chain_color,
        color = NA,
        alpha = 0.5
      )
  }

  nuts_plot <- gridExtra::arrangeGrob(violin_lp, violin_accept_stat,
                                      hist_td, nrow = 3)
  gridExtra::grid.arrange(nuts_plot)
  invisible(nuts_plot)
}






#' @rdname MCMC-nuts
#' @export
#' @template args-nuts-pars
#' @templateVar NUTSparam divergent
#'
mcmc_nuts_divergent <- function(divergent,
                                accept_stat,
                                lp,
                                chain = NULL,
                                ...) {

  divergent <- maybe_cbind_then_melt(divergent)
  divergent$Value <- factor(divergent$Value, levels = c(0, 1), labels = c("No divergence", "Divergence"))
  accept_stat <- maybe_cbind_then_melt(accept_stat)
  lp <- maybe_cbind_then_melt(lp)
  stopifnot(dim(accept_stat) == dim(lp),
            dim(divergent) == dim(lp))

  n_chain <- length(unique(lp$Chain))
  chain <- validate_enough_chains(chain, n_chain)

  violin_lp_data <- data.frame(divergent, lp = lp$Value)
  violin_lp <- ggplot(violin_lp_data,
                      aes_(x = ~Value, y = ~lp)) +
    geom_violin(
      fill = get_color("mid"),
      color = get_color("mid_highlight")
    ) +
    ylab("Log-posterior") +
    theme_ppc(x_lab = FALSE)

  violin_accept_stat_data <- data.frame(divergent, as = accept_stat$Value)
  violin_accept_stat <- ggplot(violin_accept_stat_data,
                               aes_(x = ~Value, y = ~as)) +
    geom_violin(
      fill = get_color("mid"),
      color = get_color("mid_highlight")
    ) +
    ylab("accept_stat__") +
    theme_ppc(x_lab = FALSE)

  if (!is.null(chain)) {
    chain_color <- color_vector_chain(n_chain)[chain]

    violin_lp <- violin_lp +
      geom_violin(
        data = dplyr::filter_(violin_lp_data, ~Chain == chain),
        fill = chain_color,
        color = NA,
        alpha = 0.5
      )

    violin_accept_stat <- violin_accept_stat +
      geom_violin(
        data = dplyr::filter_(violin_accept_stat_data, ~Chain == chain),
        fill = chain_color,
        color = NA,
        alpha = 0.5
      )
  }

  nuts_plot <- gridExtra::arrangeGrob(violin_lp, violin_accept_stat, nrow = 2)
  gridExtra::grid.arrange(nuts_plot)
  invisible(nuts_plot)
}





#' @rdname MCMC-nuts
#' @export
#' @template args-nuts-pars
#' @templateVar NUTSparam stepsize
#'
mcmc_nuts_stepsize <- function(stepsize,
                               accept_stat,
                               lp,
                               chain = NULL,
                               ...) {

  stepsize <- maybe_cbind_then_melt(stepsize)
  accept_stat <- maybe_cbind_then_melt(accept_stat)
  lp <- maybe_cbind_then_melt(lp)
  stopifnot(dim(accept_stat) == dim(lp),
            dim(stepsize) == dim(lp))

  n_chain <- length(unique(lp$Chain))
  chain <- validate_enough_chains(chain, n_chain)

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
      fill = get_color("mid"),
      color = get_color("mid_highlight")
    ) +
    ylab("Log-posterior") +
    stepsize_labels +
    theme_ppc(x_lab = FALSE)

  violin_accept_stat_data <-
    dplyr::left_join(accept_stat, stepsize_by_chain, by = "Chain")
  violin_accept_stat <- ggplot(violin_accept_stat_data,
                               aes_(x = ~as.factor(ss), y = ~Value)) +
    geom_violin(
      fill = get_color("mid"),
      color = get_color("mid_highlight")
    ) +
    ylab("accept_stat__") +
    stepsize_labels +
    theme_ppc(x_lab = FALSE)

  if (!is.null(chain)) {
    chain_color <- color_vector_chain(n_chain)[chain]

    violin_lp <- violin_lp +
      geom_violin(
        data = dplyr::filter_(violin_lp_data, ~Chain == chain),
        fill = chain_color,
        color = NA,
        alpha = 0.5
      )

    violin_accept_stat <- violin_accept_stat +
      geom_violin(
        data = dplyr::filter_(violin_accept_stat_data, ~Chain == chain),
        fill = chain_color,
        color = NA,
        alpha = 0.5
      )
  }

  nuts_plot <- gridExtra::arrangeGrob(violin_lp, violin_accept_stat, nrow = 2)
  gridExtra::grid.arrange(nuts_plot)
  invisible(nuts_plot)
}




# internal ----------------------------------------------------------------
maybe_cbind_then_melt <- function(x) {
  if (is.list(x)) {
    x <- do.call("cbind", x)
  } else {
    stopifnot(is.matrix(x))
  }
  out <- reshape2::melt(x)
  colnames(out) <- c("Iteration", "Chain", "Value")
  out
}

validate_enough_chains <- function(chain = NULL, n_chain) {
  if (!is.null(chain)) {
    stopifnot(chain >= 1)
    if (!isTRUE(n_chain >= chain))
      stop("'chain' is ", chain, " but only ", n_chain, " chains found.")
  }
  chain
}

color_vector_chain <- function(n) {
  hues = seq(15, 375, length = n + 2)
  hcl(h = hues, l = 80, c = 50)[2:(n+1)]
}
