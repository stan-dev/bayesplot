#' Unified wrapper functions for bayesplot families
#'
#' @description
#' High-level wrapper functions that consolidate related plotting functions
#' within each bayesplot family into a single entry point. Each wrapper uses a
#' `type` argument to select the specific plot variant, and (where applicable) a
#' `grouped` flag to switch to a faceted-by-group version.
#'
#' All additional arguments are forwarded to the underlying plotting function
#' via `...`.
#'
#' @details
#' These wrappers are thin dispatchers. The underlying functions and their full
#' documentation are still available directly (e.g., [ppc_error_hist()],
#' [mcmc_rhat()]). Use [available_ppc()], [available_ppd()], or
#' [available_mcmc()] to discover all plotting functions.
#'
#' @name bayesplot-wrappers
#' @seealso [available_ppc()], [available_ppd()], [available_mcmc()],
#'   [PPC-overview]
NULL


# Internal dispatcher -----------------------------------------------------

.dispatch <- function(prefix, type, is_grouped, ...) {
  suffix <- type
  if (is_grouped) {
    suffix <- paste0(suffix, "_grouped")
  }
  fn_name <- paste0(prefix, "_", suffix)
  fn <- tryCatch(
    match.fun(fn_name),
    error = function(e) {
      if (is_grouped) {
        abort(paste0(
          "No grouped variant available for type '", type, "'. ",
          "Try `grouped = FALSE` or a different `type`."
        ))
      }
      abort(paste0(
        "'", type, "' is not a valid type for `", prefix, "()`. ",
        "See the documentation for supported types."
      ))
    }
  )
  fn(...)
}


# PPC wrappers ------------------------------------------------------------

#' @rdname bayesplot-wrappers
#' @export
#'
#' @param type A string selecting the specific plot variant within the family.
#'   Partial matching is supported. See each wrapper's section below for valid
#'   values.
#' @param grouped If `TRUE`, use the grouped (faceted) variant of the plot when
#'   one exists. A `group` argument must also be provided via `...`.
#' @param ... Arguments passed to the underlying plotting function
#'   (e.g., `y`, `yrep`, `prob`, `size`, etc.).
#'
#' @section PPC error plots (`ppc_error()`):
#' Dispatches to the [PPC-errors] family.
#'
#' Valid types: `"hist"`, `"scatter"`, `"scatter_avg"`, `"binned"`.
#' Grouped variants exist for `"hist"` and `"scatter_avg"`.
#'
#' @examples
#' y <- example_y_data()
#' yrep <- example_yrep_draws()
#'
#' \donttest{
#' ppc_error(y = y, yrep = yrep[1:3, ], type = "hist")
#' }
#'
ppc_error <- function(type = c("hist", "scatter", "scatter_avg", "binned"),
                      grouped = FALSE,
                      ...) {
  type <- match.arg(type)
  .dispatch("ppc_error", type, grouped, ...)
}


#' @rdname bayesplot-wrappers
#' @export
#'
#' @section PPC distribution plots (`ppc_dist()`):
#' Dispatches to the [PPC-distributions] family.
#'
#' Valid types: `"hist"`, `"dens"`, `"dens_overlay"`, `"ecdf_overlay"`,
#' `"freqpoly"`, `"boxplot"`, `"violin"`, `"dots"`, `"pit_ecdf"`.
#' Grouped variants exist for `"dens_overlay"`, `"ecdf_overlay"`,
#' `"freqpoly"`, `"violin"`, and `"pit_ecdf"`.
#'
#' Note: `"violin"` only exists as a grouped variant, so `grouped = TRUE` is
#' required.
#'
#' @examples
#' \donttest{
#' ppc_dist(y = y, yrep = yrep[1:8, ], type = "hist")
#' ppc_dist(y = y, yrep = yrep, type = "dens_overlay")
#' }
#'
ppc_dist <- function(type = c("hist", "dens", "dens_overlay", "ecdf_overlay",
                              "freqpoly", "boxplot", "violin", "dots",
                              "pit_ecdf"),
                     grouped = FALSE,
                     ...) {
  type <- match.arg(type)
  if (type == "violin" && !grouped) {
    abort(paste0(
      "`ppc_violin_grouped()` only exists as a grouped variant. ",
      "Use `grouped = TRUE`."
    ))
  }
  .dispatch("ppc", type, grouped, ...)
}


#' @rdname bayesplot-wrappers
#' @export
#'
#' @section PPC discrete plots (`ppc_discrete()`):
#' Dispatches to the [PPC-discrete] family.
#'
#' Valid types: `"bars"`, `"rootogram"`.
#' A grouped variant exists for `"bars"`.
#'
ppc_discrete <- function(type = c("bars", "rootogram"),
                         grouped = FALSE,
                         ...) {
  type <- match.arg(type)
  .dispatch("ppc", type, grouped, ...)
}


#' @rdname bayesplot-wrappers
#' @export
#'
#' @section PPC LOO plots (`ppc_loo()`):
#' Dispatches to the [PPC-loo] family.
#'
#' Valid types: `"pit_overlay"`, `"pit_qq"`, `"pit_ecdf"`,
#' `"intervals"`, `"ribbon"`.
#' No grouped variants.
#'
ppc_loo <- function(type = c("pit_overlay", "pit_qq", "pit_ecdf",
                             "intervals", "ribbon"),
                    ...) {
  type <- match.arg(type)
  fn_name <- paste0("ppc_loo_", type)
  fn <- match.fun(fn_name)
  fn(...)
}


# PPD wrappers -------------------------------------------------------------

#' @rdname bayesplot-wrappers
#' @export
#'
#' @section PPD distribution plots (`ppd_dist()`):
#' Dispatches to the [PPD-distributions] family.
#'
#' Valid types: `"hist"`, `"dens"`, `"dens_overlay"`, `"ecdf_overlay"`,
#' `"freqpoly"`, `"boxplot"`, `"dots"`.
#' A grouped variant exists for `"freqpoly"`.
#'
ppd_dist <- function(type = c("hist", "dens", "dens_overlay", "ecdf_overlay",
                              "freqpoly", "boxplot", "dots"),
                     grouped = FALSE,
                     ...) {
  type <- match.arg(type)
  .dispatch("ppd", type, grouped, ...)
}


# MCMC wrappers ------------------------------------------------------------

#' @rdname bayesplot-wrappers
#' @export
#'
#' @section MCMC distribution plots (`mcmc_dist()`):
#' Dispatches to the [MCMC-distributions] family.
#'
#' Valid types: `"hist"`, `"dens"`, `"hist_by_chain"`, `"dens_overlay"`,
#' `"dens_chains"`, `"violin"`, `"dots"`, `"dots_by_chain"`.
#'
mcmc_dist <- function(type = c("hist", "dens", "hist_by_chain",
                               "dens_overlay", "dens_chains", "violin",
                               "dots", "dots_by_chain"),
                      ...) {
  type <- match.arg(type)
  fn_name <- paste0("mcmc_", type)
  fn <- match.fun(fn_name)
  fn(...)
}


#' @rdname bayesplot-wrappers
#' @export
#'
#' @section MCMC trace plots (`mcmc_trace_w()`):
#' Dispatches to the [MCMC-traces] family.
#'
#' Valid types: `"trace"`, `"trace_highlight"`, `"rank_overlay"`,
#' `"rank_hist"`, `"rank_ecdf"`.
#'
mcmc_trace_w <- function(type = c("trace", "trace_highlight", "rank_overlay",
                                  "rank_hist", "rank_ecdf"),
                         ...) {
  type <- match.arg(type)
  fn_name <- paste0("mcmc_", type)
  fn <- match.fun(fn_name)
  fn(...)
}


#' @rdname bayesplot-wrappers
#' @export
#'
#' @section MCMC diagnostic plots (`mcmc_diag()`):
#' Dispatches to the [MCMC-diagnostics] family.
#'
#' Valid types: `"rhat"`, `"rhat_hist"`, `"neff"`, `"neff_hist"`,
#' `"acf"`, `"acf_bar"`.
#'
mcmc_diag <- function(type = c("rhat", "rhat_hist", "neff", "neff_hist",
                               "acf", "acf_bar"),
                      ...) {
  type <- match.arg(type)
  fn_name <- paste0("mcmc_", type)
  fn <- match.fun(fn_name)
  fn(...)
}


#' @rdname bayesplot-wrappers
#' @export
#'
#' @section MCMC NUTS diagnostic plots (`mcmc_nuts()`):
#' Dispatches to the [MCMC-nuts] family.
#'
#' Valid types: `"acceptance"`, `"divergence"`, `"stepsize"`,
#' `"treedepth"`, `"energy"`.
#'
mcmc_nuts <- function(type = c("acceptance", "divergence", "stepsize",
                               "treedepth", "energy"),
                      ...) {
  type <- match.arg(type)
  fn_name <- paste0("mcmc_nuts_", type)
  fn <- match.fun(fn_name)
  fn(...)
}


#' @rdname bayesplot-wrappers
#' @export
#'
#' @section MCMC recovery plots (`mcmc_recover()`):
#' Dispatches to the [MCMC-recover] family.
#'
#' Valid types: `"intervals"`, `"scatter"`, `"hist"`.
#'
mcmc_recover <- function(type = c("intervals", "scatter", "hist"),
                         ...) {
  type <- match.arg(type)
  fn_name <- paste0("mcmc_recover_", type)
  fn <- match.fun(fn_name)
  fn(...)
}
