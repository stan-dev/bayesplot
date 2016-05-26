#' Index of \code{ppc} functions that stratify by group level
#'
#' @name ppcs-by-group
#' @description Index of functions in the \pkg{ppcheck} package that accept a
#'   \code{group} argument.
#' @section Functions:
#' \describe{
#'   \item{\code{\link{ppc_scatter_avg_grouped}}}{
#'    Scatterplots of \code{y} vs \code{yrep} at each level of a grouping
#'    variable.
#'   }
#'   \item{\code{\link{ppc_stat_grouped}}}{
#'    The posterior predictive distributions of a test statistic
#'    \eqn{T(y^{rep})}{T(yrep)} compared to the
#'    observed value \eqn{T(y)} at each level of a grouping variable.
#'   }
#'   \item{\code{\link{ppc_ts_grouped}}}{
#'     Interval estimates of \eqn{y^{rep}}{yrep} by time and level of a grouping
#'     variable, with \eqn{y} points overlaid.
#'   }
#'   \item{\code{\link{ppc_violin_grouped}}}{
#'     Density estimates of \code{yrep} by level of a grouping variable, with
#'     \code{y} points overlaid.
#'   }
#' }
#'
NULL
