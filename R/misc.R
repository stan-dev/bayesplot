# Validate y and yrep inputs
#
# @param y,yrep The y and yrep objects from the user.
# @return TRUE, invisibly, if no issues, otherwise throws an error.
validate_y_and_yrep <- function(y, yrep) {
  stopifnot(is.vector(y), is.matrix(yrep))
  if (ncol(yrep) != length(y))
    stop("ncol(yrep) not equal to length(y).")
  if (any(is.na(yrep)))
    stop("NAs not allowed in 'yrep'.")
  if (any(is.na(y)))
    stop("NAs not allowed in 'y'.")

  invisible(TRUE)
}


# Convert yrep matrix into a molten data frame
#
# @param yrep A matrix.
# @return A data frame with three columns:
# \itemize{
#  \item 'value': the numeric values.
#  \item 'y_id': integer indicating which yrep column each of the values comes
#  from.
# \item 'rep_id': factor with levels 'yrep_1', ..., 'yrep_S', where S is
#  nrow(yrep), i.e. the number of simulations included in yrep.
# }
#
melt_yrep <- function(yrep) {
  out <- reshape2::melt(
    data = yrep,
    varnames = c("rep_id", "y_id")
  )
  out$rep_id <- paste0('yrep_', out$rep_id)
  out$rep_id <- factor(out$rep_id, levels = unique(out$rep_id))
  out
}


# Call a geom, passing arguments as a list
#
# @param geom A string naming the geom (e.g. "histogram", "ribbon", etc.)
# @param args A list of arguments to pass to \code{geom}.
# @param ... Optional arguments passed to \code{do.call}.
#
call_geom <- function(geom, args, ...) {
  stopifnot(is.character(geom), is.list(args))
  do.call(
    what = paste0("geom_", geom),
    args = args,
    ...
  )
}


`%ORifNULL%` <- function(a, b) {
  if (is.null(a)) b else a
}


set_geom_args <- function(defaults, ...) {
  dots <- list(...)
  if (!length(dots))
    return(defaults)
  dot_names <- names(dots)
  def_names <- names(defaults)
  for (j in seq_along(def_names)) {
    if (def_names[j] %in% dot_names)
      defaults[[j]] <- dots[[def_names[j]]]
  }
  extras <- setdiff(dot_names, def_names)
  if (length(extras)) {
    for (j in seq_along(extras))
      defaults[[extras[j]]] <- dots[[extras[j]]]
  }
  defaults
}


# colors and ggplot theme
.PP_LIGHT <- "#DCBCBC"
.PP_LIGHT_highlight <- "#C79999"
.PP_MID <- "#B97C7C"
.PP_MID_highlight <- "#A25050"
.PP_DARK <- "#8F2727"
.PP_DARK_highlight <- "#7C0000"

theme_ppc <- function(y_text = FALSE, legend_position = "none") {
  thm <- theme_classic() %+replace%
    theme(
      axis.line.x = element_line(size = 0.25),
      axis.line.y = element_line(size = 0.25),
      axis.ticks = element_blank(),
      legend.position = legend_position,
      strip.background = element_rect(fill = "gray95", color = NA)
    )
  if (y_text)
    return(thm)

  thm %+replace%
    theme(
      axis.text.y = element_blank(),
      axis.title.y = element_blank()
    )
}
