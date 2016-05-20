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
  stopifnot(is.matrix(yrep))
  if (any(is.na(yrep)))
    stop("NAs not allowed in 'yrep'.", call. = FALSE)
  out <- reshape2::melt(
    data = yrep,
    varnames = c("rep_id", "y_id")
  )
  out$rep_id <- paste0('yrep_', out$rep_id)
  out$rep_id <- factor(out$rep_id, levels = unique(out$rep_id))
  out
}

# Call a geom passing arguments as a list
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



.PP_FILL <- "skyblue"
.PP_DARK <- "skyblue4"
.PP_VLINE_CLR <- "#222222"
.PP_YREP_CLR <- "#487575"
.PP_YREP_FILL <- "#222222"

pp_check_theme <- function(no_y = TRUE) {
  thm <- theme_classic() +
    theme(
      axis.line = element_line(color = "#222222"),
      axis.line.y = if (no_y) element_blank() else element_line(size = 0.5),
      axis.line.x = element_line(size = 2),
      axis.title = element_text(face = "bold", size = 13),
      strip.background = element_blank(),
      strip.text = element_text(color = "black", face = "bold"),
      legend.position = "none",
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 13),
      plot.title = element_text(size = 18)
    )
  if (no_y)
    thm <- thm %+replace% theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank()
    )
  thm
}
