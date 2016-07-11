#' Set or get the color scheme
#'
#' Set or get the color scheme used for plotting.
#'
#' @export
#' @param scheme A string naming the color scheme to use.
#' @return \code{set_color_scheme} has the side effect of setting the color
#'   scheme used for plotting. It also returns,
#'   \code{\link[=invisible]{invisibly}}, a list of the hexidecimal color values
#'   used in \code{scheme}.
#'
#' @examples
#' get_color_scheme()
#' y <- rnorm(100)
#' yrep <- matrix(rnorm(2500), ncol = 100)
#' ppc_stat_2d(y, yrep)
#'
#' set_color_scheme("teal")
#' ppc_stat_2d(y, yrep)
#'
#' set_color_scheme("blue")
#' ppc_hist(y, yrep[1:8,])
#'
set_color_scheme <-
  function(scheme = c("red", "blue", "gray", "green", "pink", "teal")) {
    x <- prepare_colors(scheme)
    for (lev in names(x))
      .bayesplot_aesthetics[[lev]] <- x[[lev]]

    invisible(x)
  }

#' @export
#' @rdname set_color_scheme
#' @return \code{get_color_scheme} returns a \code{list} of the hexadecimal
#'   color values used by the current scheme.
#'
get_color_scheme <- function() {
  x <- as.list(.bayesplot_aesthetics)
  x[scheme_level_names()]
}




# helpers -----------------------------------------------------------------

# Access a subset of the scheme colors
#
# @param level A character vector of level names (see scheme_level_names()). The
#   abbreviations "l", "lh", "m", "mh", "d", and "dh" can also be used instead
#   of the full names.
# @return A character vector of color values.
#
get_color <- function(levels) {
  sel <- which(!levels %in% scheme_level_names())
  if (length(sel))
    levels[sel] <- sapply(levels[sel], full_level_name)
  stopifnot(all(levels %in% scheme_level_names()))
  color_vals <- get_color_scheme()[levels]
  unlist(color_vals, use.names = FALSE)
}
full_level_name <- function(x) {
  switch(x,
         l = "light", lh = "light_highlight",
         m = "mid", mh = "mid_highlight",
         d = "dark", dh = "dark_highlight"
         )
}


# Color scheme level names
scheme_level_names <- function() {
  c("light",
    "light_highlight",
    "mid",
    "mid_highlight",
    "dark",
    "dark_highlight")
}

# Color schemes
master_color_list <- list(
  blue =
    list(
      "#bcccdc",
      "#99b0c7",
      "#7c9bb9",
      "#5079a2",
      "#275b8f",
      "#003e7c"
    ),
  gray =
    list(
      "#D9D9D9",
      "#BDBDBD",
      "#969696",
      "#737373",
      "#525252",
      "#252525"
    ),
  green =
    list(
      "#bcdcbc",
      "#99c799",
      "#7cb97c",
      "#50a250",
      "#278f27",
      "#007c00"
    ),
  pink =
    list(
      "#dcbccc",
      "#c799b0",
      "#b97c9b",
      "#a25079",
      "#8f275b",
      "#7c003e"
    ),
  red =
    list(
      "#DCBCBC",
      "#C79999",
      "#B97C7C",
      "#A25050",
      "#8F2727",
      "#7C0000"
    ),
  teal =
    list(
      "#bcdcdc",
      "#99c7c7",
      "#7cb9b9",
      "#50a2a2",
      "#278f8f",
      "#007C7C"
    )
)
prepare_colors <- function(scheme) {
  setNames(master_color_list[[scheme]],
           scheme_level_names())
}


# instantiate aesthetics --------------------------------------------------
.bayesplot_aesthetics <- new.env(parent = emptyenv())
set_color_scheme("red")
