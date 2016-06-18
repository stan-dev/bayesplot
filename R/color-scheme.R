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
#' set_color_scheme("greens")
#' ppc_stat_2d(y, yrep)
#'
#' set_color_scheme("purples")
#' ppc_hist(y, yrep[1:8,])
#'
set_color_scheme <-
  function(scheme = c("reds", "blues", "greens", "greys", "purples")) {
    x <- switch(
      match.arg(scheme),
      "reds" = scheme_reds(),
      "blues" = scheme_blues(),
      "greens" = scheme_greens(),
      "greys" = scheme_greys(),
      "purples" = scheme_purples()
    )
    for (lev in scheme_level_names())
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

# Access a single color value
#
# @param level A haracter vector of level names (see scheme_level_names())
# @return A character vector of color values.
#
get_color <- function(levels) {
  stopifnot(all(levels %in% scheme_level_names()))
  color_vals <- get_color_scheme()[levels]
  unlist(color_vals, use.names = FALSE)
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

# Create a scheme from RColorBrewer palette
#
# @param name Palette named passed to RColorBrewer::brewer.pal
# @return A list of six colors
#
brew_scheme <- function(name) {
  x <- RColorBrewer::brewer.pal(9, name)
  x <- as.list(x[3:8])
  setNames(x, scheme_level_names())
}

# Color schemes
scheme_blues <- function() brew_scheme("Blues")
scheme_greens <- function() brew_scheme("Greens")
scheme_purples <- function() brew_scheme("Purples")
scheme_greys <- function() brew_scheme("Greys")
scheme_reds <- function() {
  reds <-
    list("#DCBCBC",
         "#C79999",
         "#B97C7C",
         "#A25050",
         "#8F2727",
         "#7C0000")
  setNames(reds, scheme_level_names())
}


# instantiate aesthetics --------------------------------------------------
.bayesplot_aesthetics <- new.env(parent = emptyenv())
set_color_scheme("reds")
