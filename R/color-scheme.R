#' Set, get, or view color schemes
#'
#' Set, get, or view color schemes. Choose from a preset
#' scheme or create a custom scheme.
#'
#' @export
#' @param scheme For \code{set_color_scheme}, either a string naming one of the
#'   available color schemes or a character vector of \emph{exactly six} colors
#'   specifying a custom scheme (see the \strong{Custom Color Schemes} section,
#'   below, for more on specifying a custom scheme).
#'
#'   For \code{get_color_scheme}, \code{scheme} can be missing (to get the
#'   current color scheme) or a string naming one of the preset schemes.
#'
#'   For \code{view_color_scheme}, \code{scheme} can be missing (to use the
#'   current color scheme) or a character vector containing a subset of the
#'   available scheme names.
#'
#'   Currently, the available preset color schemes are:
#'   \itemize{
#'    \item \code{"blue"}
#'    \item \code{"brightblue"}
#'    \item \code{"gray"}
#'    \item \code{"green"}
#'    \item \code{"pink"}
#'    \item \code{"purple"}
#'    \item \code{"red"} (default)
#'    \item \code{"teal"}
#'    \item \code{"yellow"}
#'    \item \code{"mix-x-y"}, replacing \code{x} and \code{y} with any two of
#'    the scheme names listed above (e.g. "mix-teal-pink", "mix-blue-red",
#'    etc.). The order of \code{x} and \code{y} matters, i.e., the color schemes
#'    "mix-blue-red" and "mix-red-blue" are not identical. There is no gaurantee
#'    that every possible mixed scheme will look good with every possible plot.
#'   }
#'
#' @return \code{set_color_scheme} has the side effect of setting the color
#'   scheme used for plotting. It also returns
#'   (\code{\link[=invisible]{invisibly}}) a list of the hexidecimal color
#'   values used in \code{scheme}.
#'
#'   \code{get_color_scheme} returns a \code{list} of the hexadecimal color
#'   values (without changing the current scheme). If the \code{scheme} argument
#'   is not specified the returned values correspond to the current color
#'   scheme.
#'
#'   \code{view_color_scheme} returns a ggplot object if only a single scheme is
#'   specified and a gtable object if multiple schemes names are specified.
#'
#'
#' @section Custom Color Schemes: A \pkg{bayesplot} color scheme consists of six
#'   colors. To specify a custom color scheme simply pass a character vector
#'   containing either the names of six \code{\link[grDevices]{colors}} or six
#'   hexidecimal color values (or a mix of names and hex values). The colors
#'   should be in order from lightest to darkest. See the end of the
#'   \strong{Examples} section for a demonstration.
#'
#' @examples
#' set_color_scheme("red")
#' get_color_scheme()
#' view_color_scheme()
#'
#' # compare multiple schemes
#' view_color_scheme(c("pink", "gray", "teal"))
#'
#' get_color_scheme("brightblue")
#' view_color_scheme("brightblue")
#' view_color_scheme("purple")
#' get_color_scheme("purple")$light
#'
#'
#' set_color_scheme("pink")
#' x <- example_mcmc_draws()
#' mcmc_intervals(x)
#'
#' set_color_scheme("teal")
#' view_color_scheme()
#' mcmc_intervals(x)
#'
#' set_color_scheme("blue")
#' mcmc_areas(x, regex_pars = "beta")
#'
#' set_color_scheme("pink")
#' view_color_scheme()
#' y <- example_y_data()
#' yrep <- example_yrep_draws()
#' ppc_stat(y, yrep, stat = "mean") + no_legend()
#'
#' set_color_scheme("mix-teal-pink")
#' ppc_stat(y, yrep, stat = "sd") + no_legend()
#' mcmc_areas(x, regex_pars = "beta")
#'
#' ###########################
#' ### custom color scheme ###
#' ###########################
#' orange_scheme <- c("#ffebcc", "#ffcc80",
#'                    "#ffad33", "#e68a00",
#'                    "#995c00", "#663d00")
#' set_color_scheme(orange_scheme)
#' get_color_scheme()
#' mcmc_areas(x, regex_pars = "alpha")
#' mcmc_dens_overlay(x)
#' ppc_stat(y, yrep, stat = "var") + no_legend()
#'
set_color_scheme <- function(scheme) {
  stopifnot(is.character(scheme))
  if (length(scheme) == 1) {
    x <- scheme_from_string(scheme)
  } else if (length(scheme) == 6) {
    x <- prepare_custom_colors(scheme)
  } else {
    stop("'scheme' should be a character vector of length 1 or 6.")
  }

  for (lev in names(x))
    .bayesplot_aesthetics$scheme[[lev]] <- x[[lev]]

  attr(.bayesplot_aesthetics$scheme, "mixed") <- attr(x, "mixed")
  invisible(x)
}

#' @rdname set_color_scheme
#' @export
get_color_scheme <- function(scheme) {
  if (missing(scheme)) {
    x <- .bayesplot_aesthetics$scheme
    scheme <- as.list(x)[scheme_level_names()]
    attr(scheme, "mixed") <- attr(x, "mixed")
    return(scheme)
  }
  scheme_from_string(scheme)
}

#' @rdname set_color_scheme
#' @export
view_color_scheme <- function(scheme) {
  suggested_package("gridExtra")
  if (missing(scheme) || length(scheme) == 1)
    return(.view_scheme(scheme))

  gridExtra::grid.arrange(
    grobs = lapply(scheme, .view_scheme),
    ncol = length(scheme)
  )
}



# internal -----------------------------------------------------------------

# plot color scheme
# @param scheme A string (length 1) naming a scheme
.view_scheme <- function(scheme) {
  x <- if (missing(scheme))
    get_color_scheme() else get_color_scheme(scheme)

  color_data <- data.frame(
    group = factor(names(x), levels = rev(names(x))),
    value = rep(1, length(x))
  )
  ggplot(
    color_data,
    aes_(
      x = if (missing(scheme)) "" else factor(scheme),
      y = ~ value,
      fill = ~ group
    )
  ) +
    geom_bar(
      width = .5,
      stat = "identity",
      color = "white",
      size = 0.1
    ) +
    scale_fill_manual("", values = unlist(x)) +
    theme_void() +
    no_legend() +
    xaxis_text(
      size = rel(1.1),
      face = "bold",
      margin = margin(t = -3, b = 10)
    )
}


# @param scheme A string (length 1) naming a scheme
scheme_from_string <- function(scheme) {
  stopifnot(length(scheme) == 1)
  if (identical(substr(scheme, 1, 4), "mix-")) {
    to_mix <- unlist(strsplit(scheme, split = "-"))[2:3]
    x <- setNames(mixed_scheme(to_mix[1], to_mix[2]), scheme_level_names())
    structure(x, mixed = TRUE)
  } else {
    scheme <- match.arg(scheme, choices = names(master_color_list))
    x <- prepare_colors(scheme)
    structure(x, mixed = FALSE)
  }
}

# check if object returned by get_color_scheme is a mixed scheme
# @param x object returned by get_color_scheme
is_mixed_scheme <- function(x) {
  stopifnot(is.list(x))
  isTRUE(attr(x, "mixed"))
}

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

prepare_colors <- function(scheme) {
  setNames(
    master_color_list[[scheme]],
    scheme_level_names()
  )
}

prepare_custom_colors <- function(scheme) {
  if (length(scheme) != 6)
    stop("Custom color schemes must contain exactly 6 colors.",
         call. = FALSE)

  not_found <- character(0)
  for (j in seq_along(scheme)) {
    clr <- scheme[j]
    if (!is_hex_color(clr)  && !clr %in% grDevices::colors())
      not_found <- c(not_found, clr)
  }
  if (length(not_found))
    STOP_bad_colors(not_found)

  setNames(as.list(scheme), scheme_level_names())
}

is_hex_color <- function(x) {
  if (!identical(substr(x, 1, 1), "#"))
    return(FALSE)
  isTRUE(nchar(x) == 7)
}

# @param x character vector of bad color names
STOP_bad_colors <- function(x) {
  stop(
    "Each color must specified as either a hexidecimal color value ",
    "(e.g. '#C79999') or the name of a color (e.g. 'blue'). ",
    "The following provided colors were not found: ",
    paste(unlist(x), collapse = ", "),
    call. = FALSE
  )
}

# master color list -------------------------------------------------------
# create mixed scheme
mixed_scheme <- function(scheme1, scheme2) {
  scheme1 <- get_color_scheme(scheme1)
  scheme2 <- get_color_scheme(scheme2)
  scheme <- unname(list(
    scheme1$light,
    scheme2$light_highlight,
    scheme2$mid,
    scheme1$mid_highlight,
    scheme1$dark,
    scheme2$dark_highlight
  ))
  attr(scheme, "mixed") <- TRUE
  return(scheme)
}

master_color_list <- list(
  blue =
    list("#d1e1ec", "#b3cde0", "#6497b1", "#005b96", "#03396c", "#011f4b"),
  brightblue =
    list("#cce5ff", "#99cbff", "#4ca5ff", "#198bff", "#0065cc", "#004c99"),
  gray =
    list("#DFDFDF", "#bfbfbf", "#999999", "#737373", "#505050", "#383838"), # "#0d0d0d"),
  green =
    list("#d9f2e6", "#9fdfbf", "#66cc99", "#40bf80", "#2d8659", "#194d33"),
  pink =
    list("#dcbccc", "#c799b0", "#b97c9b", "#a25079", "#8f275b", "#7c003e"),
  purple =
    list("#e5cce5", "#bf7fbf", "#a64ca6", "#800080", "#660066", "#400040"),
  red =
    list("#DCBCBC", "#C79999", "#B97C7C", "#A25050", "#8F2727", "#7C0000"),
  # brightred =
  #   list("gray90", "gray70", "#ff4c4c", "#ff0000", "#b20000", "red4"),
  teal =
    list("#bcdcdc", "#99c7c7", "#7cb9b9", "#50a2a2", "#278f8f", "#007C7C"),
  yellow =
    list("#fbf3da", "#f8e8b5", "#f5dc90", "#dbc376", "#aa975c", "#7a6c42")
)

# instantiate aesthetics --------------------------------------------------
.bayesplot_aesthetics <- new.env(parent = emptyenv())
.bayesplot_aesthetics$scheme <- list()
set_color_scheme("red")
