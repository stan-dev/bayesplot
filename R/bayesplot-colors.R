#' Set, get, or view color schemes
#'
#' Set, get, or view color schemes. Choose from a preset scheme or create a
#' custom scheme.
#'
#' @name bayesplot-colors
#'
#' @param scheme For \code{color_scheme_set}, either a string naming one of the
#'   available color schemes or a character vector of \emph{exactly six} colors
#'   specifying a custom scheme (see the \strong{Custom Color Schemes} section,
#'   below, for more on specifying a custom scheme).
#'
#'   For \code{color_scheme_get}, \code{scheme} can be missing (to get the
#'   current color scheme) or a string naming one of the preset schemes.
#'
#'   For \code{color_scheme_view}, \code{scheme} can be missing (to use the
#'   current color scheme) or a character vector containing a subset of the
#'   available scheme names.
#'
#'   Currently, the available preset color schemes are:
#'   \itemize{
#'    \item \code{"blue"}, \code{"brightblue"}
#'    \item \code{"gray"}, \code{"darkgray"}
#'    \item \code{"green"}
#'    \item \code{"pink"}
#'    \item \code{"purple"}
#'    \item \code{"red"}
#'    \item \code{"teal"}
#'    \item \code{"yellow"}
#'    \item \code{"viridis"}, \code{"viridisA"}, \code{"viridisB"}, \code{"viridisC"}
#'    \item \code{"mix-x-y"}, replacing \code{x} and \code{y} with any two of
#'    the scheme names listed above (e.g. "mix-teal-pink", "mix-blue-red",
#'    etc.). The order of \code{x} and \code{y} matters, i.e., the color schemes
#'    "mix-blue-red" and "mix-red-blue" are not identical. There is no gaurantee
#'    that every possible mixed scheme will look good with every possible plot.
#'   }
#'
#' @return \code{color_scheme_set} has the side effect of setting the color
#'   scheme used for plotting. It also returns
#'   (\code{\link[=invisible]{invisibly}}) a list of the hexidecimal color
#'   values used in \code{scheme}.
#'
#'   \code{color_scheme_get} returns a \code{list} of the hexadecimal color
#'   values (without changing the current scheme). If the \code{scheme} argument
#'   is not specified the returned values correspond to the current color
#'   scheme. If the optional argument \code{i} is specified then the returned
#'   list only contains \code{length(i)} elements.
#'
#'   \code{color_scheme_view} returns a ggplot object if only a single scheme is
#'   specified and a gtable object if multiple schemes names are specified.
#'
#' @section Custom Color Schemes: A \pkg{bayesplot} color scheme consists of six
#'   colors. To specify a custom color scheme simply pass a character vector
#'   containing either the names of six \code{\link[grDevices]{colors}} or six
#'   hexidecimal color values (or a mix of names and hex values). The colors
#'   should be in order from lightest to darkest. See the end of the
#'   \strong{Examples} section for a demonstration.
#'
#' @template seealso-theme
#'
#' @examples
#' color_scheme_set("blue")
#' color_scheme_view()
#'
#' color_scheme_get()
#' color_scheme_get(i = c(3, 5)) # 3rd and 5th colors only
#'
#' color_scheme_get("brightblue")
#' color_scheme_view("brightblue")
#'
#' # compare multiple schemes
#' color_scheme_view(c("pink", "gray", "teal"))
#' color_scheme_view(c("viridis", "viridisA", "viridisB", "viridisC"))
#'
#' color_scheme_set("pink")
#' x <- example_mcmc_draws()
#' mcmc_intervals(x)
#'
#' color_scheme_set("teal")
#' color_scheme_view()
#' mcmc_intervals(x)
#'
#' color_scheme_set("red")
#' mcmc_areas(x, regex_pars = "beta")
#'
#' color_scheme_set("purple")
#' color_scheme_view()
#' y <- example_y_data()
#' yrep <- example_yrep_draws()
#' ppc_stat(y, yrep, stat = "mean") + legend_none()
#' \donttest{
#' color_scheme_set("mix-teal-pink")
#' ppc_stat(y, yrep, stat = "sd") + legend_none()
#' mcmc_areas(x, regex_pars = "beta")
#' }
#' ###########################
#' ### custom color scheme ###
#' ###########################
#' orange_scheme <- c("#ffebcc", "#ffcc80",
#'                    "#ffad33", "#e68a00",
#'                    "#995c00", "#663d00")
#' color_scheme_set(orange_scheme)
#' mcmc_areas(x, regex_pars = "alpha")
#' mcmc_dens_overlay(x)
#' ppc_stat(y, yrep, stat = "var") + legend_none()
#'
NULL

#' @rdname bayesplot-colors
#' @export
color_scheme_set <- function(scheme = "blue") {
  stopifnot(is.character(scheme))
  if (length(scheme) == 1) {
    x <- scheme_from_string(scheme)
  } else if (length(scheme) == 6) {
    x <- prepare_custom_colors(scheme)
  } else {
    stop("'scheme' should be a character vector of length 1 or 6.")
  }
  .bayesplot_aesthetics[["scheme"]] <- x
  invisible(x)
}

#' @rdname bayesplot-colors
#' @export
#' @param i For \code{color_scheme_get}, a subset of the integers from \code{1}
#'   (lightest) to \code{6} (darkest) indicating which of the colors in the
#'   scheme to return. If \code{i} is not specified then all six colors in the
#'   scheme are included.
#'
color_scheme_get <- function(scheme, i) {
  if (!missing(scheme)) {
    scheme <- scheme_from_string(scheme)
  } else {
    x <- .bayesplot_aesthetics$scheme
    scheme <- as.list(x)[scheme_level_names()]
    attr(scheme, "mixed") <- attr(x, "mixed")
    attr(scheme, "scheme_name") <- attr(x, "scheme_name")
  }
  class(scheme) <- c("bayesplot_scheme", "list")
  if (missing(i)) {
    return(scheme)
  } else if (is.character(i)) {
    return(get_color(i))
  }

  stopifnot(
    all(i %in% seq_along(scheme)),
    length(unique(i)) == length(i)
  )
  scheme[i]
}

#' @export
print.bayesplot_scheme <- function(x, ...) {
  tab <- data.frame(unlist(x, use.names = FALSE),
                    stringsAsFactors = FALSE)
  colnames(tab) <- attr(x, "scheme_name") %||% "hex_color"
  print(tab, ...)
}
#' @export
plot.bayesplot_scheme <- function(x, ...) {
  scheme <- attr(x, "scheme_name") %||% stop("Scheme name not found.")
  .view_scheme(scheme)
}


#' @rdname bayesplot-colors
#' @export
color_scheme_view <- function(scheme) {
  suggested_package("gridExtra")
  if (missing(scheme) || length(scheme) == 1)
    return(.view_scheme(scheme))

  bayesplot_grid(
    plots = lapply(scheme, .view_scheme),
    grid_args = list(ncol = length(scheme))
  )
}



# internal -----------------------------------------------------------------

# plot color scheme
# @param scheme A string (length 1) naming a scheme
.view_scheme <- function(scheme) {
  x <- if (missing(scheme))
    color_scheme_get() else color_scheme_get(scheme)

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
    legend_none() +
    xaxis_text(
      face = "bold",
      margin = margin(t = -3, b = 10),
      angle = 0,
      debug = FALSE
    )
}


# @param scheme A string (length 1) naming a scheme
scheme_from_string <- function(scheme) {
  stopifnot(length(scheme) == 1)
  if (identical(substr(scheme, 1, 4), "mix-")) {
    to_mix <- unlist(strsplit(scheme, split = "-"))[2:3]
    x <- setNames(mixed_scheme(to_mix[1], to_mix[2]), scheme_level_names())
    structure(x, mixed = TRUE, scheme_name = scheme)
  } else {
    scheme <- match.arg(scheme, choices = names(master_color_list))
    x <- prepare_colors(scheme)
    structure(x, mixed = FALSE, scheme_name = scheme)
  }
}

# check if object returned by color_scheme_get is a mixed scheme
# @param x object returned by color_scheme_get
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
  color_vals <- color_scheme_get()[levels]
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

  x <- setNames(as.list(scheme), scheme_level_names())
  attr(x, "scheme_name") <- "custom"
  x
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
  scheme1 <- color_scheme_get(scheme1)
  scheme2 <- color_scheme_get(scheme2)
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
  darkgray =
    list("#bfbfbf", "#999999", "#737373", "#505050", "#383838", "#0d0d0d"),
  gray =
    list("#DFDFDF", "#bfbfbf", "#999999", "#737373", "#505050", "#383838"),
  green =
    list("#d9f2e6", "#9fdfbf", "#66cc99", "#40bf80", "#2d8659", "#194d33"),
  pink =
    list("#dcbccc", "#c799b0", "#b97c9b", "#a25079", "#8f275b", "#7c003e"),
  purple =
    list("#e5cce5", "#bf7fbf", "#a64ca6", "#800080", "#660066", "#400040"),
  red =
    list("#DCBCBC", "#C79999", "#B97C7C", "#A25050", "#8F2727", "#7C0000"),
  teal =
    list("#bcdcdc", "#99c7c7", "#7cb9b9", "#50a2a2", "#278f8f", "#007C7C"),
  yellow =
    list("#fbf3da", "#f8e8b5", "#f5dc90", "#dbc376", "#aa975c", "#7a6c42"),
  viridis =
    list("#FDE725FF", "#7AD151FF", "#22A884FF", "#2A788EFF", "#414487FF", "#440154FF"),
  viridisA =
    list("#FCFDBFFF", "#FE9F6DFF", "#DE4968FF", "#8C2981FF", "#3B0F70FF", "#000004FF"),
  viridisB =
    list("#FCFFA4FF", "#FCA50AFF", "#DD513AFF", "#932667FF", "#420A68FF", "#000004FF"),
  viridisC =
    list("#F0F921FF", "#FCA636FF", "#E16462FF", "#B12A90FF", "#6A00A8FF", "#0D0887FF")
)

# instantiate aesthetics --------------------------------------------------
.bayesplot_aesthetics <- new.env(parent = emptyenv())
.bayesplot_aesthetics$scheme <- list()
color_scheme_set()
