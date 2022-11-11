#' Set, get, or view **bayesplot** color schemes
#'
#' Set, get, or view color schemes. Choose from a preset scheme or create a
#' custom scheme. See the **Available color schemes** section below for a list
#' of available scheme names. The **Custom color schemes** section describes how
#' to specify a custom scheme.
#'
#' @name bayesplot-colors
#' @param scheme For `color_scheme_set()`, either a string naming one of the
#'   available color schemes or a character vector of _exactly six_ colors
#'   specifying a custom scheme.
#'
#'   For `color_scheme_get()`, `scheme` can be missing (to get the
#'   current color scheme) or a string naming one of the preset schemes.
#'
#'   For `color_scheme_view()`, `scheme` can be missing (to use the
#'   current color scheme) or a character vector containing a subset of the
#'   available scheme names.
#'
#'   See the **Available color schemes** section below for a list of available
#'   scheme names. The **Custom color schemes** section describes how to specify
#'   a custom scheme.
#'
#' @return `color_scheme_set()` has the side effect of setting the color scheme
#'   used for plotting. It also returns ([invisibly][base::invisible]) a list of
#'   the hexadecimal color values used in `scheme`.
#'
#'   `color_scheme_get()` returns a list of the hexadecimal color
#'   values (without changing the current scheme). If the `scheme` argument
#'   is not specified the returned values correspond to the current color
#'   scheme. If the optional argument `i` is specified then the returned
#'   list only contains `length(i)` elements.
#'
#'   `color_scheme_view()` returns a ggplot object if only a single scheme is
#'   specified and a gtable object if multiple schemes names are specified.
#'
#' @section Available color schemes: Currently, the available preset color
#'   schemes are:
#'  * `"blue"`, `"brightblue"`
#'  * `"gray"`, `"darkgray"`
#'  * `"green"`
#'  * `"pink"`
#'  * `"purple"`
#'  * `"red"`
#'  * `"teal"`
#'  * `"yellow"`
#'  * [`"viridis"`](https://CRAN.R-project.org/package=viridis), `"viridisA"`,
#'    `"viridisB"`, `"viridisC"`, `"viridisD"`, `"viridisE"`
#'  * `"mix-x-y"`, replacing `x` and `y` with any two of
#'      the scheme names listed above (e.g. "mix-teal-pink", "mix-blue-red",
#'      etc.). The order of `x` and `y` matters, i.e., the color schemes
#'      `"mix-blue-red"` and `"mix-red-blue"` are not identical. There is no
#'      guarantee that every possible mixed scheme will look good with every
#'      possible plot.
#'  * `"brewer-x"`, replacing `x` with the name of a palette available from
#'     [RColorBrewer::brewer.pal()] (e.g., `brewer-PuBuGn`).
#'
#'  If you have a suggestion for a new color scheme please let us know via the
#'  **bayesplot** [issue tracker](https://github.com/stan-dev/bayesplot/issues).
#'
#' @section Custom color schemes: A **bayesplot** color scheme consists of six
#'   colors. To specify a custom color scheme simply pass a character vector
#'   containing either the names of six [colors][grDevices::colors] or six
#'   hexadecimal color values (or a mix of names and hex values). The colors
#'   should be in order from lightest to darkest. See the end of the
#'   **Examples** section for a demonstration.
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
#'
#' ############################
#' ### Mixing color schemes ###
#' ############################
#' color_scheme_set("mix-teal-pink")
#' ppc_stat(y, yrep, stat = "sd") + legend_none()
#' mcmc_areas(x, regex_pars = "beta")
#'
#' ##########################
#' ### ColorBrewer scheme ###
#' ##########################
#' color_scheme_set("brewer-Spectral")
#' color_scheme_view()
#' mcmc_trace(x, pars = "sigma")
#'
#' ###########################
#' ### Custom color scheme ###
#' ###########################
#' orange_scheme <- c("#ffebcc", "#ffcc80",
#'                    "#ffad33", "#e68a00",
#'                    "#995c00", "#663d00")
#' color_scheme_set(orange_scheme)
#' color_scheme_view()
#' mcmc_areas(x, regex_pars = "alpha")
#' mcmc_dens_overlay(x)
#' ppc_stat(y, yrep, stat = "var") + legend_none()
#'
NULL

#' @rdname bayesplot-colors
#' @export
color_scheme_set <- function(scheme = "blue") {
  if (!is.character(scheme)) {
    abort("'scheme' should be a character vector of length 1 or 6.")
  }

  if (length(scheme) == 1) {
    x <- scheme_from_string(scheme)
  } else if (length(scheme) == 6) {
    x <- prepare_custom_colors(scheme)
  } else {
    abort("'scheme' should be a character vector of length 1 or 6.")
  }
  .bayesplot_aesthetics[["scheme"]] <- x
  invisible(x)
}

#' @rdname bayesplot-colors
#' @export
#' @param i For `color_scheme_get()`, an optional subset of the integers from `1`
#'   (lightest) to `6` (darkest) indicating which of the colors in the
#'   scheme to return. If `i` is not specified then all six colors in the
#'   scheme are included.
#'
color_scheme_get <- function(scheme = NULL, i = NULL) {
  if (!is.null(scheme)) {
    scheme <- scheme_from_string(scheme)
  } else {
    x <- .bayesplot_aesthetics$scheme
    scheme <- as.list(x)[scheme_level_names()]
    attr(scheme, "mixed") <- attr(x, "mixed")
    attr(scheme, "scheme_name") <- attr(x, "scheme_name")
  }
  class(scheme) <- c("bayesplot_scheme", "list")

  if (is.null(i)) {
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

#' @rdname bayesplot-colors
#' @export
color_scheme_view <- function(scheme = NULL) {
  if (is.null(scheme) || length(scheme) == 1){
    return(plot_scheme(scheme))
  }
  bayesplot_grid(
    plots = lapply(scheme, plot_scheme),
    grid_args = list(ncol = length(scheme))
  )
}

#' @export
print.bayesplot_scheme <- function(x, ...) {
  tab <- data.frame(unlist(x, use.names = FALSE), stringsAsFactors = FALSE)
  colnames(tab) <- attr(x, "scheme_name") %||% "hex_color"
  print(tab, ...)
}

#' @export
plot.bayesplot_scheme <- function(x, ...) {
  scheme <- attr(x, "scheme_name") %||% abort("Scheme name not found.")
  plot_scheme(scheme)
}


# internal -----------------------------------------------------------------

#' Plot color scheme
#' @noRd
#' @param scheme A string (length 1) naming a scheme. If `NULL` the current
#'   scheme is used.
#' @return A ggplot object.
plot_scheme <- function(scheme = NULL) {
  if (is.null(scheme)) {
    x <- color_scheme_get()
  } else {
    x <- color_scheme_get(scheme)
  }

  color_data <- data.frame(
    name = factor(attr(x, "scheme_name")),
    group = factor(names(x), levels = rev(names(x))),
    value = rep(1, length(x))
  )

  ggplot(color_data, aes(x = .data$name, y = .data$value, fill = .data$group)) +
    geom_bar(
      width = .5,
      stat = "identity",
      color = "white",
      linewidth = 0.1
    ) +
    scale_fill_manual("", values = unlist(x)) +
    theme_void() +
    legend_none() +
    xaxis_text(
      face = "bold",
      margin = margin(t = -3, b = 10, unit = "pt"),
      angle = 0,
      vjust = 1,
      debug = FALSE
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

#' Return a color scheme based on `scheme` argument specified as a string
#' @noRd
#' @param scheme A string (length 1) naming a scheme.
scheme_from_string <- function(scheme) {
  if (identical(substr(scheme, 1, 4), "mix-")) {
    # user specified a mixed scheme (e.g., "mix-blue-red")
    to_mix <- unlist(strsplit(scheme, split = "-"))[2:3]
    x <- set_names(mixed_scheme(to_mix[1], to_mix[2]), scheme_level_names())
    return(structure(x, mixed = TRUE, scheme_name = scheme))
  } else if (identical(substr(scheme, 1, 7), "brewer-")) {
    # user specified a ColorBrewer scheme (e.g., "brewer-Blues")
    if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
      abort("Please install the 'RColorBrewer' package to use a ColorBrewer scheme.")
    }
    clrs <- RColorBrewer::brewer.pal(n = 6, name = gsub("brewer-", "", scheme))
    x <- set_names(as.list(clrs), scheme_level_names())
    return(structure(x, mixed = FALSE, scheme_name = scheme))
  } else {
    # check for scheme in master_color_list
    scheme <- match.arg(scheme, choices = names(master_color_list))
    x <- set_names(master_color_list[[scheme]], scheme_level_names())
    return(structure(x, mixed = FALSE, scheme_name = scheme))
  }
}

# create mixed scheme from two existing schemes
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
  scheme
}

#' Check if object returned by `color_scheme_get()` is a mixed scheme
#' @noRd
#' @param x object returned by `color_scheme_get()`
#' @return T/F
is_mixed_scheme <- function(x) {
  stopifnot(is.list(x))
  isTRUE(attr(x, "mixed"))
}

#' Access a subset of the current scheme colors
#' @noRd
#' @param levels A character vector of level names in `scheme_level_names()`.
#'   The abbreviations `"l", "lh", "m", "mh", "d", "dh"` can also be used
#'   instead of the full names.
#' @return A character vector of color values.
#'
get_color <- function(levels) {
  levels <- full_level_name(levels)
  stopifnot(all(levels %in% scheme_level_names()))
  color_vals <- color_scheme_get()[levels]
  unlist(color_vals, use.names = FALSE)
}

full_level_name <- function(x) {
  map <- c(
    l = "light",
    lh = "light_highlight",
    m = "mid",
    mh = "mid_highlight",
    d = "dark",
    dh = "dark_highlight",
    light = "light",
    light_highlight = "light_highlight",
    mid = "mid",
    mid_highlight = "mid_highlight",
    dark = "dark",
    dark_highlight = "dark_highlight"
  )
  unname(map[x])
}

# Custom color scheme if 6 colors specified
prepare_custom_colors <- function(scheme) {
  if (length(scheme) != 6) {
    abort("Custom color schemes must contain exactly 6 colors.")
  }

  not_found <- character(0)
  for (j in seq_along(scheme)) {
    clr <- scheme[j]
    if (!is_hex_color(clr)  && !clr %in% grDevices::colors()) {
      not_found <- c(not_found, clr)
    }
  }
  if (length(not_found)) {
    abort(paste(
      "Each color must specified as either a hexadecimal color value ",
      "(e.g. '#C79999') or the name of a color (e.g. 'blue'). ",
      "The following provided colors were not found:",
      paste(unlist(not_found), collapse = ", ")
    ))
  }

  x <- set_names(as.list(scheme), scheme_level_names())
  attr(x, "scheme_name") <- "custom"
  x
}

is_hex_color <- function(x) {
  if (!identical(substr(x, 1, 1), "#")) {
    return(FALSE)
  }
  isTRUE(nchar(x) == 7)
}


# master color list -------------------------------------------------------
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
  orange =
    list("#fecba2", "#feb174", "#fe8a2f", "#e47115", "#b15810", "#7f3f0c"),
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
    list("#F0F921FF", "#FCA636FF", "#E16462FF", "#B12A90FF", "#6A00A8FF", "#0D0887FF"),
  # popular form of viridis is viridis option D
  viridisD =
    list("#FDE725FF", "#7AD151FF", "#22A884FF", "#2A788EFF", "#414487FF", "#440154FF"),
  viridisE =
    list("#FFEA46FF", "#CBBA69FF", "#958F78FF", "#666970FF", "#31446BFF", "#00204DFF")
)

# instantiate aesthetics --------------------------------------------------
.bayesplot_aesthetics <- new.env(parent = emptyenv())
.bayesplot_aesthetics$scheme <- list()
color_scheme_set()
