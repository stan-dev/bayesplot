# Set, get, or view **bayesplot** color schemes

Set, get, or view color schemes. Choose from a preset scheme or create a
custom scheme. See the **Available color schemes** section below for a
list of available scheme names. The **Custom color schemes** section
describes how to specify a custom scheme.

## Usage

``` r
color_scheme_set(scheme = "blue")

color_scheme_get(scheme = NULL, i = NULL)

color_scheme_view(scheme = NULL)
```

## Arguments

- scheme:

  For `color_scheme_set()`, either a string naming one of the available
  color schemes or a character vector of *exactly six* colors specifying
  a custom scheme.

  For `color_scheme_get()`, `scheme` can be missing (to get the current
  color scheme) or a string naming one of the preset schemes.

  For `color_scheme_view()`, `scheme` can be missing (to use the current
  color scheme) or a character vector containing a subset of the
  available scheme names.

  See the **Available color schemes** section below for a list of
  available scheme names. The **Custom color schemes** section describes
  how to specify a custom scheme.

- i:

  For `color_scheme_get()`, an optional subset of the integers from `1`
  (lightest) to `6` (darkest) indicating which of the colors in the
  scheme to return. If `i` is not specified then all six colors in the
  scheme are included.

## Value

`color_scheme_set()` has the side effect of setting the color scheme
used for plotting. It also returns
([invisibly](https://rdrr.io/r/base/invisible.html)) a list of the
hexadecimal color values used in `scheme`.

`color_scheme_get()` returns a list of the hexadecimal color values
(without changing the current scheme). If the `scheme` argument is not
specified the returned values correspond to the current color scheme. If
the optional argument `i` is specified then the returned list only
contains `length(i)` elements.

`color_scheme_view()` returns a ggplot object if only a single scheme is
specified and a gtable object if multiple schemes names are specified.

## Available color schemes

Currently, the available preset color schemes are:

- `"blue"`, `"brightblue"`

- `"gray"`, `"darkgray"`

- `"green"`

- `"pink"`

- `"purple"`

- `"red"`

- `"teal"`

- `"yellow"`

- [`"viridis"`](https://CRAN.R-project.org/package=viridis),
  `"viridisA"`, `"viridisB"`, `"viridisC"`, `"viridisD"`, `"viridisE"`

- `"mix-x-y"`, replacing `x` and `y` with any two of the scheme names
  listed above (e.g. "mix-teal-pink", "mix-blue-red", etc.). The order
  of `x` and `y` matters, i.e., the color schemes `"mix-blue-red"` and
  `"mix-red-blue"` are not identical. There is no guarantee that every
  possible mixed scheme will look good with every possible plot.

- `"brewer-x"`, replacing `x` with the name of a palette available from
  [`RColorBrewer::brewer.pal()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html)
  (e.g., `brewer-PuBuGn`).

If you have a suggestion for a new color scheme please let us know via
the **bayesplot** [issue
tracker](https://github.com/stan-dev/bayesplot/issues).

## Custom color schemes

A **bayesplot** color scheme consists of six colors. To specify a custom
color scheme simply pass a character vector containing either the names
of six [colors](https://rdrr.io/r/grDevices/colors.html) or six
hexadecimal color values (or a mix of names and hex values). The colors
should be in order from lightest to darkest. See the end of the
**Examples** section for a demonstration.

## See also

[`theme_default()`](https://mc-stan.org/bayesplot/dev/reference/theme_default.md)
for the default ggplot theme used by **bayesplot** and
[`bayesplot_theme_set()`](https://mc-stan.org/bayesplot/dev/reference/bayesplot_theme_get.md)
to change it.

## Examples

``` r
color_scheme_set("blue")
color_scheme_view()


color_scheme_get()
#>      blue
#> 1 #d1e1ec
#> 2 #b3cde0
#> 3 #6497b1
#> 4 #005b96
#> 5 #03396c
#> 6 #011f4b
color_scheme_get(i = c(3, 5)) # 3rd and 5th colors only
#> $mid
#> [1] "#6497b1"
#> 
#> $dark
#> [1] "#03396c"
#> 

color_scheme_get("brightblue")
#>   brightblue
#> 1    #cce5ff
#> 2    #99cbff
#> 3    #4ca5ff
#> 4    #198bff
#> 5    #0065cc
#> 6    #004c99
color_scheme_view("brightblue")


# compare multiple schemes
color_scheme_view(c("pink", "gray", "teal"))

color_scheme_view(c("viridis", "viridisA", "viridisB", "viridisC"))


color_scheme_set("pink")
x <- example_mcmc_draws()
mcmc_intervals(x)


color_scheme_set("teal")
color_scheme_view()

mcmc_intervals(x)


color_scheme_set("red")
mcmc_areas(x, regex_pars = "beta")


color_scheme_set("purple")
color_scheme_view()

y <- example_y_data()
yrep <- example_yrep_draws()
ppc_stat(y, yrep, stat = "mean") + legend_none()
#> Note: in most cases the default test statistic 'mean' is too weak to detect anything of interest.
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.


############################
### Mixing color schemes ###
############################
color_scheme_set("mix-teal-pink")
ppc_stat(y, yrep, stat = "sd") + legend_none()
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.

mcmc_areas(x, regex_pars = "beta")


##########################
### ColorBrewer scheme ###
##########################
color_scheme_set("brewer-Spectral")
color_scheme_view()

mcmc_trace(x, pars = "sigma")


###########################
### Custom color scheme ###
###########################
orange_scheme <- c("#ffebcc", "#ffcc80",
                   "#ffad33", "#e68a00",
                   "#995c00", "#663d00")
color_scheme_set(orange_scheme)
color_scheme_view()

mcmc_areas(x, regex_pars = "alpha")

mcmc_dens_overlay(x)

ppc_stat(y, yrep, stat = "var") + legend_none()
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.

```
