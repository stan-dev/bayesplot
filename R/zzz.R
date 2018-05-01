.onAttach <- function(...) {
  ver <- utils::packageVersion("bayesplot")
  packageStartupMessage("This is bayesplot version ", ver)
  packageStartupMessage("- Plotting theme set to bayesplot::theme_default(). Change it with theme_set() or bayesplot_theme_set().")
  packageStartupMessage("- Online documentation at mc-stan.org/bayesplot")
}
