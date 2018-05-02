.onAttach <- function(...) {
  ver <- utils::packageVersion("bayesplot")
  packageStartupMessage("This is bayesplot version ", ver)
  packageStartupMessage("- Plotting theme set to bayesplot::theme_default(). Change it")
  packageStartupMessage("  with ggplot2::theme_set() or bayesplot::bayesplot_theme_set()")
  packageStartupMessage("- Online documentation at mc-stan.org/bayesplot")
}
