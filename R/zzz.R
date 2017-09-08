.onAttach <- function(...) {
  ver <- utils::packageVersion("bayesplot")
  packageStartupMessage("This is bayesplot version ", ver)
  packageStartupMessage("- Plotting theme set to bayesplot::theme_default()")
  packageStartupMessage("- Online documentation at mc-stan.org/bayesplot")
  ggplot2::theme_set(bayesplot::theme_default())
}
