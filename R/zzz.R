.onAttach <- function(...) {
  ver <- utils::packageVersion("bayesplot")
  packageStartupMessage("This is bayesplot version ", ver)
  packageStartupMessage("- Online documentation and vignettes at mc-stan.org/bayesplot")
  packageStartupMessage("- bayesplot theme set to bayesplot::theme_default()")
  packageStartupMessage("   * Does _not_ affect other ggplot2 plots")
  packageStartupMessage("   * See ?bayesplot_theme_set for details on theme setting")
}
