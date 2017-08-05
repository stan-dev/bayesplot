.onAttach <- function(...) {
  ver <- utils::packageVersion("bayesplot")
  packageStartupMessage(
    "This is bayesplot version ", ver, ". ",
    "Plotting theme set to bayesplot::theme_default()."
  )
  ggplot2::theme_set(bayesplot::theme_default())
}
