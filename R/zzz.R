.onAttach <- function(...) {
  ver <- utils::packageVersion("bayesplot")
  packageStartupMessage(
    "This is bayesplot version ", ver, ". ",
    "Setting ggplot2 theme to bayesplot::theme_default()."
  )
  ggplot2::theme_set(bayesplot::theme_default())
}
