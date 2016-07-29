.onAttach <- function(...) {
  ver <- utils::packageVersion("bayesplot")
  packageStartupMessage("This is bayesplot version ", ver)
}
