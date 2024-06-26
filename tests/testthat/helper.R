# used to skip visual tests of SVGs on r-devel and r-oldrel
# because they can be slightly different than r-release occasionally
skip_if_not_r_release <- function() {
  testthat::skip_if(isTRUE(Sys.getenv("R_VERSION_TYPE") == "devel"))

  #testthat::skip_if_not(isTRUE(Sys.getenv("R_VERSION_TYPE") == "release"))
}
