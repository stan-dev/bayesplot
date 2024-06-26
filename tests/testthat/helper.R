# Used if necessary to skip visual tests of SVGs (using vdiffr)
# on different R versions because they can be slightly different

# R_VERSION_TYPE is set in the R-CMD-check.yaml GitHub Actions workflow file

on_r_devel <- function() {
  isTRUE(Sys.getenv("R_VERSION_TYPE") == "devel")
}
on_r_oldrel <- function() {
  isTRUE(Sys.getenv("R_VERSION_TYPE") == "oldrel")
}

skip_on_r_devel <- function() {
  testthat::skip_if(on_r_devel())
}
skip_on_r_oldrel <- function() {
  testthat::skip_if(on_r_oldrel())
}
skip_if_not_on_r_release <- function() {
  skip_on_r_devel()
  skip_on_r_oldrel()
}
