library(testthat)
library(bayesplot)

Sys.unsetenv("R_TESTS")
if (!grepl("^sparc",  R.version$platform))
  test_check("bayesplot")
