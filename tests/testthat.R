library(testthat)
library(bayesplot)

if (!grepl("^sparc",  R.version$platform))
  test_check("bayesplot")
