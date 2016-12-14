library(testthat)
library(bayesplot)

if (!grepl("^sparc|apple",  R.version$platform))
  test_check("bayesplot")
