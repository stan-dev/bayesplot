library(testthat)
library(bayesplot)

Sys.unsetenv("R_TESTS")
test_check("bayesplot")
# if (!grepl("^sparc",  R.version$platform))
#   test_check("bayesplot")
