# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(Algorithm)

test_check("Algorithm")
test_that("Product of algorithm is not negative")
#passes, basic check
expect_that(a>=0, is_true())
#passes, another basic check for Dijkstra
expect_that(w>=0, is_true())
#Not yet passes, potentially needs a warning for not to have negative divisors
expect_that(a<=0, gives_warning())
