# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(DATA501Assign1)

test_check("DATA501Assign1")
source("Ass-1-1.R")

context("Testing the function")
## Invalid inputs testing
test_that("The function give valid erros for the inputs provided",
          {
            expect_error(shapiro_wilk_test(c("2","3","4","5")), "\n Data must be numeric")
            expect_error(shapiro_wilk_test(c(2,3,4,NA)), "\n Data contains NA values")
            expect_error(shapiro_wilk_test(c(2,3,4,5),"ad"), "Check the optional argument. By default, it's FALSE and can be set to
         TRUE")
          })


# Valid inputs testing
test_that("shapiro_wilk_test handles normal data correctly", {
  test_data <- rnorm(100)
  expect_silent(result <- shapiro_wilk_test(test_data))
  expect_is(result, "numeric")
})

test_that("shapiro_wilk_test handles uniform data correctly", {
  test_data <- runif(100)
  expect_silent(result <- shapiro_wilk_test(test_data))
  expect_is(result, "numeric")
})

test_that("shapiro_wilk_test works fine with custom data",
          {
            expect_silent(shapiro_wilk_test(c(2,33,4,5),TRUE)) # With optional argument
            expect_silent(shapiro_wilk_test(c(2,33,4,5))) # Without optional argument
          })
