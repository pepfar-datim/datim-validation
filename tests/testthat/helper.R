library(httptest)

test_config <- function(fname) rprojroot::find_testthat_root_file("config", fname)

test_that("We can fail the build", {
  
  expect_error(NA })
