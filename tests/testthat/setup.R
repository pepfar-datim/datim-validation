library(httptest)
library(datimutils)

test_config <- function(fname) {
  rprojroot::find_testthat_root_file("config", fname)
}
