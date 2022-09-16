context("Parse periods")
test_that("Paramater cannot be blank", {

  expect_error(getPeriodFromISO())
})

test_that("We can parse a daily period", {

  test_date <- getPeriodFromISO("20171213")
  test_result <- data.frame(iso = "20171213",
                          startDate = as.Date("2017-12-13"),
                          endDate = as.Date("2017-12-13"),
                          periodType = "Daily",
                          stringsAsFactors = FALSE)
  expect_identical(test_date, test_result)
})

test_that("Invalid daily period produces error", {

  expect_warning(foo <- getPeriodFromISO("20170229"))
  expect_null(foo)
})


test_that("We can parse a weekly period", {

  test_date <- getPeriodFromISO("2017W14")
  test_result <- data.frame(iso = "2017W14",
                          startDate = as.Date("2017-04-03"),
                          endDate = as.Date("2017-04-09"),
                          periodType = "Weekly",
                          stringsAsFactors = FALSE)
  expect_identical(test_date, test_result)
})

test_that("Invalid weekly period produces error", {
  expect_warning(foo <- getPeriodFromISO("2017W54"))
  expect_null(foo)
  skip("Upstream bug. 2017W53 does not exist.")
  expect_warning(getPeriodFromISO("2017W53"))
})

test_that("We can parse a monthly period", {

  test_date <- getPeriodFromISO("201812")
  test_result <- data.frame(iso = "201812",
                          startDate = as.Date("2018-12-01"),
                          endDate = as.Date("2018-12-31"),
                          periodType = "Monthly",
                          stringsAsFactors = FALSE)
  expect_identical(test_date, test_result)
})

test_that("Invalid monthly period produces error", {
  expect_warning(getPeriodFromISO("201713"))
  expect_warning(getPeriodFromISO("20171"))
})


test_that("We can parse a yearly period", {

  test_date <- getPeriodFromISO("2018")
  test_result <- data.frame(iso = "2018",
                          startDate = as.Date("2018-01-01"),
                          endDate = as.Date("2018-12-31"),
                          periodType = "Yearly",
                          stringsAsFactors = FALSE)
  expect_identical(test_date, test_result)
})

test_that("We can parse a quarterly period", {

  test_date <- getPeriodFromISO("2018Q4")
  test_result <- data.frame(iso = "2018Q4",
                          startDate = as.Date("2018-10-01"),
                          endDate = as.Date("2018-12-31"),
                          periodType = "Quarterly",
                          stringsAsFactors = FALSE)
  expect_identical(test_date, test_result)
})

test_that("We can error a bad quarterly period", {
  expect_warning(getPeriodFromISO("2018Q0"))
  expect_warning(getPeriodFromISO("2018Q5"))
})


test_that("We can parse a financial Oct period", {

  test_date <- getPeriodFromISO("2018Oct")
  test_result <- data.frame(iso = "2018Oct",
                          startDate = as.Date("2018-10-01"),
                          endDate = as.Date("2019-09-30"),
                          periodType = "FinancialOct",
                          stringsAsFactors = FALSE)
  expect_identical(test_date, test_result)
})

test_that("We can error on a bad period", {
  d <- list()
  d$info$messages <- MessageQueue()
  d$data$import <- data.frame(period = c("2017Q1", "2017Q2", "2017Q5"))

  expect_warning(d <- checkPeriodIdentifiers(d), "Invalid quarter specified in  2017Q5")
  expect_equal(NROW(d$info$messages), 1L)
})

test_that("We can error on a missing period", {
  d <- list()
  d$info$messages <- MessageQueue()
  d$data$import <- data.frame(period = c("2017Q1", "2017Q2", NA))
  expect_error(d <- checkPeriodIdentifiers(d))
})


test_that("We get true on good periods", {
  d <- as.data.frame(matrix(NA, ncol = 1, nrow = 7))
  names(d) <- c("period")
  d$data$import <- data.frame(period =  c("2018Q3", "2017Q1", "2017Q2",
                "2017Q4", "2017", "2017Oct", "2017W2"))
  expect_silent(d_new <- checkPeriodIdentifiers(d))
  expect_identical(d, d_new)
  }
  )
