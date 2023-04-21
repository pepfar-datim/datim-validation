context("Check data element cadence")

with_mock_api({
  test_that("We can identify data elements submitted for the wrong period", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))


    d <- list()
    d$info$messages <- MessageQueue()

    foo <- checkDataElementCadence(d, d2session = d2_default_session)
    expect_true(any(grepl("No data elements or periods found", foo$info$messages$message)))


    d <- list()
    d$info$messages <- MessageQueue()
    d$data$import <- tibble::tribble(
      ~dataElement,  ~period,
      "gGkFUE1ob8y", "2023Q1"
    )

    foo <- checkDataElementCadence(d, d2session = d2_default_session)
    expect_true(any(grepl("No invalid data element/period combinations found", foo$info$messages$message)))

    #"uid": "ZvZawiqn7cd",
    #"shortName": "OVC_SCHATT (N, DSD_NARRATIVE)

    d <- list()
    d$info$messages <- MessageQueue()
    d$data$import <- tibble::tribble(
      ~dataElement,  ~period,
      "ZvZawiqn7cd", "2023Q1"
    )

    foo <- checkDataElementCadence(d, d2session = d2_default_session)
    expect_true(any(grepl("ERROR! Invalid data element", foo$info$messages$message)))

    expect_equal("ZvZawiqn7cd", foo$tests$invalid_des_periods$dataElement[1])



  })
})

with_mock_api({
test_that("We can get data element cadence maps", {
  loginToDATIM(config_path = test_config("test-config.json"))
  expect_true(exists("d2_default_session"))
  foo <-
    getDataElementCadenceMapForPeriod("2023Q1", d2_default_session)
  expect_named(foo, c("uid", "shortName", "period"))
  expect_true(all(foo$period == "2023Q1"))
})
})

with_mock_api({
  test_that("We can return null if the cadence map exists", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    expect_warning(foo <-
                     getDataElementCadenceMapForPeriod("1997Q1", d2_default_session))

    expect_null(foo)
  })

})
