context("Get a list of periods")

with_mock_api({
  test_that("We can get a list of periods", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    test_periods <- getPeriodInfo(d2session = d2_default_session)
    expect_type(test_periods, "list")
    expect_is(test_periods, "data.frame")
    period_map_names <- c("periodid",
                          "iso",
                          "startdate",
                          "enddate",
                          "periodtype")
    expect_named(test_periods, period_map_names, ignore.order = TRUE)
    expect_true(is(test_periods$startdate, "Date"))
    expect_true(is(test_periods$enddate, "Date"))
  })
})
