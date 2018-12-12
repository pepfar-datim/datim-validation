context("Get a list of periods")

with_mock_api({
  test_that("We can get a list of periods", {
    config <- LoadConfigFile(test_config("test-config.json"))
    options("maxCacheAge"=NULL)
    test_periods<-getPeriodInfo()
    expect_type(test_periods,"list")
    expect_is(test_periods,"data.frame")
    period_map_names<-c("periodid","iso","startdate","enddate","periodtype")
    expect_setequal(names(test_periods),period_map_names)
    expect_true(is(test_periods$startdate,"Date"))
    expect_true(is(test_periods$enddate,"Date"))
  })
})