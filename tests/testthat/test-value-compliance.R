context("Check value type compliance")

with_mock_api({
  test_that("We can identify bad values for integer data elements", {
    config <- LoadConfigFile(test_config("test-config.json"))
    options("maxCacheAge"=NULL)
    datasets<-c("MqNLEXmzIzr","kkXf2zXqTM0")
    d<-d2Parser(filename=test_config("test-data-bad-value.csv"),
                type="csv",
                organisationUnit = "KKFzPM8LoXs",
                dataElementIdScheme = "id",
                orgUnitIdScheme = "id",
                idScheme = "id",
                invalidData = FALSE)
    expect_equal(NROW(checkValueTypeCompliance(d)), 3) 
    d$value<-5
    expect_equal(NROW(checkValueTypeCompliance(d)), 0)
  })
})


with_mock_api({
  test_that("We can identify bad values for option set elements", {
    config <- LoadConfigFile(test_config("test-config.json"))
    options("maxCacheAge"=NULL)
    datasets<-c("MqNLEXmzIzr","kkXf2zXqTM0")
    d<-d2Parser(filename=test_config("test-data-bad-value-option-sets.csv"),
                type="csv",
                organisationUnit = "KKFzPM8LoXs",
                dataElementIdScheme = "id",
                orgUnitIdScheme = "id",
                idScheme = "id",
                invalidData = FALSE)
    expect_equal(NROW(checkValueTypeCompliance(d)), 1)
  })
})
