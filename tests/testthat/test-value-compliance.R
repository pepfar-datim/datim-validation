context("Check value type compliance")

with_mock_api({
  test_that("We can identify bad values for integer data elements", {
    my_creds <- DHISLogin$new(test_config("test-config.json"))
    my_creds$maxCacheAge<-NULL
    my_creds$handle<-NULL
    datasets<-c("MqNLEXmzIzr","kkXf2zXqTM0")
    d<-d2Parser(filename=test_config("test-data-bad-value.csv"),
                type="csv",
                organisationUnit = "KKFzPM8LoXs",
                dataElementIdScheme = "id",
                orgUnitIdScheme = "id",
                idScheme = "id",
                invalidData = FALSE, creds = my_creds)
    expect_equal(NROW(checkValueTypeCompliance(d, creds = my_creds)), 3) 
    d$value<-"5"
    expect_equal(checkValueTypeCompliance(d,creds = my_creds), TRUE)
  })
})


with_mock_api({
  test_that("We can identify bad values for true only data elements", {
    my_creds <- DHISLogin$new(test_config("test-config.json"))
    my_creds$maxCacheAge<-NULL
    my_creds$handle<-NULL
    datasets<-c("tz1bQ3ZwUKJ")
    d<-d2Parser(filename=test_config("test-data-bad-true-only-value.csv"),
                type="csv",
                organisationUnit = "KKFzPM8LoXs",
                dataElementIdScheme = "id",
                orgUnitIdScheme = "id",
                idScheme = "id",
                invalidData = FALSE,
                creds = my_creds)
    expect_equal(NROW(checkValueTypeCompliance(d, creds = my_creds)), 9) 
  })
})


with_mock_api({
  test_that("We can identify bad values for option set elements", {
    my_creds <- DHISLogin$new(test_config("test-config.json"))
    my_creds$maxCacheAge<-NULL
    my_creds$handle<-NULL
    datasets<-c("MqNLEXmzIzr","kkXf2zXqTM0")
    d<-d2Parser(filename=test_config("test-data-bad-value-option-sets.csv"),
                type="csv",
                organisationUnit = "KKFzPM8LoXs",
                dataElementIdScheme = "id",
                orgUnitIdScheme = "id",
                idScheme = "id",
                invalidData = FALSE,
                creds = my_creds)
    expect_equal(NROW(checkValueTypeCompliance(d,creds = my_creds)), 1)
  })
})

with_mock_api({
  test_that("We can flag negative values in non-dedupe mechanisms", {
    my_creds <- DHISLogin$new(test_config("test-config.json"))
    my_creds$maxCacheAge<-NULL
    my_creds$handle<-NULL
    d<-d2Parser(filename=test_config("test-data-neg-values.csv"),
                type="csv",
                organisationUnit = "KKFzPM8LoXs",
                dataElementIdScheme = "id",
                orgUnitIdScheme = "id",
                idScheme = "id",
                invalidData = FALSE,
                creds = my_creds)
    expect_warning(foo<-checkNegativeValues(data = d, creds = my_creds))
    expect_equal(NROW(foo), 2)
  })
})

with_mock_api({
  test_that("We import CSV files which contain spaces in the fields", {
    my_creds <- DHISLogin$new(test_config("test-config.json"))
    my_creds$maxCacheAge<-NULL
    my_creds$handle<-NULL
    d<-d2Parser(filename=test_config("test-data-with-spaces.csv"),
                type="csv",
                organisationUnit = "KKFzPM8LoXs",
                dataElementIdScheme = "id",
                orgUnitIdScheme = "id",
                idScheme = "id",
                invalidData = FALSE,
                creds = my_creds)
    expect_equal(NROW(d), 2)
  })
})