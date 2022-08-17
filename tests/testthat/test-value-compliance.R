context("Check value type compliance")

with_mock_api({
  test_that("We can identify bad values for integer data elements", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    datasets<-c("MqNLEXmzIzr","kkXf2zXqTM0")
    d<-d2Parser(filename=test_config("test-data-bad-value.csv"),
                type="csv",
                organisationUnit = "KKFzPM8LoXs",
                dataElementIdScheme = "id",
                orgUnitIdScheme = "id",
                idScheme = "id",
                invalidData = FALSE, d2session  = d2_default_session)
    expect_equal(NROW(checkValueTypeCompliance(d, d2session  = d2_default_session)), 3)
    d$value<-"5"
    expect_equal(checkValueTypeCompliance(d,d2session  = d2_default_session), TRUE)
  })
})


with_mock_api({
  test_that("We can identify bad values for true only data elements", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    datasets<-c("tz1bQ3ZwUKJ")
    d<-d2Parser(filename=test_config("test-data-bad-true-only-value.csv"),
                type="csv",
                organisationUnit = "KKFzPM8LoXs",
                dataElementIdScheme = "id",
                orgUnitIdScheme = "id",
                idScheme = "id",
                invalidData = FALSE,
                d2session  = d2_default_session)
    expect_equal(NROW(checkValueTypeCompliance(d, d2session  = d2_default_session)), 9)
  })
})


with_mock_api({
  test_that("We can identify bad values for option set elements", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    datasets<-c("MqNLEXmzIzr","kkXf2zXqTM0")
    d<-d2Parser(filename=test_config("test-data-bad-value-option-sets.csv"),
                type="csv",
                organisationUnit = "KKFzPM8LoXs",
                dataElementIdScheme = "id",
                orgUnitIdScheme = "id",
                idScheme = "id",
                invalidData = FALSE,
                d2session  = d2_default_session)
    expect_equal(NROW(checkValueTypeCompliance(d,d2session  = d2_default_session)), 1)
  })
})

with_mock_api({
  test_that("We can flag negative values in non-dedupe mechanisms", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    d<-d2Parser(filename=test_config("test-data-neg-values.csv"),
                type="csv",
                organisationUnit = "KKFzPM8LoXs",
                dataElementIdScheme = "id",
                orgUnitIdScheme = "id",
                idScheme = "id",
                invalidData = FALSE,
                d2session  = d2_default_session)
    expect_warning(foo<-checkNegativeValues(data = d, d2session  = d2_default_session))
    expect_equal(NROW(foo), 2)
  })
})

with_mock_api({
  test_that("We import CSV files which contain spaces in the fields", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    d<-d2Parser(filename=test_config("test-data-with-spaces.csv"),
                type="csv",
                organisationUnit = "KKFzPM8LoXs",
                dataElementIdScheme = "id",
                orgUnitIdScheme = "id",
                idScheme = "id",
                invalidData = FALSE,
                d2session  = d2_default_session)
    expect_equal(NROW(d), 2)
  })
})
