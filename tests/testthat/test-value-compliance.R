context("Check value type compliance")

with_mock_api({
  test_that("We can identify bad values for integer data elements", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    datasets <- c("MqNLEXmzIzr", "kkXf2zXqTM0")
    d <- d2Parser(filename = test_config("test-data-bad-value.csv"),
                type = "csv",
                organisationUnit = "KKFzPM8LoXs",
                dataElementIdScheme = "id",
                orgUnitIdScheme = "id",
                idScheme = "id",
                invalidData = FALSE, d2session = d2_default_session)
    d <- checkValueTypeCompliance(d, d2session = d2_default_session)
    expect_equal(NROW(d$tests$value_compliance), 3)

  })
})


with_mock_api({
  test_that("We can identify bad values for true only data elements", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    d <- list()
    d$info$messages <- datimvalidation:::MessageQueue()
    # nolint start
    d$data$import <- tibble::tribble(
    ~dataElement,~period,~orgUnit,~categoryOptionCombo,~attributeOptionCombo,~value,
    "mFvVvrRvZgo","2019Q3","LnGaK6y98gC","HllvX50cXC0","X8hrDf6bLDC","TRUE",
    "mFvVvrRvZgo","2019Q3","LnGaK6y98gC","HllvX50cXC0","X8hrDf6bLDC","Yes",
    "mFvVvrRvZgo","2019Q3","LnGaK6y98gC","HllvX50cXC0","X8hrDf6bLDC","2",
    "mFvVvrRvZgo","2019Q3","LnGaK6y98gC","HllvX50cXC0","X8hrDf6bLDC","FALSE",
    "mFvVvrRvZgo","2019Q3","LnGaK6y98gC","HllvX50cXC0","X8hrDf6bLDC","No",
    "mFvVvrRvZgo","2019Q3","LnGaK6y98gC","HllvX50cXC0","X8hrDf6bLDC","-1",
    "mFvVvrRvZgo","2019Q3","LnGaK6y98gC","HllvX50cXC0","X8hrDf6bLDC","False",
    "mFvVvrRvZgo","2019Q3","LnGaK6y98gC","HllvX50cXC0","X8hrDf6bLDC","false",
    "mFvVvrRvZgo","2019Q3","LnGaK6y98gC","HllvX50cXC0","X8hrDf6bLDC","0",
    "mFvVvrRvZgo","2019Q3","LnGaK6y98gC","HllvX50cXC0","X8hrDf6bLDC","True",
    "mFvVvrRvZgo","2019Q3","LnGaK6y98gC","HllvX50cXC0","X8hrDf6bLDC","true",
    "mFvVvrRvZgo","2019Q3","LnGaK6y98gC","HllvX50cXC0","X8hrDf6bLDC","1" )
    # nolint end
    d <- checkValueTypeCompliance(d, d2session  = d2_default_session)
    expect_equal(NROW(d$tests$value_compliance), 8)
  })
})


with_mock_api({
  test_that("We can identify bad values for option set elements", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    datasets <- c("MqNLEXmzIzr", "kkXf2zXqTM0")
    d <- d2Parser(filename = test_config("test-data-bad-value-option-sets.csv"),
                type = "csv",
                organisationUnit = "KKFzPM8LoXs",
                dataElementIdScheme = "id",
                orgUnitIdScheme = "id",
                idScheme = "id",
                invalidData = FALSE,
                d2session  = d2_default_session)
    d <- checkValueTypeCompliance(d, d2session = d2_default_session)
    expect_equal(NROW(d$tests$invalid_option_set_values), 1)
  })
})

with_mock_api({
  test_that("We can flag negative values in non-dedupe mechanisms", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    d <- d2Parser(filename = test_config("test-data-neg-values.csv"),
                type = "csv",
                organisationUnit = "KKFzPM8LoXs",
                dataElementIdScheme = "id",
                orgUnitIdScheme = "id",
                idScheme = "id",
                invalidData = FALSE,
                d2session  = d2_default_session)

    d <- checkNegativeValues(d, d2session  = d2_default_session)
    expect_equal(NROW(d$tests$negative_values), 2)
  })
})

with_mock_api({
  test_that("We import CSV files which contain spaces in the fields", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    d <- d2Parser(filename = test_config("test-data-with-spaces.csv"),
                type = "csv",
                organisationUnit = "KKFzPM8LoXs",
                dataElementIdScheme = "id",
                orgUnitIdScheme = "id",
                idScheme = "id",
                invalidData = FALSE,
                d2session  = d2_default_session)
    expect_equal(NROW(d), 2)
  })
})
