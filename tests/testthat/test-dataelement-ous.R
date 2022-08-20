
context("Flag invalid data element orgunit combinations")

with_mock_api({
  test_that("We flag invalid data element / orgunit associations in the data", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    # nolint start
    d <- tibble::tribble(
      ~dataElement, ~period,~orgUnit,~categoryOptionCombo,~attributeOptionCombo,~value,~comment,
      "MMODRH694Pn","2017Q1","LnGaK6y98gC","pPoX6WdTN1o","WQa4uNduUe","10","GOOD",
      "tG7ocyZ8kVA","2017Q1","RQCy4nM3afc","HllvX50cXC0","WQa4uNduUev","5","GOOD",
      "qeS0bazg6IW","2017Q1","KKFzPM8LoX7","HllvX50cXC0","WQa4uNduUev","20","BAD"
    )
    # nolint end
  datasets <- c("MqNLEXmzIzr", "kkXf2zXqTM0")
  expect_warning(test_data <- checkDataElementOrgunitValidity(d, datasets, d2session = d2_default_session))
  expect_equal(NROW(test_data), 1)
})})


context("Flag invalid data element disagg combinations")


with_mock_api({
  test_that("We flag invalid data element / disagg associations in the data", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    d <- d2Parser(filename = test_config("test-data-bad-de-disagg.csv"),
                  type = "csv",
                  organisationUnit = "KKFzPM8LoXs",
                  dataElementIdScheme = "id",
                  orgUnitIdScheme = "id",
                  idScheme = "id",
                  invalidData = FALSE,
                  d2session = d2_default_session)
    datasets <- c("i29foJcLY9Y", "STL4izfLznL")
    expect_warning(test_data <-
                     checkDataElementDisaggValidity(
                       d,
                       datasets = datasets,
                       return_violations = TRUE,
                       d2session = d2_default_session))
    expect_equal(NROW(test_data), 1)
    expect_equal(test_data$storedby[1], "BAD")
  })})
