
context("Flag invalid data element orgunit combinations")

with_mock_api({
  test_that("Can get a list of data elements and orgunits for a dataset", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    foo <- getDataElementOrgunitMap(dataset = "cw2T5eAHxzW", d2session = d2_default_session)
    expect_type(foo,"list")
    expect_length(foo, 2L)
    expect_named(foo, expected = c("dataSetElements","organisationUnits"), ignore.order = TRUE)
    expect_true(is.data.frame(foo$dataSetElements))
    expect_named(foo$dataSetElements, expected = c("dataElement.id"))
    expect_true(is.data.frame(foo$organisationUnits))
    expect_named(foo$organisationUnits, expected = c("id"))

  })

})

with_mock_api({
  test_that("Can error if more than one dataset is specified", {
    expect_error(getDataElementOrgunitMap(dataset=c("abc123","def456"), d2session = training))
  })
})

with_mock_api({
  test_that("Can error if more than one orgunit exists", {
    d <- tibble::tribble(
      ~orgUnit, ~dataElement,
      "abc123", "zzzz123",
      "dev456", "zzzz123"
    )
    de_map <- list()

    expect_error(validateOrgunitDataElements(d, de_map))
  })

})


with_mock_api({
  test_that("Can return all invalid data", {
    d <- tibble::tribble(
      ~orgUnit, ~dataElement,
      "abc123", "zzzz123",
      "abc123", "zzzz123"
    )
    #This orgunit is not present in the data element orgunit map
    de_map <- list(dataSetElements = data.frame(dataElement.id = c("zzzz123")),
                   organisationUnits = data.frame(id = c("xxxxx1234" )))

    test_data <- validateOrgunitDataElements(d, de_map)
    expect_identical(d,test_data)
  })

})

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

  expect_warning(test_data <- checkDataElementOrgunitValidity(d, datasets,return_violations = FALSE, d2session = d2_default_session))
  expect_false(test_data)
})})

with_mock_api({
  test_that("We pass invalid data element / orgunit associations in the data",
            {
              loginToDATIM(config_path = test_config("test-config.json"))
              expect_true(exists("d2_default_session"))

              d <- tibble::tribble(
                ~ dataElement, ~ orgUnit,
                "Kk4CdspETNQ", "OZyRtJPWHii",
                "Kk4CdspETNQ", "oRU10tDCoJe",
                "Kk4CdspETNQ", "b8lwniNwcPK"
              )

              datasets <- c("cw2T5eAHxzW")
              test_data <-
                checkDataElementOrgunitValidity(d, datasets, d2session = d2_default_session)
              expect_equal(NROW(test_data), 0)
              expect_named(test_data,c("dataElement","orgUnit"))

              test_data <-
                checkDataElementOrgunitValidity(d,
                                                datasets,
                                                return_violations = FALSE,
                                                d2session = d2_default_session)
              expect_true(test_data)
            })
})


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
