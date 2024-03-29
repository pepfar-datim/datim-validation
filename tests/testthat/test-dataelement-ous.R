
context("Flag invalid data element orgunit combinations")

with_mock_api({
  test_that("Can get a list of data elements and orgunits for a dataset", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    foo <- getDataElementDetailsMap(dataset = "cw2T5eAHxzW", d2session = d2_default_session)
    expect_type(foo, "list")
    expect_length(foo, 3L)
    expect_named(foo, expected = c("des", "acocs", "ous"), ignore.order = TRUE)
    expect_true(is.vector(foo$des))
    expect_true(is.vector(foo$ous))
    expect_true(is.vector(foo$acocs))

  })

})

with_mock_api({
  test_that("Can error if more than one dataset is specified", {
    expect_error(getDataElementDetailsMap(dataset = c("abc123", "def456"), d2session = training))
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
    de_map <- list(list(des = c("zzzz123"),
                   ous = c("xxxxx1234")))

    test_data <- validateOrgunitDataElements(d, de_map)
    expect_identical(d, test_data)
  })

})

with_mock_api({
  test_that("We can flag invalid data element / orgunit associations in the data", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    # nolint start
    d <- list()
    d$info$messages <- MessageQueue()

    d$data$import <- tibble::tribble(
      ~dataElement, ~period,~orgUnit,~categoryOptionCombo,~attributeOptionCombo,~value,~comment,
      "MMODRH694Pn","2017Q1","OZyRtJPWHii","pPoX6WdTN1o","WQa4uNduUe","10","GOOD",
      "tG7ocyZ8kVA","2017Q1","oRU10tDCoJe","HllvX50cXC0","WQa4uNduUev","5","GOOD",
      "qeS0bazg6IW","2017Q1","badOU12345","HllvX50cXC0","WQa4uNduUev","20","BAD"
    )
    # nolint end
  datasets <- c("MqNLEXmzIzr")
  d <- checkDataElementOrgunitValidity(d, datasets, d2session = d2_default_session)
  expect_equal(NROW(d$tests$invalid_des_ous), 1)
  expect_named(d$tests$invalid_des_ous, c("dataElement", "orgUnit"), ignore.order = TRUE)

})})

with_mock_api({
  test_that("We pass valid data element / orgunit associations in the data",
            {
              loginToDATIM(config_path = test_config("test-config.json"))
              expect_true(exists("d2_default_session"))
              d <- list()
              d$info$messages <- MessageQueue()

              d$data$import  <- tibble::tribble(
                ~ dataElement, ~ orgUnit,
                "Kk4CdspETNQ", "OZyRtJPWHii",
                "Kk4CdspETNQ", "oRU10tDCoJe",
                "Kk4CdspETNQ", "b8lwniNwcPK"
              )

              datasets <- c("cw2T5eAHxzW")
              test_data <-
                checkDataElementOrgunitValidity(d, datasets, d2session = d2_default_session)
              expect_null(d$tests$invalid_des_ous)

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
  d <- checkDataElementDisaggValidity(
                       d,
                       datasets = datasets,
                       d2session = d2_default_session)
    expect_equal(NROW(d$tests$data_des_cocs_bad), 1)
    expect_equal(d$tests$data_des_cocs_bad$storedby[1], "BAD")
  })})
