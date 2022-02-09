context("Get a list of data element orgunit associations")

with_mock_api({
  test_that("We can get a list of valid data elements for organisation units", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    datasets <- c("MqNLEXmzIzr", "kkXf2zXqTM0")
    test_ous_des <- getDataElementsOrgunits(organisationUnit = "KKFzPM8LoXs",
                                            datasets = datasets,
                                            d2session = d2_default_session)
    expect_type(test_ous_des, "list")
    expect_equal(length(test_ous_des), 2)
    expect_setequal(unlist(rlist::list.select(test_ous_des, dataset)), datasets)
    expect_equal(names(test_ous_des[[1]][1]), "dataset")
    expect_setequal(names(test_ous_des[[1]][[2]]), c("ous", "des"))
  })
})


context("Flag invalid data element orgunit combinations")

with_mock_api({
  test_that("We flag invalid data element / orgunit associations in the data", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    d <- d2Parser(filename = test_config("test-data-bad-des-ous.csv"),
                type = "csv",
                organisationUnit = "KKFzPM8LoXs",
                dataElementIdScheme = "id",
                orgUnitIdScheme = "id",
                idScheme = "id",
                invalidData = FALSE,
                d2session = d2_default_session)
  datasets <- c("MqNLEXmzIzr", "kkXf2zXqTM0")
  expect_warning(test_data <- checkDataElementOrgunitValidity(d,
                                                              "KKFzPM8LoXs",
                                                              datasets,
                                                              d2session = d2_default_session))
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
    expect_warning(test_data <- checkDataElementDisaggValidity(d,
                                                             datasets = datasets,
                                                             return_violations = TRUE,
                                                             d2session = d2_default_session))
    expect_equal(NROW(test_data), 1)
    expect_equal(test_data$storedby[1], "BAD")
  })})

