context("Parse CSV data")

with_mock_api({
test_that("We can read a CSV file coded with IDs", {
  config <- LoadConfigFile(test_config("test-config.json"))
  options("maxCacheAge"=NULL)
  expect_type(config,"list")
  d<-d2Parser(filename=test_config("test-data.csv"),
              type="csv",
              organisationUnit = "KKFzPM8LoXs",
              dataElementIdScheme = "id",
              orgUnitIdScheme = "id",
              idScheme = "id",
              invalidData = FALSE)
  expect_type(d,"list")
  expect_is(d,"data.frame")
  d_names<-c("dataElement","period","orgUnit","categoryOptionCombo","attributeOptionCombo","value")
  expect_identical(names(d),d_names)
})})

with_mock_api({
test_that("We can read a headerless CSV file coded with IDs", {
  config <- LoadConfigFile(test_config("test-config.json"))
  options("maxCacheAge"=NULL)
  expect_type(config,"list")
  d<-d2Parser(filename=test_config("test-data-no-header.csv"),
              type="csv",
              organisationUnit = "KKFzPM8LoXs",
              dataElementIdScheme = "id",
              orgUnitIdScheme = "id",
              idScheme = "id",
              invalidData = FALSE,
              csv_header = FALSE)
  expect_type(d,"list")
  expect_is(d,"data.frame")
  d_names<-c("dataElement","period","orgUnit","categoryOptionCombo","attributeOptionCombo","value")
  expect_identical(names(d),d_names)
  expect_equal(NROW(d),5)
})})

with_mock_api({
test_that("We can error when mechanisms are not coded properly", {
  config <- LoadConfigFile(test_config("test-config.json"))
  options("maxCacheAge"=NULL)
  expect_type(config,"list")
  expect_error(d2Parser(filename=test_config("test-data-bad-mechs.csv"),
              type="csv",
              organisationUnit = "KKFzPM8LoXs",
              dataElementIdScheme = "id",
              orgUnitIdScheme = "id",
              idScheme = "id",
              invalidData = FALSE,
              csv_header = FALSE))
})})

context("Parse JSON data")

with_mock_api({
test_that("We can read a JSON file coded with IDs", {
  config <- LoadConfigFile(test_config("test-config.json"))
  options("maxCacheAge"=NULL)
  expect_type(config,"list")
  d<-d2Parser(filename=test_config("test-json.json"),
              type="json",
              organisationUnit = "KKFzPM8LoXs",
              dataElementIdScheme = "id",
              orgUnitIdScheme = "id",
              idScheme = "id",
              invalidData = FALSE)
  expect_type(d,"list")
  expect_is(d,"data.frame")
  d_names<-c("dataElement","period","orgUnit","categoryOptionCombo","attributeOptionCombo","value")
  expect_identical(names(d),d_names)
})})

with_mock_api({
  test_that("We can error when the JSON attributes are not correct", {
    config <- LoadConfigFile(test_config("test-config.json"))
    options("maxCacheAge"=NULL)
    expect_type(config,"list")
    expect_error(d2Parser(filename=test_config("test-json-bad-attributes.json"),
                          type="json",
                          organisationUnit = "KKFzPM8LoXs",
                          dataElementIdScheme = "id",
                          orgUnitIdScheme = "id",
                          idScheme = "id",
                          invalidData = FALSE),"JSON attributes must be one of the following")
  })})


context("Parse XML data")

with_mock_api({
test_that("We can read an XML file coded with IDs", {
  config <- LoadConfigFile(test_config("test-config.json"))
  options("maxCacheAge"=NULL)
  expect_type(config,"list")
  d<-d2Parser(filename=test_config("test-xml.xml"),
              type="xml",
              organisationUnit = "KKFzPM8LoXs",
              dataElementIdScheme = "id",
              orgUnitIdScheme = "id",
              idScheme = "id",
              invalidData = FALSE)
  expect_type(d,"list")
  expect_is(d,"data.frame")
  d_names<-c("dataElement","period","orgUnit","categoryOptionCombo","attributeOptionCombo","value")
  expect_identical(names(d),d_names)
})})


with_mock_api({
  test_that("We can error when the XML attributes are not correct", {
    config <- LoadConfigFile(test_config("test-config.json"))
    options("maxCacheAge"=NULL)
    expect_type(config,"list")
    expect_error(d2Parser(filename=test_config("test-xml-bad-attributes.xml"),
                type="xml",
                organisationUnit = "KKFzPM8LoXs",
                dataElementIdScheme = "id",
                orgUnitIdScheme = "id",
                idScheme = "id",
                invalidData = FALSE),"XML attributes must be one of the following")
  })})

context("Can error on a wrong file type")

with_mock_api({
test_that("We can create an error on a bad file type", {
  config <- LoadConfigFile(test_config("test-config.json"))
  options("maxCacheAge"=NULL)
  expect_type(config,"list")
  expect_error(d2Parser(filename=test_config("test-xml.xml"),
              type="foo",
              organisationUnit = "KKFzPM8LoXs",
              dataElementIdScheme = "id",
              orgUnitIdScheme = "id",
              idScheme = "id",
              invalidData = FALSE))
})})

context("Can error on a wrong period identifier")

with_mock_api({
  test_that("We can create an error on a file with a bad period", {
    config <- LoadConfigFile(test_config("test-config.json"))
    options("maxCacheAge"=NULL)
    expect_type(config,"list")
    d<-d2Parser(filename=test_config("test-data-bad-periods.csv"),
                          type="csv",
                          organisationUnit = "KKFzPM8LoXs",
                          dataElementIdScheme = "id",
                          orgUnitIdScheme = "id",
                          idScheme = "id",
                          invalidData = FALSE)
   expect_error(checkPeriodIdentifiers(d))
  })})

context("Can error on a bad mechanism/period association")

with_mock_api({
  test_that("We can create an error for an invalid/mechanism period association", {
    config <- LoadConfigFile(test_config("test-config.json"))
    options("maxCacheAge"=NULL)
    expect_type(config,"list")
    d<-d2Parser(filename=test_config("test-data-bad-periods-mechanisms.csv"),
                type="csv",
                organisationUnit = "KKFzPM8LoXs",
                dataElementIdScheme = "id",
                orgUnitIdScheme = "id",
                idScheme = "id",
                invalidData = FALSE)
    expect_error(checkMechanismPeriodValidity(d),"Invalid mechanisms for periods found: 1973Oct")
  })})

context("Can error on an invalid orgunit UID")

with_mock_api({
  test_that("We can create an error for an invalid organisation unit identifier", {
    config <- LoadConfigFile(test_config("test-config.json"))
    options("maxCacheAge"=NULL)
    expect_type(config,"list")
    expect_error(d2Parser(filename=test_config("test-data-bad-ou-uid.csv"),
                type="csv",
                organisationUnit = "KKFzPM8LoXs",
                dataElementIdScheme = "id",
                orgUnitIdScheme = "id",
                idScheme = "id",
                invalidData = FALSE), "The following org unit identifiers could not be found:SiuNE0ywCW4")
  })})
