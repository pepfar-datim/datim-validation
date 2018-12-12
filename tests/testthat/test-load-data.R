context("Parse CSV data")

test_that("We can read a CSV file coded with IDs", {
  config <- LoadConfigFile(test_config("test-config.json"))
  expect_type(config,"list")
  d<-d2Parser(filename=test_config("test-data.csv"),
              type="csv",
              organisationUnit = NA,
              dataElementIdScheme = "id",
              orgUnitIdScheme = "id",
              idScheme = "id",
              invalidData = FALSE)
  expect_type(d,"list")
  expect_is(d,"data.frame")
  d_names<-c("dataElement","period","orgUnit","categoryOptionCombo","attributeOptionCombo","value")
  expect_identical(names(d),d_names)
})

context("Parse JSON data")
test_that("We can read a JSON file coded with IDs", {
  config <- LoadConfigFile(test_config("test-config.json"))
  expect_type(config,"list")
  d<-d2Parser(filename=test_config("test-json.json"),
              type="json",
              organisationUnit = NA,
              dataElementIdScheme = "id",
              orgUnitIdScheme = "id",
              idScheme = "id",
              invalidData = FALSE)
  expect_type(d,"list")
  expect_is(d,"data.frame")
  d_names<-c("dataElement","period","orgUnit","categoryOptionCombo","attributeOptionCombo","value")
  expect_identical(names(d),d_names)
})


context("Parse XML data")
test_that("We can read an XML file coded with IDs", {
  config <- LoadConfigFile(test_config("test-config.json"))
  expect_type(config,"list")
  d<-d2Parser(filename=test_config("test-xml.xml"),
              type="xml",
              organisationUnit = NA,
              dataElementIdScheme = "id",
              orgUnitIdScheme = "id",
              idScheme = "id",
              invalidData = FALSE)
  expect_type(d,"list")
  expect_is(d,"data.frame")
  d_names<-c("dataElement","period","orgUnit","categoryOptionCombo","attributeOptionCombo","value")
  expect_identical(names(d),d_names)
})

context("Can error on a wrong file type")

test_that("We can create an error on a bad file type", {
  config <- LoadConfigFile(test_config("test-config.json"))
  expect_type(config,"list")
  expect_error(d2Parser(filename=test_config("test-xml.xml"),
              type="foo",
              organisationUnit = NA,
              dataElementIdScheme = "id",
              orgUnitIdScheme = "id",
              idScheme = "id",
              invalidData = FALSE))
})