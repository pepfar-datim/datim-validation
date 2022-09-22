context("Parse CSV data")

with_mock_api({
  test_that("We can read a CSV file coded with IDs", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    d <- d2Parser(filename = test_config("test-data.csv"),
                  type = "csv",
                  organisationUnit = "KKFzPM8LoXs",
                  dataElementIdScheme = "id",
                  orgUnitIdScheme = "id",
                  idScheme = "id",
                  invalidData = FALSE,
                  d2session = d2_default_session)
    expect_type(d, "list")
    expect_named(d, c("info", "data"), ignore.order = TRUE)
    expect_named(d$data, c("parsed", "import"), ignore.order = TRUE)
    expect_type(d$info, "list")
    info_names <-  c("filename",
                     "datastream",
                     "organisationUnit",
                     "type",
                     "dataElementIdScheme",
                     "orgUnitIdScheme",
                     "idScheme",
                     "invalidData",
                     "hasHeader",
                     "isoPeriod",
                     "messages",
                     "has_error",
                     "datasets")
    expect_named(d$info, info_names, ignore.order = TRUE)
    expect_is(d$data$import, "data.frame")
    d_names <- c("dataElement",
                 "period",
                 "orgUnit",
                 "categoryOptionCombo",
                 "attributeOptionCombo",
                 "value")
    expect_named(d$data$parsed, d_names, ignore.order = TRUE)
    expect_named(d$data$import, d_names, ignore.order = TRUE)
  })})

with_mock_api({
  test_that("We can read a headerless CSV file coded with IDs", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    d <- d2Parser(filename = test_config("test-data-no-header.csv"),
                  type = "csv",
                  organisationUnit = "KKFzPM8LoXs",
                  dataElementIdScheme = "id",
                  orgUnitIdScheme = "id",
                  idScheme = "id",
                  invalidData = FALSE,
                  hasHeader = FALSE,
                  d2session = d2_default_session)


    expect_equal(NROW(d$data$import), 5)
  })})

with_mock_api({
  test_that("We can error when mechanisms are not coded properly", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
     expect_warning(d <- d2Parser(filename = test_config("test-data-bad-mechs.csv"),
                            type = "csv",
                            organisationUnit = "KKFzPM8LoXs",
                            dataElementIdScheme = "id",
                            orgUnitIdScheme = "id",
                            idScheme = "id",
                            invalidData = FALSE,
                            hasHeader = TRUE,
                            d2session = d2_default_session))
     expect_equal(NROW(d$tests$acoc_check), 3)
  })})

context("Parse JSON data")

with_mock_api({
  test_that("We can read a JSON file coded with IDs", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    d <- d2Parser(filename = test_config("test-json.json"),
                  type = "json",
                  organisationUnit = "KKFzPM8LoXs",
                  dataElementIdScheme = "id",
                  orgUnitIdScheme = "id",
                  idScheme = "id",
                  invalidData = FALSE,
                  d2session = d2_default_session)
    expect_type(d, "list")
    expect_is(d$data$import, "data.frame")
    d_names <- c("dataElement",
                 "period",
                 "orgUnit",
                 "categoryOptionCombo",
                 "attributeOptionCombo",
                 "value")
    expect_named(d$data$import, d_names)
    expect_equal(NROW(d$data$import), 1000)
  })})

with_mock_api({
  test_that("We can error when the JSON attributes are not correct", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    expect_error(
      d2Parser(filename = test_config("test-json-bad-attributes.json"),
               type = "json",
               organisationUnit = "KKFzPM8LoXs",
               dataElementIdScheme = "id",
               orgUnitIdScheme = "id",
               idScheme = "id",
               invalidData = FALSE, d2session = d2_default_session),
      "JSON attributes must be one of the following")
  })})


context("Parse XML data")

with_mock_api({
  test_that("We can read an XML file coded with IDs", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    d <- d2Parser(filename = test_config("test-xml.xml"),
                  type = "xml",
                  organisationUnit = "KKFzPM8LoXs",
                  dataElementIdScheme = "id",
                  orgUnitIdScheme = "id",
                  idScheme = "id",
                  invalidData = FALSE, d2session = d2_default_session)
    expect_type(d, "list")
    expect_is(d$data$import, "data.frame")
    d_names <- c("dataElement",
                 "period",
                 "orgUnit",
                 "categoryOptionCombo",
                 "attributeOptionCombo",
                 "value")
    expect_identical(names(d$data$import), d_names)
    expect_equal(NROW(d$data$import), 4)
  })})


with_mock_api({
  test_that("We can error when the XML attributes are not correct", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    expect_error(d2Parser(filename = test_config("test-xml-bad-attributes.xml"),
                          type = "xml",
                          organisationUnit = "KKFzPM8LoXs",
                          dataElementIdScheme = "id",
                          orgUnitIdScheme = "id",
                          idScheme = "id",
                          invalidData = FALSE, d2session = d2_default_session),
                 "XML attributes must be one of the following")
  })})

context("Can error on a wrong file type")

with_mock_api({
  test_that("We can create an error on a bad file type", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    expect_error(d2Parser(filename = test_config("test-xml.xml"),
                          type = "foo",
                          organisationUnit = "KKFzPM8LoXs",
                          dataElementIdScheme = "id",
                          orgUnitIdScheme = "id",
                          idScheme = "id",
                          invalidData = FALSE, d2session = d2_default_session))
  })})

context("Can error on a wrong period identifier")

with_mock_api({
  test_that("We can create an error on a file with a bad period", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    d <- d2Parser(filename = test_config("test-data-bad-periods.csv"),
                  type = "csv",
                  organisationUnit = "KKFzPM8LoXs",
                  dataElementIdScheme = "id",
                  orgUnitIdScheme = "id",
                  idScheme = "id",
                  invalidData = FALSE, d2session = d2_default_session)
    d <- expect_warning(checkPeriodIdentifiers(d))
    expect_equal(length(d$tests$bad_periods), 2L)
  })})

context("Can return bad mechanism/period association")

with_mock_api({
  test_that(
    "We can create an warning for an invalid/mechanism period association", {
      loginToDATIM(config_path = test_config("test-config.json"))
      expect_true(exists("d2_default_session"))
      d <- d2Parser(
        filename = test_config("test-data-bad-periods-mechanisms.csv"),
        type = "csv",
        organisationUnit = "KKFzPM8LoXs",
        dataElementIdScheme = "id",
        orgUnitIdScheme = "id",
        idScheme = "id",
        invalidData = FALSE, d2session = d2_default_session)

      d <- checkMechanismValidity(d,
                                            organisationUnit = "KKFzPM8LoXs",
                                            return_violations = TRUE,
                                            d2session = d2_default_session)

      expect_is(d$tests$invalid_mechanisms, "data.frame")
      bad_mechs_names <- c("attributeOptionCombo",
                           "period",
                           "startDate",
                           "endDate",
                           "periodType",
                           "code",
                           "startDate_mech",
                           "endDate_mech",
                           "is_valid")
      expect_setequal(names(d$tests$invalid_mechanisms), bad_mechs_names)
      expect_equal(NROW(d$tests$invalid_mechanisms), 4L)
    })})

context("Can warn on bad mechanism/period associations")

with_mock_api({
  test_that(
    "We can create an warning for an invalid/mechanism period association", {
      loginToDATIM(config_path = test_config("test-config.json"))
      expect_true(exists("d2_default_session"))
      d <- d2Parser(
        filename = test_config("test-data-bad-periods-mechanisms.csv"),
        type = "csv",
        organisationUnit = "KKFzPM8LoXs",
        dataElementIdScheme = "id",
        orgUnitIdScheme = "id",
        idScheme = "id",
        invalidData = FALSE,
        d2session  = d2_default_session)
      d <- checkMechanismValidity(d,
                                  organisationUnit = "KKFzPM8LoXs",
                                  return_violations = TRUE,
                                  d2session = d2_default_session)
      expect_is(d$tests$invalid_mechanisms, "data.frame")
      bad_mechs_names <- c("attributeOptionCombo",
                           "period",
                           "startDate",
                           "endDate",
                           "periodType",
                           "code",
                           "startDate_mech",
                           "endDate_mech",
                           "is_valid")
      expect_setequal(names(d$tests$invalid_mechanisms), bad_mechs_names)
      expect_equal(NROW(d$tests$invalid_mechanisms), 4L)
    })})


context("Can error on an invalid orgunit UID")

with_mock_api({
  test_that(
    "We can create an error for an invalid organisation unit identifier", {
      loginToDATIM(config_path = test_config("test-config.json"))
      expect_true(exists("d2_default_session"))
      expect_warning(
        d <- d2Parser(filename = test_config("test-data-bad-ou-uid.csv"),
                        type = "csv",
                        organisationUnit = "KKFzPM8LoXs",
                        dataElementIdScheme = "id",
                        orgUnitIdScheme = "id",
                        idScheme = "id",
                        invalidData = FALSE, d2session  = d2_default_session))
      expect_is(d$tests$orgunit_check, "character")
      expect_equal(length(d$tests$orgunit_check), 1L)
    })})

context("Can error on an invalid data element UID")

with_mock_api({
  test_that(
    "We can create an error for an invalid data element identifier", {
      loginToDATIM(config_path = test_config("test-config.json"))
      expect_true(exists("d2_default_session"))
      expect_warning(
        d <- d2Parser(filename = test_config("test-data-bad-de-uid.csv"),
                      type = "csv",
                      organisationUnit = "KKFzPM8LoXs",
                      dataElementIdScheme = "id",
                      orgUnitIdScheme = "id",
                      idScheme = "id",
                      invalidData = FALSE, d2session  = d2_default_session)
      )

      expect_is(d$tests$data_element_check, "character")
      expect_identical(d$tests$data_element_check, "SiuNE0ywCW4")
      expect_equal(length(d$tests$data_element_check), 1L)

    })})

context("Can error on an invalid attribute option combo UID")

with_mock_api({
  test_that(
    "We can create an error for an invalid attribute option combo identifier", {
      loginToDATIM(config_path = test_config("test-config.json"))
      expect_true(exists("d2_default_session"))
      expect_warning(
        d <- d2Parser(filename = test_config("test-data-bad-acoc-uid.csv"),
                        type = "csv",
                        organisationUnit = "KKFzPM8LoXs",
                        dataElementIdScheme = "id",
                        orgUnitIdScheme = "id",
                        idScheme = "id",
                        invalidData = FALSE, d2session = d2_default_session))
      expect_is(d$tests$acoc_check, "character")
      expect_identical(d$tests$acoc_check, "SiuNE0ywCW4")
      expect_equal(length(d$tests$acoc), 1L)
    })})

context("Can warn on a missing values")

with_mock_api({
  test_that("Can warn on a missing data value", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    expect_warning(
      foo <- d2Parser(filename = test_config("test-data-missing-value.csv"),
                      type = "csv",
                      organisationUnit = "KKFzPM8LoXs",
                      dataElementIdScheme = "id",
                      orgUnitIdScheme = "id",
                      idScheme = "id",
                      invalidData = FALSE, d2session = d2_default_session),
      "1 rows are incomplete. Please check your file to ensure its correct.")
  })})
