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
    expect_is(d, "data.frame")
    d_names <- c("dataElement",
                 "period",
                 "orgUnit",
                 "categoryOptionCombo",
                 "attributeOptionCombo",
                 "value")
    expect_identical(names(d), d_names)
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
                  csv_header = FALSE,
                  d2session = d2_default_session)
    expect_type(d, "list")
    expect_is(d, "data.frame")
    d_names <- c("dataElement",
                 "period",
                 "orgUnit",
                 "categoryOptionCombo",
                 "attributeOptionCombo",
                 "value")
    expect_identical(names(d), d_names)
    expect_equal(NROW(d), 5)
  })})

with_mock_api({
  test_that("We can error when mechanisms are not coded properly", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    expect_warning(d2Parser(filename = test_config("test-data-bad-mechs.csv"),
                            type = "csv",
                            organisationUnit = "KKFzPM8LoXs",
                            dataElementIdScheme = "id",
                            orgUnitIdScheme = "id",
                            idScheme = "id",
                            invalidData = FALSE,
                            csv_header = FALSE,
                            d2session = d2_default_session))
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
    expect_is(d, "data.frame")
    d_names <- c("dataElement",
                 "period",
                 "orgUnit",
                 "categoryOptionCombo",
                 "attributeOptionCombo",
                 "value")
    expect_identical(names(d), d_names)
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
    expect_is(d, "data.frame")
    d_names <- c("dataElement",
                 "period",
                 "orgUnit",
                 "categoryOptionCombo",
                 "attributeOptionCombo",
                 "value")
    expect_identical(names(d), d_names)
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
    expect_error(checkPeriodIdentifiers(d))
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
      expect_warning(
        bad_mechs <- checkMechanismValidity(d,
                                            organisationUnit = "KKFzPM8LoXs",
                                            return_violations = TRUE,
                                            d2session = d2_default_session),
        "Invalid mechanisms found!")
      expect_type(bad_mechs, "list")
      expect_is(bad_mechs, "data.frame")
      bad_mechs_names <- c("attributeOptionCombo",
                           "period",
                           "startDate",
                           "endDate",
                           "periodType",
                           "code",
                           "startDate_mech",
                           "endDate_mech",
                           "is_valid")
      expect_setequal(names(bad_mechs), bad_mechs_names)
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
      expect_warning(
        bad_mechs <- checkMechanismValidity(d,
                                            organisationUnit = "KKFzPM8LoXs",
                                            return_violations = FALSE,
                                            d2session = d2_default_session),
        "Invalid mechanisms found!")
      expect_null(bad_mechs)
    })})


context("Can error on an invalid orgunit UID")

with_mock_api({
  test_that(
    "We can create an error for an invalid organisation unit identifier", {
      loginToDATIM(config_path = test_config("test-config.json"))
      expect_true(exists("d2_default_session"))
      expect_warning(
        foo <- d2Parser(filename = test_config("test-data-bad-ou-uid.csv"),
                        type = "csv",
                        organisationUnit = "KKFzPM8LoXs",
                        dataElementIdScheme = "id",
                        orgUnitIdScheme = "id",
                        idScheme = "id",
                        invalidData = FALSE, d2session  = d2_default_session),
        "The following org unit identifiers could not be found:SiuNE0ywCW4")
      expect_false(foo$is_valid)
      expect_type(foo, "list")
    })})

context("Can error on an invalid data element UID")

with_mock_api({
  test_that(
    "We can create an error for an invalid data element identifier", {
      loginToDATIM(config_path = test_config("test-config.json"))
      expect_true(exists("d2_default_session"))
      expect_warning(
        foo <- d2Parser(filename = test_config("test-data-bad-de-uid.csv"),
                        type = "csv",
                        organisationUnit = "KKFzPM8LoXs",
                        dataElementIdScheme = "id",
                        orgUnitIdScheme = "id",
                        idScheme = "id",
                        invalidData = FALSE, d2session  = d2_default_session),
        "The following data element identifiers could not be found:SiuNE0ywCW4")
      expect_false(foo$is_valid)
      expect_type(foo, "list")
    })})

context("Can error on an invalid attribute option combo UID")

with_mock_api({
  test_that(
    "We can create an error for an invalid attribute option combo identifier", {
      loginToDATIM(config_path = test_config("test-config.json"))
      expect_true(exists("d2_default_session"))
      expect_warning(
        foo <- d2Parser(filename = test_config("test-data-bad-acoc-uid.csv"),
                        type = "csv",
                        organisationUnit = "KKFzPM8LoXs",
                        dataElementIdScheme = "id",
                        orgUnitIdScheme = "id",
                        idScheme = "id",
                        invalidData = FALSE, d2session = d2_default_session),
        "The following attribute option combo identifiers could not be found:SiuNE0ywCW4") #nolint
      expect_false(foo$is_valid)
      expect_type(foo, "list")
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
