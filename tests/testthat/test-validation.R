context("Get a list of validation rules")

with_mock_api({
  test_that("We can get a list of validation rules from the server", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    datasets <- c("MqNLEXmzIzr", "kkXf2zXqTM0")
    test_vrs <- getValidationRules(d2session = d2_default_session)
    expect_type(test_vrs, "list")
    names_vrs <- c("name", "id", "periodType", "description",
"operator", "leftSide.expression", "leftSide.missingValueStrategy",
"rightSide.expression", "rightSide.missingValueStrategy",
"rightSide.ops", "leftSide.ops")
    expect_setequal(names(test_vrs), names_vrs)

  })
})

context("Correctly validate data with validation rules")
with_mock_api({
  test_that("We can fail a never skip rule with data on both sides", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    datasets <- c("MqNLEXmzIzr", "kkXf2zXqTM0")
    d <- d2Parser(filename = test_config("test-data-validation-simple-fail.csv"),
                type = "csv",
                organisationUnit = "KKFzPM8LoXs",
                dataElementIdScheme = "id",
                orgUnitIdScheme = "id",
                idScheme = "id",
                invalidData = FALSE, d2session = d2_default_session)
    d <- prepDataForValidation(d)
    vr <- getValidationRules(d2session = d2_default_session)
    foo <- evaluateValidation(d$combi, d$value, vr, FALSE)
    expect_false(foo$result)
  })
})

with_mock_api({
  test_that("We can pass a never skip rule with data on both sides", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    datasets <- c("MqNLEXmzIzr", "kkXf2zXqTM0")
    d <- d2Parser(filename = test_config("test-data-validation-simple-pass.csv"),
                type = "csv",
                organisationUnit = "KKFzPM8LoXs",
                dataElementIdScheme = "id",
                orgUnitIdScheme = "id",
                idScheme = "id",
                invalidData = FALSE, d2session = d2_default_session)
    d <- prepDataForValidation(d)
    vr <- getValidationRules(d2session = d2_default_session)
    foo <- evaluateValidation(d$combi, d$value, vr, FALSE)
    expect_true(foo$result)
  })
})

with_mock_api({
  test_that("We can fail a never skip rule with data missing on right", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    datasets <- c("MqNLEXmzIzr", "kkXf2zXqTM0")
    d <- d2Parser(filename = test_config("test-data-validation-never-skip-missing-right.csv"),
                type = "csv",
                organisationUnit = "KKFzPM8LoXs",
                dataElementIdScheme = "id",
                orgUnitIdScheme = "id",
                idScheme = "id",
                invalidData = FALSE, d2session = d2_default_session)
    d <- prepDataForValidation(d)
    vr <- getValidationRules(d2session = d2_default_session)
    foo <- evaluateValidation(d$combi, d$value, vr, FALSE)
    expect_false(foo$result)
    expect_equal(foo$rightSide.expression, 0)
    expect_equal(foo$leftSide.expression, 5)
  })
})

with_mock_api({
  test_that("We can pass a never skip rule with data missing on left", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    datasets <- c("MqNLEXmzIzr", "kkXf2zXqTM0")
    d <- d2Parser(filename = test_config("test-data-validation-never-skip-missing-left.csv"),
                type = "csv",
                organisationUnit = "KKFzPM8LoXs",
                dataElementIdScheme = "id",
                orgUnitIdScheme = "id",
                idScheme = "id",
                invalidData = FALSE,
                d2session = d2_default_session)
    d <- prepDataForValidation(d)
    vr <- getValidationRules(d2session = d2_default_session)
    foo <- evaluateValidation(d$combi, d$value, vr, FALSE)
    expect_true(foo$result)
    expect_equal(foo$rightSide.expression, 6)
    expect_equal(foo$leftSide.expression, 0)
  })
})



with_mock_api({
  test_that("We can fail an exclusive rule with data missing on both sides", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    datasets <- c("MqNLEXmzIzr", "kkXf2zXqTM0")
    d <- d2Parser(filename = test_config("test-data-validation-exclusive-fail.csv"),
                type = "csv",
                organisationUnit = "KKFzPM8LoXs",
                dataElementIdScheme = "id",
                orgUnitIdScheme = "id",
                idScheme = "id",
                invalidData = FALSE,
                d2session = d2_default_session)
    d <- prepDataForValidation(d)
    vr <- getValidationRules(d2session = d2_default_session)
    foo <- evaluateValidation(d$combi, d$value, vr, FALSE)
    foo <- foo %>% dplyr::filter(operator == "|")
    expect_false(foo$result)
    expect_equal(foo$rightSide.expression, 1)
    expect_equal(foo$leftSide.expression, 1)
  })
})


with_mock_api({
  test_that("We can pass an exclusive rule with data missing on one side", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    datasets <- c("MqNLEXmzIzr", "kkXf2zXqTM0")
    d <- d2Parser(filename = test_config("test-data-validation-exclusive-fail.csv"),
                type = "csv",
                organisationUnit = "KKFzPM8LoXs",
                dataElementIdScheme = "id",
                orgUnitIdScheme = "id",
                idScheme = "id",
                invalidData = FALSE,
                d2session = d2_default_session)
    d <- d[1, ]
    d <- prepDataForValidation(d)
    vr <- getValidationRules(d2session = d2_default_session)
    foo <- evaluateValidation(d$combi, d$value, vr, FALSE)
    foo <- foo %>% dplyr::filter(operator == "|")
    expect_true(foo$result)
    expect_equal(foo$rightSide.expression, 0)
    expect_equal(foo$leftSide.expression, 1)
  })
})

with_mock_api({
  test_that("We can return validation rule violations of bulk data", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    datasets <- c("MqNLEXmzIzr", "kkXf2zXqTM0")
    d <- d2Parser(filename = test_config("test-data.csv"),
                type = "csv",
                organisationUnit = "KKFzPM8LoXs",
                dataElementIdScheme = "id",
                orgUnitIdScheme = "id",
                idScheme = "id",
                invalidData = FALSE,
                d2session = d2_default_session)
    datasets <- c("MqNLEXmzIzr", "kkXf2zXqTM0")
    foo <- validateData(d, organisationUnit = "KKFzPM8LoXs", return_violations_only = TRUE,
                      parallel = FALSE, datasets = datasets, d2session = d2_default_session)
    expect_type(foo, "list")
  })
})
