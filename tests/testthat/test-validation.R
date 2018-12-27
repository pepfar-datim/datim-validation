context("Get a list of validation rules")

with_mock_api({
  test_that("We can get a list of validation rules from the server", {
    config <- LoadConfigFile(test_config("test-config.json"))
    options("maxCacheAge"=NULL)
    datasets<-c("MqNLEXmzIzr","kkXf2zXqTM0")
    test_vrs<-getValidationRules()
    expect_type(test_vrs,"list")
    names_vrs<-c("name","id","periodType","description",
"operator","leftSide.expression","leftSide.missingValueStrategy",
"rightSide.expression","rightSide.missingValueStrategy",
"rightSide.ops","leftSide.ops")
    expect_setequal(names(test_vrs),names_vrs)

  })
})

context("Correctly validate data with validation rules")
with_mock_api({
  test_that("We can fail a never skip rule with data on both sides", {
    config <- LoadConfigFile(test_config("test-config.json"))
    options("maxCacheAge"=NULL)
    datasets<-c("MqNLEXmzIzr","kkXf2zXqTM0")
    d<-d2Parser(filename=test_config("test-data-validation-simple-fail.csv"),
                type="csv",
                organisationUnit = "KKFzPM8LoXs",
                dataElementIdScheme = "id",
                orgUnitIdScheme = "id",
                idScheme = "id",
                invalidData = FALSE)
    d<-prepDataForValidation(d)
    vr<-getValidationRules()
    foo<-evaluateValidation(d$combi,d$value,vr,FALSE)
    expect_false(foo$result)
  })
})

with_mock_api({
  test_that("We can pass a never skip rule with data on both sides", {
    config <- LoadConfigFile(test_config("test-config.json"))
    options("maxCacheAge"=NULL)
    datasets<-c("MqNLEXmzIzr","kkXf2zXqTM0")
    d<-d2Parser(filename=test_config("test-data-validation-simple-pass.csv"),
                type="csv",
                organisationUnit = "KKFzPM8LoXs",
                dataElementIdScheme = "id",
                orgUnitIdScheme = "id",
                idScheme = "id",
                invalidData = FALSE)
    d<-prepDataForValidation(d)
    vr<-getValidationRules()
    foo<-evaluateValidation(d$combi,d$value,vr,FALSE)
    expect_true(foo$result)
  })
})

with_mock_api({
  test_that("We can fail a never skip rule with data missing on right", {
    config <- LoadConfigFile(test_config("test-config.json"))
    options("maxCacheAge"=NULL)
    datasets<-c("MqNLEXmzIzr","kkXf2zXqTM0")
    d<-d2Parser(filename=test_config("test-data-validation-never-skip-missing-right.csv"),
                type="csv",
                organisationUnit = "KKFzPM8LoXs",
                dataElementIdScheme = "id",
                orgUnitIdScheme = "id",
                idScheme = "id",
                invalidData = FALSE)
    d<-prepDataForValidation(d)
    vr<-getValidationRules()
    foo<-evaluateValidation(d$combi,d$value,vr,FALSE)
    expect_false(foo$result)
    expect_equal(foo$rightSide.expression,0)
    expect_equal(foo$leftSide.expression,5)
  })
})

with_mock_api({
  test_that("We can pass a never skip rule with data missing on left", {
    config <- LoadConfigFile(test_config("test-config.json"))
    options("maxCacheAge"=NULL)
    datasets<-c("MqNLEXmzIzr","kkXf2zXqTM0")
    d<-d2Parser(filename=test_config("test-data-validation-never-skip-missing-left.csv"),
                type="csv",
                organisationUnit = "KKFzPM8LoXs",
                dataElementIdScheme = "id",
                orgUnitIdScheme = "id",
                idScheme = "id",
                invalidData = FALSE)
    d<-prepDataForValidation(d)
    vr<-getValidationRules()
    foo<-evaluateValidation(d$combi,d$value,vr,FALSE)
    expect_true(foo$result)
    expect_equal(foo$rightSide.expression,6)
    expect_equal(foo$leftSide.expression,0)
  })
})