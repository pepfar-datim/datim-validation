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

