context("Get a list of validation rules")

with_mock_api({
  test_that("We can get a listing of validation rules", {
    config <- LoadConfigFile(test_config("test-config.json"))
    options("maxCacheAge"=NULL)
    print(getOption("maxCacheAge"))
    test_vrs<-getValidationRules()
    expect_type(test_vrs,"list")
    expect_is(test_vrs,"data.frame")
    vr_map_names<-c("name","id","periodType","description","operator",
                    "leftSide.expression","leftSide.missingValueStrategy",
                    "rightSide.expression","rightSide.missingValueStrategy",
                    "rightSide.ops","leftSide.ops")
    expect_setequal(names(test_vrs),vr_map_names)
  })
})