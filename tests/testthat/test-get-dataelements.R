context("Get a list of data elements")

with_mock_api({
  test_that("We can get an dataelement map", {
    my_creds <- DHISLogin$new(test_config("test-config.json"))
    my_creds$maxCacheAge<-NULL
    my_creds$handle<-NULL
    test_des<-getDataElementMap(creds = my_creds)
    expect_type(test_des,"list")
    expect_is(test_des,"data.frame")
    de_map_names<-c("name","id","code","shortName","optionSet.id","zeroIsSignificant","valueType")
    expect_setequal(names(test_des),de_map_names)
  })
})