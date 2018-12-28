context("Get a list of category option combos")

with_mock_api({
  test_that("We can get an category option combo map", {
    config <- LoadConfigFile(test_config("test-config.json"))
    options("maxCacheAge"=NULL)
    test_cocs<-getCategoryOptionCombosMap()
    expect_type(test_cocs,"list")
    expect_is(test_cocs,"data.frame")
    coc_map_names<-c("name","id","code","shortName")
    expect_setequal(names(test_cocs),coc_map_names)
    col_types<-lapply(test_cocs, typeof)
    expect_identical(col_types$code,"character")
    expect_identical(col_types$id,"character")
    expect_identical(col_types$name,"character")
    expect_identical(col_types$shortName,"character")
  })
})