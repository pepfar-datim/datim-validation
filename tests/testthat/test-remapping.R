context("Remap category option combos")

with_mock_api({
  test_that("We can remap from id to name", {
    config <- LoadConfigFile(test_config("test-config.json"))
    options("maxCacheAge"=NULL)
    cocs_uids<-c("EOuF74nArxp","fIB4ouSgQJ4")
    coc_names<-remapCategoryOptionCombos(cocs_uids,"id","name")
    expect_setequal(c("Positive","Negative"),coc_names)
  })
})


with_mock_api({
  test_that("We can remap from id to shortName", {
    config <- LoadConfigFile(test_config("test-config.json"))
    options("maxCacheAge"=NULL)
    cocs_uids<-c("EOuF74nArxp","fIB4ouSgQJ4")
    coc_names<-remapCategoryOptionCombos(cocs_uids,"id","shortName")
    expect_setequal(c("Positive","Negative"),coc_names)
  })
})


with_mock_api({
  test_that("We can remap from code to shortName", {
    config <- LoadConfigFile(test_config("test-config.json"))
    options("maxCacheAge"=NULL)
    cocs_code<-c("00000")
    coc_id<-remapCategoryOptionCombos(cocs_code,"code","id")
    expect_equal("X8hrDf6bLDC",coc_id)
  })
})