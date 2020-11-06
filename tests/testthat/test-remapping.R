context("Remap category option combos")

with_mock_api({
  test_that("We can remap from id to name", {
    my_creds <- DHISLogin$new(test_config("test-config.json"))
    my_creds$maxCacheAge<-NULL
    my_creds$handle<-NULL
    cocs_uids<-c("EOuF74nArxp","fIB4ouSgQJ4")
    coc_names<-remapCategoryOptionCombos(cocs_uids,"id","name", creds = my_creds)
    expect_setequal(c("Positive","Negative"),coc_names)
  })
})


with_mock_api({
  test_that("We can remap from id to shortName", {
    my_creds <- DHISLogin$new(test_config("test-config.json"))
    my_creds$maxCacheAge<-NULL
    my_creds$handle<-NULL
    cocs_uids<-c("EOuF74nArxp","fIB4ouSgQJ4")
    coc_names<-remapCategoryOptionCombos(cocs_uids,"id","shortName", creds = my_creds)
    expect_setequal(c("Positive","Negative"),coc_names)
  })
})


with_mock_api({
  test_that("We can remap from code to shortName", {
    my_creds <- DHISLogin$new(test_config("test-config.json"))
    my_creds$maxCacheAge<-NULL
    my_creds$handle<-NULL
    cocs_code<-c("00000")
    coc_id<-remapCategoryOptionCombos(cocs_code,"code","id", creds = my_creds)
    expect_equal("X8hrDf6bLDC",coc_id)
  })
})