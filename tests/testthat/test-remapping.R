context("Remap category option combos")

with_mock_api({
  test_that("We can remap from id to name", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    cocs_uids<-c("EOuF74nArxp","fIB4ouSgQJ4")
    coc_names<-remapCategoryOptionCombos(cocs_uids,"id","name", d2session = d2_default_session)
    expect_setequal(c("Positive","Negative"),coc_names)
  })
})


with_mock_api({
  test_that("We can remap from id to shortName", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    cocs_uids<-c("EOuF74nArxp","fIB4ouSgQJ4")
    coc_names<-remapCategoryOptionCombos(cocs_uids,"id","shortName", d2session = d2_default_session)
    expect_setequal(c("Positive","Negative"),coc_names)
  })
})


with_mock_api({
  test_that("We can remap from code to shortName", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    cocs_code<-c("00000")
    coc_id<-remapCategoryOptionCombos(cocs_code,"code","id", d2session = d2_default_session)
    expect_equal("X8hrDf6bLDC",coc_id)
  })
})