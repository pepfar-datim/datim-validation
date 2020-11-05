context("Get a list of organisation units")

with_mock_api({
  test_that("We can get an orgunit map", {
    my_creds <- DHISLogin$new(test_config("test-config.json"))
    my_creds$maxCacheAge<-NULL
    my_creds$handle<-NULL
    test_ous<-getOrganisationUnitMap(creds = my_creds)
    expect_type(test_ous,"list")
    ou_map_names<-c("name","id","code","shortName")
    expect_setequal(names(test_ous),ou_map_names)
  })
})


with_mock_api({
  test_that("We can inform whether a UID is an orgunit", {
    my_creds <- DHISLogin$new(test_config("test-config.json"))
    my_creds$maxCacheAge<-NULL
    my_creds$handle<-NULL
    expect_true(checkOperatingUnit("KKFzPM8LoXs", creds = my_creds))
    expect_false(checkOperatingUnit("ABCDEFG123", creds = my_creds))
    expect_true(checkOperatingUnit("KKFzPM8LoXs", creds = my_creds))

  })
})