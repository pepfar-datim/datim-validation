context("Get a list of mechanisms")

with_mock_api({
  test_that("We can get an mechanism map", {
    my_creds <- DHISLogin$new(test_config("test-config.json"))
    my_creds$maxCacheAge<-NULL
    my_creds$handle<-NULL
    expect_warning(getMechanismsMap(creds = my_creds))
    test_mechs<-getMechanismsMap(organisationUnit="KKFzPM8LoXs", creds = my_creds)
    expect_type(test_mechs,"list")
    expect_is(test_mechs,"data.frame")
    mech_map_names<-c("name","id","code","startDate","endDate")
    expect_setequal(names(test_mechs),mech_map_names)
    col_types<-lapply(test_mechs, typeof)
    expect_identical(col_types$code,"character")
    expect_identical(col_types$id,"character")
    expect_identical(col_types$name,"character")
    expect_identical(col_types$startDate,"double")
    expect_identical(col_types$endDate,"double")
  })
})

with_mock_api({
  test_that("We can error on an empty list of mechanisms", {
    my_creds <- DHISLogin$new(test_config("test-config.json"))
    my_creds$maxCacheAge<-NULL
    my_creds$handle<-NULL
    expect_null(getMechanismsMap(organisationUnit="RKoVudgb05Y", creds = my_creds))
  })
})