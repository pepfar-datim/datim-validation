context("Get a list of data element orgunit associations")

with_mock_api({
  test_that("We can get a list of valid data elements for organisation units", {
    config <- LoadConfigFile(test_config("test-config.json"))
    options("maxCacheAge"=NULL)
    datasets<-c("MqNLEXmzIzr","kkXf2zXqTM0")
    test_ous_des<-getDataElementsOrgunits(organisationUnit = "KKFzPM8LoXs",datasets=datasets)
    expect_type(test_ous_des,"list")
    expect_equal(length(test_ous_des),2)
    expect_setequal(unlist(rlist::list.select(test_ous_des,dataset)),datasets)
    expect_equal(names(test_ous_des[[1]][1]),"dataset")
    expect_setequal(names(test_ous_des[[1]][[2]]),c("ous","des"))
  })
})


context("Flag invalid data element orgunit combinations")

with_mock_api({
  test_that("We flag invalid data element / orgunit associations in the data", {
    config <- LoadConfigFile(test_config("test-config.json"))
    options("maxCacheAge"=NULL)
    expect_type(config,"list")
    d<-d2Parser(filename=test_config("test-data-bad-des-ous.csv"),
                type="csv",
                organisationUnit = "KKFzPM8LoXs",
                dataElementIdScheme = "id",
                orgUnitIdScheme = "id",
                idScheme = "id",
                invalidData = FALSE) 
  datasets<-c("MqNLEXmzIzr","kkXf2zXqTM0")
  expect_warning(test_data<-checkDataElementOrgunitValidity(d,"KKFzPM8LoXs",datasets))
  expect_equal(NROW(test_data),1) 
})})


context("Flag invalid data element disagg combinations")

with_mock_api({
  test_that("We flag invalid data element / disagg associations in the data", {
    config <- LoadConfigFile(test_config("test-config.json"))
    options("maxCacheAge"=NULL)
    expect_type(config,"list")
    d<-d2Parser(filename=test_config("test-data-bad-de-disagg.csv"),
                type="csv",
                organisationUnit = "KKFzPM8LoXs",
                dataElementIdScheme = "id",
                orgUnitIdScheme = "id",
                idScheme = "id",
                invalidData = FALSE) 
    datasets<-c("i29foJcLY9Y","STL4izfLznL")
    skip_on_cran()
    expect_warning(test_data<-checkDataElementDisaggValidity(d,datasets=datasets,return_violations=TRUE))
    expect_equal(NROW(test_data),1) 
    expect_equal(test_data$storedby[1], "BAD")
  })})