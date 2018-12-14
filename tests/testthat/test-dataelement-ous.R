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