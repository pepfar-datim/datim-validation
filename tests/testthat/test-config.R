context("Read a config file and login")

test_that("We can read a config file", {
  config <- LoadConfigFile(test_config("test-config.json"))
  expect_type(config,"list")
  expect_named(config,c("dhis"))
  expect_named(config$dhis,c("baseurl","username","password"))
})

test_that("We can error when we cannot read a config file", {
  expect_error(LoadConfigFile(test_config("foo-config.json")))

})

with_mock_api({
  test_that("We can login", {
    config <- LoadConfigFile(test_config("test-config.json"))
    test_result <- DHISLogin(config)
    expect_true(test_result)
    expect_equal(getOption("organisationUnit"),"KKFzPM8LoXs")
    expect_equal(getOption("baseurl"),config$dhis$baseurl)
    expect_equal(getOption("config"),test_config("test-config.json"))
  })
})