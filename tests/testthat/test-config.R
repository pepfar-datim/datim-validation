context("Read a config file and login")

test_that("We can read a config file", {
  config <- LoadConfigFile(test_config("test-config.json"))
  expect_type(config,"list")
  expect_named(config,c("dhis"))
  expect_equal(getOption("config"),test_config("test-config.json"))
  expect_named(config$dhis,c("baseurl","username","password"))
  
})

test_that("We can error when we cannot read a config file", {
  expect_error(LoadConfigFile(test_config("foo-config.json")))

})

with_mock_api({
test_that("We can error when we there is no user in the config file", {
  expect_error(loadSecrets(test_config("test-config-no-user.json"))) })
})

with_mock_api({
  test_that("We can error when we there is no password in the config file", {
    expect_error(loadSecrets(test_config("test-config-no-password.json"))) })
})

with_mock_api({
  test_that("We can error when we base URL does not appear to be valid", {
    expect_error(loadSecrets(test_config("test-config-bad-baseurl.json"))) })
})

with_mock_api({
  test_that("We can login", {
    test_result <- loadSecrets(test_config("test-config.json"))
    expect_true(test_result)
    expect_true(!is.na(getOption("organisationUnit")))
    expect_true(!is.na(getOption("baseurl")))
    expect_equal(getOption("maxCacheAge"),"7 days")

  })
})