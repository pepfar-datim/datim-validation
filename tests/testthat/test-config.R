context("Read a config file and login")


with_mock_api({
  test_that("We can login", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_setequal(class(d2_default_session),c("R6","d2Session"))
    expect_equal(d2_default_session$base_url,"https://training.datim.org/")
    rm(d2_default_session)
  })
})