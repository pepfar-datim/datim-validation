context("Get a list of organisation units")

with_mock_api({
  test_that("We can get an orgunit map", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    test_ous <- getOrganisationUnitMap(d2session = d2_default_session)
    expect_type(test_ous, "list")
    ou_map_names <- c("name", "id", "code", "shortName")
    expect_setequal(names(test_ous), ou_map_names)
  })
})


with_mock_api({
  test_that("We can inform whether a UID is an orgunit", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    expect_true(checkOperatingUnit("KKFzPM8LoXs", d2session = d2_default_session))
    expect_false(checkOperatingUnit("ABCDEFG123", d2session = d2_default_session))
    expect_true(checkOperatingUnit("KKFzPM8LoXs", d2session = d2_default_session))

  })
})
