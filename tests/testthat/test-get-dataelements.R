context("Get a list of data elements")

with_mock_api({
  test_that("We can get an dataelement map", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    test_des <- getDataElementMap(d2session = d2_default_session)
    expect_type(test_des, "list")
    expect_is(test_des, "data.frame")
    de_map_names <- c("name", "id", "code", "shortName", "optionSet.id", "zeroIsSignificant", "valueType")
    expect_setequal(names(test_des), de_map_names)
  })
})
