context("Get a list of mechanisms")

with_mock_api({
  test_that("We can get an mechanism map", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    #expect_warning(getMechanismsMap(d2session = d2_default_session))
    test_mechs <- getMechanismsMap(organisationUnit = "KKFzPM8LoXs",
                                   d2session = d2_default_session)
    expect_type(test_mechs, "list")
    expect_is(test_mechs, "data.frame")
    mech_map_names <- c("name", "id", "code", "startDate", "endDate")
    expect_setequal(names(test_mechs), mech_map_names)
    col_types <- lapply(test_mechs, typeof)
    expect_identical(col_types$code, "character")
    expect_identical(col_types$id, "character")
    expect_identical(col_types$name, "character")
    expect_identical(col_types$startDate, "double")
    expect_identical(col_types$endDate, "double")
  })
})

with_mock_api({
  test_that("We can error on an empty list of mechanisms", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    expect_null(getMechanismsMap(organisationUnit = "RKoVudgb05Y",
                                 d2session = d2_default_session))
  })
})
