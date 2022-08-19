context("Get a list of category option combos")

with_mock_api({
  test_that("We can get an category option combo map", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    test_cocs <- getCategoryOptionCombosMap(d2session = d2_default_session)
    expect_type(test_cocs, "list")
    expect_is(test_cocs, "data.frame")
    coc_map_names <- c("name", "id", "code", "shortName")
    expect_setequal(names(test_cocs), coc_map_names)
    col_types <- lapply(test_cocs, typeof)
    expect_identical(col_types$code, "character")
    expect_identical(col_types$id, "character")
    expect_identical(col_types$name, "character")
    expect_identical(col_types$shortName, "character")
  })
})
