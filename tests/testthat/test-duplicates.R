context("Can detect exact duplicates")
with_mock_api({
  test_that("We can detect exact duplicates", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    d <- d2Parser(filename = test_config("test-data-exact-dups.csv"),
                          type = "csv",
                          organisationUnit = "KKFzPM8LoXs",
                          dataElementIdScheme = "id",
                          orgUnitIdScheme = "id",
                          idScheme = "id",
                          invalidData = FALSE,
                          d2session = d2_default_session)
d <- getExactDuplicates(d)
expect_equal(NROW(d$tests$exact_duplicates), 1)
expect_true(grepl("duplicates", d$info$messages$message))
})})


context("Don't warn on no duplicates")
with_mock_api({
  test_that("We don't flag data without duplication", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    d <- d2Parser(filename = test_config("test-data.csv"),
                type = "csv",
                organisationUnit = "KKFzPM8LoXs",
                dataElementIdScheme = "id",
                orgUnitIdScheme = "id",
                idScheme = "id",
                invalidData = FALSE,
                d2session = d2_default_session)
    d <- getExactDuplicates(d)
    expect_null(d$tests$exact_duplicates)
  })})
