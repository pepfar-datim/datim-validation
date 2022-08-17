context("Can detect exact duplicates")
with_mock_api({
  test_that("We can detect exact duplicates", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    d<-d2Parser(filename=test_config("test-data-exact-dups.csv"),
                          type="csv",
                          organisationUnit = "KKFzPM8LoXs",
                          dataElementIdScheme = "id",
                          orgUnitIdScheme = "id",
                          idScheme = "id",
                          invalidData = FALSE,
                          d2session = d2_default_session)
    expect_warning(dups<-getExactDuplicates(d),"Your data contains exact duplicates!")
    expect_equal(NROW(dups),1)
  })})


context("Don't warn on no duplicates")
with_mock_api({
  test_that("We don't flag data without duplication", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    d<-d2Parser(filename=test_config("test-data.csv"),
                type="csv",
                organisationUnit = "KKFzPM8LoXs",
                dataElementIdScheme = "id",
                orgUnitIdScheme = "id",
                idScheme = "id",
                invalidData = FALSE,
                d2session = d2_default_session)
    expect_silent(dups<-getExactDuplicates(d))
    expect_equal(NROW(dups),0)
  })})
