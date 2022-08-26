
context("Flag invalid data element ACOC combinations")

with_mock_api({
  test_that("Can get a list of data elements and orgunits for a dataset", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    foo <- getDE_ACOC_Map(dataset = "cw2T5eAHxzW", d2session = d2_default_session)
    expect_type(foo, "list")
    expect_length(foo, 2L)
    expect_named(foo, expected = c("dataSetElements", "categoryCombo"), ignore.order = TRUE)
    expect_true(is.data.frame(foo$dataSetElements))
    expect_named(foo$dataSetElements, expected = c("dataElement.id"))
    expect_true(is.list(foo$categoryCombo))
    expect_named(foo$categoryCombo, expected = c("categoryOptionCombos"))
    expect_true(is.data.frame(foo$categoryCombo$categoryOptionCombos))
    expect_named(foo$categoryCombo$categoryOptionCombos, expected = c("id"))
  })

})

with_mock_api({
  test_that("Can error if more than one ACOC is specified", {
    expect_error(getDE_ACOC_Map(dataset = c("abc123", "def456"), d2session = training))
  })
})

with_mock_api({
  test_that("Can error if more than one orgunit exists", {
    d <- tibble::tribble(
      ~attributeOptionCombo, ~dataElement,
      "abc123", "zzzz123",
      "dev456", "zzzz123"
    )
    de_map <- list()

    expect_error(validateDEs_ACOCs(d, de_map))
  })

})


with_mock_api({
  test_that("Can return all invalid data", {
    d <- tibble::tribble(
      ~attributeOptionCombo, ~dataElement,
      "abc123", "zzzz123",
      "abc123", "zzzz123"
    )
    #This attribute option combo is not present in the data element/ACOC map
    de_map <- list(dataSetElements = data.frame(dataElement.id = c("zzzz123")),
                   categoryCombo = list(categoryOptionCombos = data.frame(id = c("xxxxx1234"))))

    test_data <- validateDEs_ACOCs(d, de_map)
    expect_identical(d, test_data)
  })

})

with_mock_api({
  test_that("We flag invalid data element / ACOC in the data", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    # nolint start
    d <- tibble::tribble(
      ~dataElement, ~period,~orgUnit,~categoryOptionCombo,~attributeOptionCombo,~value,~comment,
      "Kk4CdspETNQ","2017Q1","LnGaK6y98gC","pPoX6WdTN1o","pPoX6WdTN1o","10","BAD",
      "Kk4CdspETNQ","2017Q1","RQCy4nM3afc","HllvX50cXC0","HllvX50cXC0","5","GOOD",
      "Kk4CdspETNQ","2017Q1","KKFzPM8LoX7","HllvX50cXC0","HllvX50cXC0","20","GOOD"
    )
    # nolint end
    datasets <- c("cw2T5eAHxzW")
    expect_warning(test_data <- checkDataElementMechValidity(d, datasets, d2session = d2_default_session))
    expect_equal(NROW(test_data), 1)

    expect_warning(
      test_data <-
        checkDataElementMechValidity(
          d,
          datasets,
          return_violations = FALSE,
          d2session = d2_default_session
        )
    )
    expect_false(test_data)
  })})

with_mock_api({
  test_that("We pass invalid data element / attribute option combos in the data",
            {
              loginToDATIM(config_path = test_config("test-config.json"))
              expect_true(exists("d2_default_session"))

              d <- tibble::tribble(
                ~ dataElement, ~ attributeOptionCombo,
                "Kk4CdspETNQ", "HllvX50cXC0",
                "Kk4CdspETNQ", "HllvX50cXC0",
                "Kk4CdspETNQ", "HllvX50cXC0"
              )

              datasets <- c("cw2T5eAHxzW")
              test_data <-
                checkDataElementMechValidity(d, datasets, d2session = d2_default_session)
              expect_equal(NROW(test_data), 0)
              expect_named(test_data, c("dataElement", "attributeOptionCombo"))

              test_data <-
                checkDataElementMechValidity(d,
                                                datasets,
                                                return_violations = FALSE,
                                                d2session = d2_default_session)
              expect_true(test_data)
            })
})
