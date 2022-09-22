
context("Flag invalid data element ACOC combinations")


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
    de_map <- list(list(acocs = c("o0exnLGytku", "SJCCfahcsGr"),
                                  ous = c("OZyRtJPWHii"),
                                  des = c("OPmsmx2Rvz2")))


    test_data <- validateDEs_ACOCs(d, de_map)
    expect_identical(d, test_data)

    #A dataset with no mechanisms. All data should be invalid
    de_map <- list(list(acocs = character(),
                        ous = c("OZyRtJPWHii"),
                        des = c("zzzz123")))

    test_data <- validateDEs_ACOCs(d, de_map)
    expect_identical(d, test_data)

    #A dataset with no data element. Makes no sense, but lets test it.
    de_map <- list(list(acocs = c("abc123"),
                        ous = c("OZyRtJPWHii"),
                        des = character()))

    test_data <- validateDEs_ACOCs(d, de_map)
    expect_identical(d, test_data)

  })

})

with_mock_api({
  test_that("We flag invalid data element / ACOC in the data", {
    loginToDATIM(config_path = test_config("test-config.json"))
    expect_true(exists("d2_default_session"))
    d <- list()
    d$info$messages <- MessageQueue()
    # nolint start
    d$data$import <- tibble::tribble(
      ~dataElement, ~period,~orgUnit,~categoryOptionCombo,~attributeOptionCombo,~value,~comment,
      "Kk4CdspETNQ","2017Q1","LnGaK6y98gC","pPoX6WdTN1o","badMech1234","10","BAD",
      "Kk4CdspETNQ","2017Q1","RQCy4nM3afc","HllvX50cXC0","o0exnLGytku","5","GOOD",
      "Kk4CdspETNQ","2017Q1","KKFzPM8LoX7","HllvX50cXC0","SJCCfahcsGr","20","GOOD"
    )
    # nolint end
    datasets <- c("cw2T5eAHxzW")
    d <- checkDataElementMechValidity(d, datasets, d2session = d2_default_session)
    expect_equal(NROW(d$tests$bad_data_des_acocs), 1)
    expect_named(d$tests$bad_data_des_acocs, c("dataElement", "attributeOptionCombo"), ignore.order = TRUE)
  })})

with_mock_api({
  test_that("We pass invalid data element / attribute option combos in the data",
            {
              loginToDATIM(config_path = test_config("test-config.json"))
              expect_true(exists("d2_default_session"))

              d <- list()
              d$info$messages <- MessageQueue()

              d$data$import <- tibble::tribble(
                ~ dataElement, ~ attributeOptionCombo,
                "Kk4CdspETNQ", "o0exnLGytku",
                "Kk4CdspETNQ", "miCk34PtHnS",
                "Kk4CdspETNQ", "SJCCfahcsGr"
              )


              datasets <- c("cw2T5eAHxzW")
              d <-
                checkDataElementMechValidity(d,
                                             datasets,
                                             d2session = d2_default_session)
              expect_null(d$tests$bad_data_des_acocs)

            })
})
