test_that("Can pass a valid validation rule expression", {

  inds <- list("#{AppEXdI1dhS}",
               "#{AppEXdI1dhS} + 10",
               "( 2 * #{P3jJH5Tu5VC.S34ULMcHMca} ) / ( #{FQ2o8UBlcrS.S34ULMcHMca} - 200 ) * 25",
               "1+2+3-4")

  parsed_inds <- lapply(inds, function(x) lex(x, indicator_regexes))
  is_valid <- isValidIndicator(parsed_inds)
  expect_identical(is_valid, c(TRUE, TRUE, TRUE,TRUE))

})

test_that("Can fail a valid validation rule expression", {

  inds <- list("foo",
               "#{AppEXdI1dhS} + foo",
               "R{BfMAe6Itzgt.ACTUAL_REPORTS} / R{BfMAe6Itzgt.EXPECTED_REPORTS}")
  parsed_inds <- lapply(inds, function(x) lex(x, indicator_regexes))
  is_valid <- isValidIndicator(parsed_inds)
  expect_identical(is_valid, c(FALSE, FALSE, FALSE))

})
