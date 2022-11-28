test_that("Can pass a valid validation rule expression", {

  inds <- "#{AppEXdI1dhS}"
  parsed_inds <- lapply(inds, function(x) lex(x, datimvalidation:::indicator_regexes))
  is_valid <- datimvalidation:::isValidIndicator(parsed_inds)
  expect_true(is_valid)

  inds <- "#{AppEXdI1dhS} + 10"
  parsed_inds <- lapply(inds, function(x) lex(x, indicator_regexes))
  is_valid <- isValidIndicator(parsed_inds)
  expect_true(all(is_valid))

})

test_that("Can fail a valid validation rule expression", {

  inds <- "foo"
  parsed_inds <- lapply(inds, function(x) lex(x, indicator_regexes))
  is_valid <- isValidIndicator(parsed_inds)
  expect_false(is_valid)



  inds <- "#{AppEXdI1dhS} + foo"
  parsed_inds <- lapply(inds, function(x) lex(x, indicator_regexes))
  is_valid <- isValidIndicator(parsed_inds)
  expect_false(is_valid)
})
