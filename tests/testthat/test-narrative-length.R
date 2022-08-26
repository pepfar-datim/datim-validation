test_that("We can flag narratives which are too long", {

  d <- tibble::tribble(
    ~value,
    paste(stringi::stri_rand_lipsum(100),sep="",collapse=""), #Approximately 63k
    stringi::stri_rand_lipsum(1) #Approximately 500
  )

  expect_warning(narrative_check <- checkNarrativeLength(d))
  expect_equal(NROW(narrative_check),1L) })

test_that("We can pass good narratives", {

  d <- tibble::tribble(
    ~value,
     "This is a great narrative."
  )

  expect_silent(narrative_check <- checkNarrativeLength(d))
  expect_equal(narrative_check, TRUE)  })
