test_that("We can flag narratives which are too long", {
  d <- list()
  d$info$messages <- MessageQueue()
  d$data$import <- tibble::tribble(
    ~value,
    paste(stringi::stri_rand_lipsum(100), sep = "", collapse = ""), #Approximately 63k
    stringi::stri_rand_lipsum(1) #Approximately 500
  )

  expect_silent(d <- checkNarrativeLength(d))
  expect_equal(NROW(d$tests$long_narratives), 1L)
  expect_equal(length(d$info$messages$message), 1L)
  })

test_that("We can pass good narratives", {
  d <- list()
  d$info$messages <- MessageQueue()
  d$data$import <- tibble::tribble(
    ~value,
     "This is a great narrative."
  )

  expect_silent(d <- checkNarrativeLength(d))
  expect_null(d$tests$long_narratives)})
