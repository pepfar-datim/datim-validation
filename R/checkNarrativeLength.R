#' Title Check that the length of narrative data does not exceed 50k characters.
#'
#' @param d datimvalidation object obtained from d2Parser.
#'
#' @return datimvalidation object obtained from d2Parser
#' checked for long narratives.
#' @export
#'

checkNarrativeLength <- function(d) {
  MAX_ALLOWED_CHARACTERS <- 50000L

  long_narratives <- d$data$import %>%
    dplyr::filter(nchar(value) > MAX_ALLOWED_CHARACTERS)

  if (NROW(long_narratives) > 0) {
    msg <- "ERROR: Narratives with more than 50,000 characters found!"
    d$info$messages <- appendMessage(d$info$messages, msg, "ERROR")
    d$tests$long_narratives <- long_narratives
  } else {
    msg <- "Narratives were of an acceptable length."
    d$info$messages <- appendMessage(d$info$messages, msg, "INFO")
  }
  d

}
