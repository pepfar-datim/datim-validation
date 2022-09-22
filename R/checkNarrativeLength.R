#' Title Check that the length of narrative data does not exceed 50k characters.
#'
#' @param data
#' @param d2session
#'
#' @return
#' @export
#'

checkNarrativeLength <- function(data) {
  MAX_ALLOWED_CHARACTERS <- 50000L

  long_narratives <- data %>%
    dplyr::filter(nchar(value) > MAX_ALLOWED_CHARACTERS)

  if (NROW(long_narratives) > 0) {
    warning("ERROR: Narratives with more than 50,000 characters found!")
    long_narratives
  } else {
    return(TRUE)
  }
}
