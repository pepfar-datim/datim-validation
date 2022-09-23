#' Title
#'
#' @param d datimvalidation object
#' @param d2session datimutils login object
#'
#' @return datimvalidation object
#' @export
#'
checkValidationRules <- function(d, d2session = dynGet("d2_default_session",
                                                                   inherits = TRUE)) {
  can_parallel <-
    "parallel" %in% rownames(utils::installed.packages()) == TRUE &
    .Platform$OS.type != "windows"
  
  if (can_parallel) {
    n_cores <-
      ifelse(Sys.getenv("MAX_CORES") != "",
             as.numeric(Sys.getenv("MAX_CORES")),
             parallel::detectCores())
    doMC::registerDoMC(cores = n_cores - 1L)
  }
  
  d$tests$validation_rules <- validateData(
    d$data$import,
    organisationUnit = d$info$organisationUnit,
    return_violations_only = TRUE,
    parallel = can_parallel,
    d2session = d2session
  )
  
  if (!is.null(d$tests$validation_rules) && NROW (d$tests$validation_rules) > 0) {
    msg <- paste("WARNING!", NROW(d$tests$validation_rules), 
                 "validation rule issues were found.")
    d$info$messages <- appendMessage(d$info$messages,msg,"WARNING")
  } else {
    msg <- "No validation rule issues were found."
    d$info$messages <- appendMessage(d$info$messages,msg,"INFO")
  }
  
  d
}

