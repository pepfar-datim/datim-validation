
#' Title MessageQueue
#' @description A simple S3 object to deal with messages created during
#' DataPack processing
#' @param message One or more character strings
#' @param level One of ERROR, WARNING, INFO
#'
#' @return Object of class data.frame and Message queue.
#' @export

MessageQueue <- function(message=character(), level=character()) {

  messages <- data.frame(
    message = message,
    level = level, stringsAsFactors = FALSE
  )

  ## Set the name for the class

  attr(messages, "class") <- "MessageQueue"

  messages
}


#' Title appendMessage
#' @description Generic function to handle appending messages to a MessageQueue
#' @param x A message queue
#' @param message A string or vector of strings of messages.
#' @param level A string or vector of strings of
#' message levels (ERROR, WARNING, INFO)
#' @return A MessageQueue class.
#' @export

appendMessage <- function(x, message, level) {
  UseMethod("appendMessage", x)
}

#' Title appendMessage.MessageQueue
#' @description Internal S3 method to deal with appending messages
#' @param x A MessageQueue object
#' @param message  A message or vector of messages.
#' @param level  A string or vector of message levels (ERROR, WARNING, INFO)
#' @return A MessageQueue object
#' @export

appendMessage.MessageQueue <- function(x, message=NA, level=NA) {

  if (length(message) != length(level)) {
    stop("Messages and warnings must be of the same length")
  }

  if (any(is.na(message))) {
    warning("Empty message detected.")
  }

  if (any(is.na(level))) {
    level[is.na(level)] <- "UNKNOWN"
  }

  #Check to see if the message and level match.
  #If they don't issue a warning

  if (!grepl(level, substring(message, first = 0, last = 20))) {
    warning(paste("Inconsistent message and level!", level, ":", message))
  }

  new_me <- rbind.data.frame(x, list(message = message, level = level),
                             stringsAsFactors = FALSE)
  class(new_me) <- c("data.frame", "MessageQueue")
  return(new_me)
}

#' Title printMessages
#' @description Generic function to handle printing messages of a MessageQueue
#'
#' @param x A MessageQueue object
#'
#' @return Returns a formatted output to the console
#' @export
#'
printMessages <- function(x) {
  UseMethod("printMessages", x)
}


#' Title printMessage.MessageQueue
#' @description Internal S3 method to deal with printing messages
#'
#' @param x A MessageQueue object
#'
#' @return Returns a formatted output to the console
#' @export
#'
printMessages.MessageQueue <- function(x) {
  # If warnings, show all grouped by sheet and issue
  if (NROW(x) > 0 & interactive()) {
    options(warning.length = 8170)

    levels <- c("ERROR", "WARNING", "INFO")
    messages <- x
    class(messages) <- "data.frame"
    messages <- messages %>%
      dplyr::mutate(level = factor((level), levels = levels)) %>%
      dplyr::arrange(level, message) %>%
      dplyr::select(message)

    messages <-
      paste(
        paste(
          seq_along(messages$message),
          ": ", messages$message
        ),
        sep = "",
        collapse = "\r\n")

    key <- paste0(
      "*********************\r\n",
      "KEY:\r\n",
      "- WARNING!: Problematic, but doesn't stop us from processing validating your file.\r\n",
      "- ERROR!: You MUST address these issues and resubmit your file.\r\n",
      "*********************\r\n\r\n")

    cat(crayon::red(crayon::bold("VALIDATION ISSUES: \r\n\r\n")))
    cat(crayon::red(key))
    cat(crayon::red(messages))
  }
}
