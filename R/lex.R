# Copyright (c) 2018-2021 mikefc@coolbutuseless.com

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Break a string into labelled tokens based upon a set of patterns
#'
#' @param text a single character string
#' @param regexes a named vector of regex strings.  Each string represents
#'                a regex to match a token, and the name of the string is the
#'                label for the token.  Each regex can contain an explicit
#'                captured group using the standard \code{()} brackets. If a regex
#'                doesn't not define a captured group then the entire regex will
#'                be captured. The regexes
#'                will be processed in order such that an early match takes
#'                 precedence over any later match.
#' @param verbose print more information about the matching process. default: FALSE
#' @param ... further arguments passed to \code{stringi::stri_match_all()}.
#'        e.g. \code{multiline = TRUE}
#'
#' @return a named character vector with the names representing the token type
#'         with the value being the element extracted by the corresponding
#'         regular expression.
#'
#' @import stringi
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lex <- function(text, regexes, verbose = FALSE, ...) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # disallow multiple capture groups in a single pattern.
  # i.e. regexes = c("(a|b)", "(c)|(d)")
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  captured_groups <- stringi::stri_match_all(regexes, regex = "(?<!\\\\)\\([^?]")

  n_captured_groups <- vapply(captured_groups, FUN = function(x) {
    if (anyNA(x)) {
      0L
    } else {
      nrow(x)
    }
  }, integer(1))
  if (any(n_captured_groups > 1)) {
    stop("Regexes can define at most only a single capture group. Patterns which need fixing",
         deparse(regexes[n_captured_groups > 1]))
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Any regex that has 0 capture groups has its whole regex become the
  # capture group
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  idx <- n_captured_groups == 0
  regexes[idx] <- paste0("(", regexes[idx], ")")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Insert a default pattern to match anything missed by the provided regexes
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  regexes        <- c(regexes, .missing = "(.)")
  regex_labels   <- names(regexes)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # All regexes must be named
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot(!anyNA(regex_labels))
  stopifnot(!any(regex_labels == ""))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Combine all the patterns into a single regex
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  regex <- paste(regexes, collapse = "|")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Match all regex against the text
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  token_matching  <- stringi::stri_match_all(text, ..., regex = regex)[[1]]

  if (verbose) {
    colnames(token_matching) <- c("all", regex_labels)
    print(token_matching)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Extract the actual token and the regex which matched the token
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # nolint start
  regex_idx      <- apply(token_matching[, -1, drop=FALSE], 1, function(x) {which(!is.na(x))})
  tokens         <- apply(token_matching[, -1, drop=FALSE], 1, function(x) {x[which(!is.na(x))]})
  # nolint end

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # if 'regex_idx' is a list, then a location was matched by multiple regexes
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.list(regex_idx)) { ## nocov start
    # 99.9% sure this error can't happen any more.
    # I can't even figure out how to trigger this error with pathological inputs
    lens <- lengths(regex_idx)
    idx  <- which(lens > 1)
    stop("lex issues at the following locations within 'text': ", deparse(idx))
  } ## nocov end


  names(tokens)  <- regex_labels[regex_idx]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If any tokens were captured by the '.missing' regex, then show
  # a warning message
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (verbose && any(names(tokens) == ".missing")) {
    not_captured <- sort(unique(tokens[names(tokens) == ".missing"]))
    warning("The following characters were not captured: ", deparse(not_captured))
  }

  tokens
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Regexes to match common elements
#'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
re <- list(
  number    = "[+\\-]?(?:0|[1-9]\\d*)(?:\\.\\d*)?(?:[eE][+\\-]?\\d+)?",
  email     = "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}",
  ipaddress = "(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)"
)
