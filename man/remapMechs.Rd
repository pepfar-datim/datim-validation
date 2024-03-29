% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remapMechs.R
\name{remapMechs}
\alias{remapMechs}
\title{Remap Mechanisms}
\usage{
remapMechs(
  mechs_in,
  organisationUnit,
  mode_in = "code",
  mode_out = "id",
  d2session = dynGet("d2_default_session", inherits = TRUE)
)
}
\arguments{
\item{mechs_in}{A vector of data element identifiers (codes, names, or
shortNames).}

\item{organisationUnit}{Organisation unit ancestor.}

\item{mode_in}{Should be one of code, name,shortName or id. This is the class
we are mapping from.}

\item{mode_out}{Should be one of code,name,shortName or id. This is the class
we are mapping to.}

\item{d2session}{DHIS2 Session id}
}
\value{
Returns a vector of mechanism UIDs
}
\description{
Function which converts mechanism codes, names or shortnames to
UIDs. \code{remapMechs} should be supplied a vector of data mechanisms
(names, codes or shortnames) along with the other required paramaters.
It will return a vector of UIDs.
}
\examples{
\dontrun{
d <- d2Parser("myfile.csv",type="csv")
#Add a new column with organisation unit short names.
d$mech_names <- remapMechs(d$attributeOptionCombo,
                           mode_in="id",
                           mode_out="code")
}
}
