% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getDataElementMap.R
\name{getDataElementMap}
\alias{getDataElementMap}
\title{Get Data Element Map}
\usage{
getDataElementMap(d2session = dynGet("d2_default_session", inherits = TRUE))
}
\arguments{
\item{d2session}{DHIS2 Session id}
}
\value{
Returns a data frame  of id, code, shortName, and name
}
\description{
Utility function of extraction of data element ids, codes,
shortNames, and names
}
\examples{
\dontrun{
de_map <- getDataElementMap()
}
}
