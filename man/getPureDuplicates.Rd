% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getDuplication.R
\name{getPureDuplicates}
\alias{getPureDuplicates}
\title{Get Pure Duplicates}
\usage{
getPureDuplicates(d)
}
\arguments{
\item{d}{D2 Parsed data frame}
}
\value{
Returns a data frame of d2 data, with pure duplicates used for
de-duplication purposes.
}
\description{
Returns a listing of pure duplicates by dataElement, period,
orgunit, categoryOptionCombo. For the purposes of de-duplication, these are
considered to be pure duplicates (only the attributeOptionCombo differ).
}
\examples{
\dontrun{
    d <- d2Parser("myfile.csv",type="csv",header=TRUE)
    dups <- getPureDuplicates(d)
    dups
 }

}
