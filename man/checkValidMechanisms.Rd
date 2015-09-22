\name{checkValidMechanisms}
\alias{checkValidMechanisms}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
checkValidMechanisms(mechs, base.url, ou, username, passowrd)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mechs}{
%%     ~~Describe \code{mechs} here~~
}
  \item{base.url}{
%%     ~~Describe \code{base.url} here~~
}
  \item{ou}{
%%     ~~Describe \code{ou} here~~
}
  \item{username}{
%%     ~~Describe \code{username} here~~
}
  \item{passowrd}{
%%     ~~Describe \code{passowrd} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (mechs, base.url, ou, username, passowrd) 
{
    r <- GET(URLencode(paste0(base.url, "api/categoryOptions?filter=organisationUnits.id:eq:", 
        ou, "&fields=name,id,code,categoryOptionCombos[id]&filter=endDate:gt:2016-09-29&paging=false")), 
        authenticate(username, password))
    r <- content(r, "parsed", "application/json")
    mechs.valid <- ldply(lapply(r$categoryOptions, function(x) t(unlist(x))))
    mechs.valid <- colwise(as.character)(mechs.valid)
    mechs <- unique(mechs)
    mechs[!(mechs \%in\% mechs.valid$categoryOptionCombos.id)]
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
