\name{nwsListVars}
\docType{methods}
\alias{nwsListVars}
\alias{nwsListVars-methods}
\alias{nwsListVars,netWorkSpace-method}
\title{netWorkSpace Class Method}
\description{
List variables in a netWorkSpace.
}

\usage{
\S4method{nwsListVars}{netWorkSpace}(.Object, wsName='', showDataFrame=FALSE)
}

\arguments{
  \item{.Object}{a netWorkSpace class object}
  \item{wsName}{name of the netWorkSpace}
  \item{showDataFrame}{show result in data frame or string}
}

\details{
Return listing of variables in the netWorkSpace with the name passed by wsName argument.
If wsName is empty, then return variables in the netWorkSpace
represented by .Object.

The return values from nwsListVars can be represented in a string or a data frame.
If showDataFrame is set to FALSE (default value), then
the listing is returned in a string. To see list output clearly,
use: write(nwsListVars(.Object), stdout()).
If showDataFrame is set to TRUE, then the listing is returned in a
data frame with these fields: Variables, NumValues, NumFetchers,
NumFinders, and Mode.
}

\examples{
\dontrun{
# example 1
ws <- netWorkSpace('nws example')
write(nwsListVars(ws), stdout())
}
}

\keyword{methods}
