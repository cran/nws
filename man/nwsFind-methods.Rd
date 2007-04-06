\name{nwsFind}
\docType{methods}
\alias{nwsFind}
\alias{nwsFind-methods}
\alias{nwsFind,netWorkSpace-method}
\title{netWorkSpace Class Method}
\description{
Find value associates with a variable in the shared netWorkSpace, .Object.
}

\usage{
\S4method{nwsFind}{netWorkSpace}(.Object, xName)
}

\arguments{
\item{.Object}{a netWorkSpace class object}
\item{xName}{name of the variable to be found}
}

\details{
Find method blocks until a value for xName is found in the shared netWorkSpace 
.Object. Once found, return the value associated with xName, but the value
is not removed from the shared netWorkSpace (as in the case of \code{\link{nwsFetch}}). 
If there is more than one value associated with xName, the
particular value returned depends on xName's behavior. See
\code{\link{nwsDeclare}} for details.
}

\seealso{ \code{\link{nwsDeclare}}, \code{\link{nwsFetch}}}

\examples{
\dontrun{
ws <- netWorkSpace('nws example')
nwsStore(ws, 'x', 1)
x <- nwsFind(ws, 'x')
}
}


\keyword{methods}
