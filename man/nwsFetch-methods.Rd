\name{nwsFetch}
\docType{methods}
\alias{nwsFetch}
\alias{nwsFetch-methods}
\alias{nwsFetch,netWorkSpace-method}
\title{netWorkSpace Class Method}
\description{
Fetch value associates with a variable from the shared netWorkSpace, .Object.
}

\usage{
\S4method{nwsFetch}{netWorkSpace}(.Object, xName)
}


\arguments{
\item{.Object}{a netWorkSpace class object}
\item{xName}{name of the variable to be fetched}
}

\details{
Fetch method blocks until a value for xName is found in the shared netWorkSpace, .Object.
Once found, remove a value associated with xName from the shared netWorkSpace.
This operation is atomic. If there are multiple NetWorkSpaces clients
nwsFetch or nwsFetchTry a given xName, any given value from the set of values associated with
xName will be returned to just one client session. 
If there is more than one value associated with xName, the particular
value removed depends on xName's behavior. See \code{\link{nwsDeclare}} for details.
}
\seealso{ \code{\link{nwsDeclare}}, \code{\link{nwsFetchTry}} }

\examples{
\dontrun{
ws = new('netWorkSpace', 'nws example')
nwsStore(ws, 'x', 10)
nwsFetch(ws, 'x')
nwsFetch(ws, 'x') # no value for x; therefore block on fetch
}
}

\keyword{methods}
