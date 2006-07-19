\name{nwsFetchTry}
\docType{methods}
\alias{nwsFetchTry}
\alias{nwsFetchTry-methods}
\alias{nwsFetchTry,netWorkSpace-method}
\title{netWorkSpace Class Method}
\description{
Attempt to fetch value associates with a variable from the shared netWorkSpace; 
a non-blocking version of \code{\link{nwsFetch}}.
}

\usage{
\S4method{nwsFetchTry}{netWorkSpace}(.Object, xName, defaultVal=NULL)
}

\arguments{
\item{.Object}{a netWorkSpace class object}
\item{xName}{name of the variable to be fetched}
\item{defaultVal}{value to return, if xName is not found in the netWorkSpace}
}

\details{
Look in the shared netWorkSpace for a value bound to xName.  If found,
remove a value associated with xName from the shared netWorkSpace.
This operation is atomic. If there are multiple NetWorkSpaces clients
nwsFetch or nwsFetchTry a given xName, any given value from the set of values associated with
xName will be returned to just one client session. 

If variable is not found, return immediately rather than block on the 
operation (as in the case of \code{\link{nwsFetch}}). If variable is not found, the value of argument 
defaultVal is returned. By default, defaultVal is NULL.
}
\seealso{ \code{\link{nwsDeclare}}, \code{\link{nwsFetch}} }

\examples{
\dontrun{
ws = new('netWorkSpace', 'nws example')
# If variable 'x' is not found in the shared netWorkSpace,
# return default value, NULL.
nwsFetchTry(ws, 'x')
# If variable 'x' is not found in the shared netWorkSpace, return 10.
nwsFetchTry(ws, 'x', 10)
}
}

\keyword{methods}
