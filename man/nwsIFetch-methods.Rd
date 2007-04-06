\name{nwsIFetch}
\docType{methods}
\alias{nwsIFetch}
\alias{nwsIFetch-methods}
\alias{nwsIFetch,netWorkSpace-method}
\title{netWorkSpace Class Method}
\description{
Create a function that acts as a destructive iterator over the
values of the specified variable.
}

\usage{
\S4method{nwsIFetch}{netWorkSpace}(.Object, xName)
}

\arguments{
\item{.Object}{a netWorkSpace class object}
\item{xName}{name of the variable to be fetched}
}

\details{
The iterator function returned by the nwsIFetch method takes
no arguments, and works just like the nwsFetch method, specified
with the same arguments that were passed to nwsIFetch method.
Note that the nwsIFind and nwsIFindTry methods are much more useful,
since they provide the only way to iterate over values of a variable
non-destructively.
nwsIFetch and nwsIFetchTry are provided for completeness.
}
\seealso{ \code{\link{nwsFetch}}, \code{\link{nwsIFetchTry}} }

\examples{
\dontrun{
ws <- netWorkSpace('nws example')
nwsStore(ws, 'x', 10)
it <- nwsIFetch(ws, 'x')
it()  # returns the value 10
it()  # blocks until another process stores a value in the variable
}
}

\keyword{methods}
