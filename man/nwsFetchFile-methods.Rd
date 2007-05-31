\name{nwsFetchFile}
\docType{methods}
\alias{nwsFetchFile}
\alias{nwsFetchFile-methods}
\alias{nwsFetchFile,netWorkSpace-method}
\title{netWorkSpace Class Method}
\description{
Fetch a value of a workspace variable and write it to a file.
}

\usage{
\S4method{nwsFetchFile}{netWorkSpace}(.Object, xName, fObj)
}

\arguments{
\item{.Object}{a netWorkSpace object}
\item{xName}{name of the variable to find}
\item{fObj}{File to write data to}
}

\details{
The nwsFetchFile method blocks until a value in the variable specified by
'xName' is found.  Once found, it writes the value to the file object,
and the value is removed from the variable.
}

\seealso{ \code{\link{nwsFindFile}}, \code{\link{nwsFetch}}}

\examples{
\dontrun{
ws <- netWorkSpace('nws example')
nwsStore(ws, 'x', 'Hello, world\n')
nwsFetchFile(ws, 'x', 'hello.txt')
}
}

\keyword{methods}
