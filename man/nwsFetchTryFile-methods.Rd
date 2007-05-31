\name{nwsFetchTryFile}
\docType{methods}
\alias{nwsFetchTryFile}
\alias{nwsFetchTryFile-methods}
\alias{nwsFetchTryFile,netWorkSpace-method}
\title{netWorkSpace Class Method}
\description{
Fetch a value of a workspace variable and write it to a file.
}

\usage{
\S4method{nwsFetchTryFile}{netWorkSpace}(.Object, xName, fObj)
}

\arguments{
\item{.Object}{a netWorkSpace object}
\item{xName}{name of the variable to find}
\item{fObj}{File to write data to}
}

\details{
The nwsFetchTryFile method trys to find a value in the variable specified by
'xName'.  If found, the value is removed from the variable, it writes the
value to the file object, and the method returns TRUE.  If it is not found,
it simply returns FALSE.
}

\seealso{ \code{\link{nwsFindTryFile}}, \code{\link{nwsFetchTry}}}

\examples{
\dontrun{
ws <- netWorkSpace('nws example')
nwsStore(ws, 'x', 'Hello, world\n')
if (nwsFetchTryFile(ws, 'x', 'hello.txt')) {
  cat('success\n')
} else {
  cat('failure\n')
}
}
}

\keyword{methods}
