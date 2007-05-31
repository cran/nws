\name{nwsFindTryFile}
\docType{methods}
\alias{nwsFindTryFile}
\alias{nwsFindTryFile-methods}
\alias{nwsFindTryFile,netWorkSpace-method}
\title{netWorkSpace Class Method}
\description{
Find a value of a workspace variable and write it to a file.
}

\usage{
\S4method{nwsFindTryFile}{netWorkSpace}(.Object, xName, fObj)
}

\arguments{
\item{.Object}{a netWorkSpace object}
\item{xName}{name of the variable to find}
\item{fObj}{File to write data to}
}

\details{
The nwsFindTryFile method trys to find a value in the variable specified by
'xName'.  If found, it writes the value to the file object, and
the method returns TRUE.  If it is not found, it simply returns FALSE.
}

\seealso{ \code{\link{nwsFetchTryFile}}, \code{\link{nwsFindTry}}}

\examples{
\dontrun{
ws <- netWorkSpace('nws example')
nwsStore(ws, 'x', 'Hello, world\n')
if (nwsFindTryFile(ws, 'x', 'hello.txt')) {
  cat('success\n')
} else {
  cat('failure\n')
}
}
}

\keyword{methods}
