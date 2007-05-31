\name{nwsFindFile}
\docType{methods}
\alias{nwsFindFile}
\alias{nwsFindFile-methods}
\alias{nwsFindFile,netWorkSpace-method}
\title{netWorkSpace Class Method}
\description{
Find a value of a workspace variable and write it to a file.
}

\usage{
\S4method{nwsFindFile}{netWorkSpace}(.Object, xName, fObj)
}

\arguments{
\item{.Object}{a netWorkSpace object}
\item{xName}{name of the variable to find}
\item{fObj}{File to write data to}
}

\details{
The nwsFindFile method blocks until a value in the variable specified by
'xName' is found.  Once found, it writes the value to the file object,
but the value is not removed from the variable (as in the case of
\code{\link{nwsFetchFile}}). 
}

\seealso{ \code{\link{nwsFetchFile}}, \code{\link{nwsFind}}}

\examples{
\dontrun{
ws <- netWorkSpace('nws example')
nwsStore(ws, 'x', 'Hello, world\n')
nwsFindFile(ws, 'x', 'hello.txt')
}
}

\keyword{methods}
