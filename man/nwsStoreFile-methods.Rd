\name{nwsStoreFile}
\docType{methods}
\alias{nwsStoreFile}
\alias{nwsStoreFile-methods}
\alias{nwsStoreFile,netWorkSpace-method}
\title{netWorkSpace Class Method}
\description{
Store a new value into a variable in the workspace from a file.
}

\usage{
\S4method{nwsStoreFile}{netWorkSpace}(.Object, xName, fObj, n=0)
}

\arguments{
\item{.Object}{a netWorkSpace class object}
\item{xName}{name of the variable to be stored}
\item{fObj}{file to read data from to store in the variable.}
\item{n}{Number of bytes to write.  A value of zero means to
write all the data in the file.}
}

\details{
nwsStoreFile works like nwsStore, except that the value to
store in the workspace variable comes from a file.
If 'fObj' is a character string, nwsStoreFile calls 'file'
to obtain a file connection which is opened for the duration
of the method.
}

\seealso{\code{\link{nwsStore}}}

\examples{
\dontrun{
ws <- netWorkSpace('nws example')
nwsStoreFile(ws, 'x', '/etc/printcap')
}
}

\keyword{methods}
