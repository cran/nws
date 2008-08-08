\name{close}
\docType{methods}
\alias{close}
\alias{close-methods}
\alias{close,ANY-method}
\alias{close,sleigh-method}
\alias{close,nwsServer-method}
\title{Close a Sleigh or netWorkSpace}
\description{
If the first argument is a \code{sleigh} object, then \code{close} calls 
\code{stopSleigh} to shut down the sleigh workers and delete the sleigh 
workspace. 
If the first argument is a \code{nwsServer} object, then the connection to the
netWorkSpace server is closed.
}

\usage{
\S4method{close}{nwsServer}(con, ...)
\S4method{close}{sleigh}(con, ...)
}

\arguments{
\item{con}{an object of class \code{sleigh} or \code{nwsServer}.}
\item{...}{optional fields}
}

\details{
The optional arguments are not passed to \code{stopSleigh} method. 
They are defined to be compatible with the default, non-generic 
\code{close} method.
}

\examples{
\dontrun{
s = sleigh()
close(s)

wss = nwsServer()
close(wss)
}
}

\keyword{methods}
