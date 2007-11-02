\name{close}
\docType{methods}
\alias{close}
\alias{close-methods}
\alias{close,ANY-method}
\alias{close,sleigh-method}
\alias{close,nwsServer-method}
\title{nwsServer, sleigh Class Method}
\description{
Depends on the first argument type passed to the close function,
it behaves differently.
If the first argument is a sleigh object, then close function calls stopSleigh
to shutdown the sleigh workers and delete the sleigh workspace. 
If the first argument is a nwsServer object, then the connection to the
netWorkSpace server is closed.
}

\usage{
\S4method{close}{nwsServer}(con, ...)
\S4method{close}{sleigh}(con, ...)
}

\arguments{
\item{con}{a sleigh class object or a nwsServer class object}
\item{...}{optional fields}
}

\details{
The optional fields are not passed to stopSleigh method. 
They are defined to be compatible with the default, non-generic close method.
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
