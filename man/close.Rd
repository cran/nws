\name{close}
\docType{methods}
\alias{close-methods}
\alias{close,ANY-method}
\alias{close,sleigh-method}
\alias{close}
\title{sleigh Class Method}
\description{
Shutdown sleigh workers and delete sleigh workspace.
}

\usage{
close(con, ...)
}

\arguments{
\item{con}{a sleigh class object}
\item{...}{optional fields}
}

\details{
close method calls stopSleigh method to shutdown sleigh workers and delete sleigh workspace.\cr

The optional fields are not passed to stopSleigh method. 
They are defined to be compatiable with the default, non-generic close method.
}

\examples{
\dontrun{
s = sleigh()
close(s)
}
}

\keyword{methods}