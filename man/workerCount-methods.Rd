\name{workerCount}
\docType{methods}
\alias{workerCount-methods}
\alias{workerCount,sleigh-method}
\alias{workerCount}
\title{sleigh Class Method}
\description{
Return number of Sleigh workers that have successfully started.
}

\usage{
\S4method{workerCount}{sleigh}(.Object)
}

\arguments{
\item{.Object}{a sleigh class object}
}

\examples{
\dontrun{
# simple hello world
s = sleigh()
workerCount(s)
}
}

\keyword{methods}
