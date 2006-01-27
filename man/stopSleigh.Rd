\name{stopSleigh}
\docType{methods}
\alias{stopSleigh}
\alias{stopSleigh,sleigh-method}
\title{sleigh Class Method}
\description{
Shutdown workers and remove sleigh workspace.
}

\usage{
stopSleigh(.Object)
}

\arguments{
\item{.Object}{a sleigh class object}
}

\details{
This function is called by \code{\link{close}} method. 
}

\seealso{\code{\link{close}}}


\examples{
\dontrun{
s = sleigh()
stopSleigh(s)
}
}

\keyword{methods}
