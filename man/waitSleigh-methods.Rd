\name{waitSleigh}
\docType{methods}
\alias{waitSleigh}
\alias{waitSleigh-methods}
\alias{waitSleigh,sleighPending-method}
\title{sleighPending Class Method}
\description{
Wait and block for the results to be generated for the pending sleigh job, .Object. 
If the job has completed, then waitSleigh returns immediately with the generated results.
}

\usage{
\S4method{waitSleigh}{sleighPending}(.Object)
}

\arguments{
\item{.Object}{a sleighPending class object}
}

\details{
The sleighPending class object, .Object, is usually obtained from the return value of 
non-blocking eachElem or non-blocking eachWorker. 
}
\seealso{ \code{\link{eachWorker}}, \code{\link{eachElem}} }


\examples{
\dontrun{
eo = list(blocking=0)
sp = eachWorker(s, function() {Sys.sleep(100)}, eo=eo) 
waitSleigh(sp)  # wait on workers. Each worker sleeps for 100 seconds
}
}


\keyword{methods}
