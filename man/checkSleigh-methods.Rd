\name{checkSleigh}
\docType{methods}
\alias{checkSleigh}
\alias{checkSleigh-methods}
\alias{checkSleigh,sleighPending-method}
\title{sleighPending Class Method}
\description{
Return the number of results yet to be generated for the pending sleigh job, .Object. 
}

\usage{
\S4method{checkSleigh}{sleighPending}(.Object)
}

\arguments{
\item{.Object}{a sleighPending class object}
}

\details{
The sleighPending class object, .Object, is usually obtained through non-blocking
eachElem or non-blocking eachWorker. If the pending job is finished, i.e.
all results are generated, then 0 is returned. 
}
\seealso{ \code{\link{eachWorker}}, \code{\link{eachElem}} }

\examples{
\dontrun{
eo = list(blocking=0)
s = sleigh()
sp = eachElem(s, function(x) {Sys.sleep(100); x}, list(1:10), eo=eo) 
checkSleigh(sp)  
}
}

\keyword{methods}

