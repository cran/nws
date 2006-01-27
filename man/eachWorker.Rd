\name{eachWorker}
\docType{methods}
\alias{eachWorker-methods}
\alias{eachWorker,sleigh-method}
\alias{eachWorker}
\title{sleigh Class Method}
\description{
Evaluate given function exactly once for each worker in sleigh, s. 
}

\usage{
eachWorker(.Object, fun, ..., eo=NULL)
}

\arguments{
\item{.Object}{a sleigh class object}
\item{fun}{function to be evaluated by sleigh workers}
\item{...}{optional fixed argument(s)}
\item{eo}{additional options, see details}
}

\details{
eo argument can be defined as a list containing 'blocking' variable,
or an environment variable with 'blocking' field. 
Blocking indicates whether eachWorker will block or not for the results 
to return. 
By default, blocking is set to TRUE, which means eachWorker does not 
return until every worker finish executing function fun. 
If blocking mode is set to FALSE,  then eachWorker returns immediately with a 
sleighPending class object. You can then use this object to monitor
the stauts of the tasks, and eventually used it to retrieve results. You
must wait for the results to be compeleted and retrieved using waitSleigh method
before submitted more tasks to the sleigh workspace.
}

\examples{
\dontrun{
# example 1
s = sleigh()
eachWorker(s, function() {x<<-1})
# eachElem can use global variable x initialized by eachWorker.
eachElem(s, function(y) {x+y}, list(1:10)) 

# example 2
options = list(blocking=0)
sp = eachWorker(s, function(z) {Sys.sleep(100)}, eo=options)
checkSleigh(sp)
waitSleigh(sp)	

# example 3
# pass in fixed arguments to eachWorker
eachWorker(s, function(x, y) {x+y}, 10, 5)
}
}

\keyword{methods}
