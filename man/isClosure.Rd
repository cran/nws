\name{isClosure}
\alias{isClosure}
\title{isClosure Function}
\description{
This is a heuristic function that is used by eachWorker and eachElem
to guess if the worker function is a closure.
}

\details{
If the "closure" option wasn't specified via the "eo" argument to
eachWorker or eachElem, then this function is used to guess if
the worker function is a closure.  It can be very useful to use
a closures with eachWorker and eachElem, but if not used properly,
you could accidentally include a lot of unnecessary data in the tasks,
thus hurting your performance.

This function is included as a development tool so you can manually
test your worker functions.  This could be useful if you are getting
a warning from eachWorker or eachElem, and are trying to determine
how to modify the function or set the "closure" option.
}

\examples{
# this should return FALSE
isClosure(sqrt)
f <- function(x) function(y) x + y
g <- f(1)
# this should return TRUE
isClosure(g)
}

\keyword{debugging}
\keyword{utilities}
