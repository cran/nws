\name{launch}
\alias{launch}
\title{Sleigh Auxiliary Function}
\description{
  Setup appropropriate worker accounting information, and
  start the worker loop. 
}

\details{
  The only time clients ever need to invoke this function directly is when web launch is used. 
  The 'runMe' variable contains value, which is to execute launch function. 
  For other launch types such as SSH and local launch, launch function is invoked within cmdLaunch.
}
\seealso{\code{\link{cmdLaunch}}}

\keyword{methods}
\keyword{internal}
