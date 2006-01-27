\name{workerLoop}
\alias{workerLoop}
\title{sleigh Auxiliary Function}
\description{
This function is used by workers to wait forever on tasks in the sleigh
workspace. Worker then works on the retrieved tasks and 
return results back to the sleigh workspace. 
This loop ends when stopSleigh is invoked.
}

\details{
This function is used in the sleigh worker script and executed by workers in the web launch mode. 
}

\keyword{methods}
\keyword{internal}