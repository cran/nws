\name{rshcmd}
\alias{rshcmd}
\title{Sleigh Auxiliary Function}
\description{
This function indicates that RSH will be used to launch sleigh workers. 
}
\seealso{\code{\link{sleigh}}}

\details{
This function is only meant to be passed through optional argument 'launch'
in the sleigh constructor.
}

\examples{
\dontrun{
s = sleigh(launch=rshcmd)
}
}

\keyword{methods}
\keyword{internal}
