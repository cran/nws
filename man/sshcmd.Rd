\name{sshcmd}
\alias{sshcmd}
\title{sleigh Auxiliary Function}
\description{
This function is used to indicate SSH will be used to launch sleigh workers. 
}
\seealso{\code{\link{sleigh}}}

\details{
This function is only meant to be passed into sleigh constructor.
sshcmd is also the default launch method for sleigh workers.
}

\examples{
\dontrun{
s = sleigh(launch=sshcmd)
# is equivalent to
s = sleigh()
}
}

\keyword{methods}
\keyword{internal}
