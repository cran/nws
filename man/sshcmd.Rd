\name{sshcmd}
\alias{sshcmd}
\title{sleigh Auxiliary Function}
\description{
This function indicates that SSH will be used to launch sleigh workers.
}
\seealso{\code{\link{sleigh}}}

\details{
This function is only meant to be passed through optional argument, 'launch' 
in the sleigh constructor.
}

\examples{
\dontrun{
# Both master and sleigh workers run on Linux.
s = sleigh(launch=sshcmd)

# master runs on Windows and sleigh workers run on Linux. 
# Since Windows and Linux have different file structure, 
# we need to specify the location of worker scripts.
s = sleigh(launch=sshcmd, scriptDir='/usr/local/lib/R/library/nws/bin')
}
}

\keyword{methods}
\keyword{internal}
