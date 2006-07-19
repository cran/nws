\name{scriptcmd}
\alias{scriptcmd}
\title{Sleigh Auxiliary Function}
\description{
This function is set through optional argument 'scriptExec' in the sleigh
constructor. It uses Python to execute worker script. 
}

\details{
This function indicates that configuration variables 
such as RSleighName and RSleighNwsName are passed as part of 
the worker script arguments rather than through env command. 
This is often useful on systems that do not support env command.

scriptcmd is the default choice to execute worker scripts on Windows.
}

\examples{
\dontrun{
s = sleigh(scriptExec=scriptcmd)
}
}

\keyword{methods}
\keyword{internal}
