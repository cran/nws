\name{sshcmd}
\alias{sshcmd}
\title{Sleigh Auxiliary Function}
\description{
This function is used by the sleigh constructor when starting workers on
remote nodes using the ssh command.  Note that it doesn't actually start
any workers directly: it simply returns the program name and arguments
to start a worker on the specified node.
}

\usage{
sshcmd(host, options)
}

\note{
sshcmd is not intended to be called by the user.
It is called by the sleigh constructor when specified via the
sleigh launch argument.  You may want to execute it when debugging
your sleigh option settings, but that can also be accomplished by
setting the sleigh verbose argument to TRUE.
}

\details{
sshcmd currently uses the following options: 'user', 'wrapperDir',
'workerWrapper', and 'python'.  The 'user' option is the most useful.
}

\arguments{
\item{host}{Name of the worker machine to be started.}
\item{options}{An environment or list that contains information used to
construct the command.  See defaultSleighOptions for more information.}
}

\value{
A character mode vector, whose first element is the command that will be
executed to start the worker, and whose subsequent elements are the
arguments to the command.
}

\seealso{\code{\link{sleigh}}, \code{\link{defaultSleighOptions}}}

\examples{
\dontrun{
# Create a sleigh with workers on nodes n1 and n2 started via ssh:
s <- sleigh(launch=sshcmd, nodeList=c('n1', 'n2'))

# Same as previous, but as user 'frank':
s <- sleigh(launch=sshcmd, nodeList=c('n1', 'n2'), user='frank')
}
}

\keyword{utilities}
