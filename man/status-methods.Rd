\name{status}
\docType{methods}
\alias{status}
\alias{status-methods}
\alias{status,sleigh-method}
\title{sleigh Class Method}
\description{
  Check status of sleigh workers and return a list that contains two
  values: numWorkers and closed.
  If status is zero, then some workers failed to start.
  If status is one, then either all workers have started or 
  closeGroup is set to TRUE. numWorkers indicates the number of workers started. 
}

\usage{
\S4method{status}{sleigh}(.Object, closeGroup=FALSE, timeout=0)
}

\arguments{
  \item{.Object}{a sleigh class object}
  \item{closeGroup}{a boolean value indicating whether to close the
    worker group}
  \item{timeout}{timeout value measure in seconds}
}

\details{
closeGroup is set to FALSE by default. If closeGroup is set to FALSE,
then all workers must wait for each other to start before they can start
working on tasks. If closeGroup is set to TRUE, no new workers may
join the group after timeout value has expired. Once the group is closed,
all launched workers can start working on tasks. 
 
timeout indicates how long to wait and check on the status
of workers.
}

\examples{
\dontrun{
# example 1
# one available machine and one non-existent machine
s <- sleigh(c('localhost', 'noname'))
slist <- status(s)
# slist$numWorkers = the number of worker started
# slist$closed = whether the worker group is closed or not. 

# example 2
# check the status of worker group after 20 seconds
slist <- status(s, timeout=20)

# example 3
# close the group after 10 seconds, regardless of whether
# all workers have started up successfully.
slist <- status(s, closeGroup=TRUE, timeout=10)
}
}


\keyword{methods}

