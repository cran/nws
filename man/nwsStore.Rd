\name{nwsStore-methods}
\docType{methods}
\alias{nwsStore}
\alias{nwsStore,netWorkSpace-method}
\title{netWorkSpace Class Method}
\description{
Store something to the shared netWorkSpace.
}

\usage{
nwsStore(.Object, xName, xVal)
}

\arguments{
\item{.Object}{a netWorkSpace class object}
\item{xName}{name of the variable to be stored}
\item{xVal}{value to be stored}
}


\details{
nwsStore associates the value xVal with the variable xName in the
shared netWorkSpace corresponding to .Object, thereby making the
value available to all the distributed R processes.  If a mode
has not already been set for xName, 'fifo' will be used (see
\code{\link{nwsDeclare}}).

Note that, by default ('fifo' mode), nwsStore is not idempotent: repeating 
nwsStore(nws, xName, xVal) will add additional values to the set of
values associated with the variable named xName. See the examples
below for details.
}

\examples{
\dontrun{
ws = new('netWorkSpace', 'nws example')

# To store value 5 bound to variable 'x' on the netWorkSpace 'ws'
# (If 'x' was declared, then its mode is inherited, 
# otherwise 'x' uses the default mode 'fifo')
nwsStore(ws, 'x', 5)

# store 10 values associate with variable y to the netWorkSpace
for (i in 1:10)
nwsStore(ws, 'y', i)

# retrieve 10 values associate with variable y from the netWorkSpace
for (i in 1:10)
print(nwsFetch(ws, 'y'))
}
}

\keyword{methods}
