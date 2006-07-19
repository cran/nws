\name{nwsDeclare}
\docType{methods}
\alias{nwsDeclare}
\alias{nwsDeclare-methods}
\alias{nwsDeclare,netWorkSpace-method}
\title{netWorkSpace Class Method}
\description{
Declare a variable with particular mode in a shared netWorkSpace.
}

\usage{
  nwsDeclare(.Object, xName, mode)
}

\arguments{
  \item{.Object}{a netWorkSpace class object}
  \item{xName}{name of variable to be declared}
  \item{mode}{mode of the variable, see details}
}

\details{
If xName has not already been declared in the netWorkSpace, the behavior
of xName will be determined by mode. Mode can be 'fifo',
'lifo', 'multi', or 'single'. In the first three cases,
multiple values can be associated with xName. When a value is
retrieved for xName, the oldest value stored will be used in
'fifo' mode, the youngest in 'lifo' mode, and a
nondeterministic choice will be made in 'multi' mode. In
'single' mode, only the most recent value is retained.
}

\examples{
\dontrun{
ws = new('netWorkSpace', 'nws example')
nwsDeclare(ws, 'pi', 'single')
nwsStore(ws, 'pi', 2.171828182)
nwsStore(ws, 'pi', 3.141592654)

# nwsListVars(ws) will show that only the most recent value of pi is retained
}
}

\keyword{methods}
