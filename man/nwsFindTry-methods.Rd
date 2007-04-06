\name{nwsFindTry}
\docType{methods}
\alias{nwsFindTry}
\alias{nwsFindTry-methods}
\alias{nwsFindTry,netWorkSpace-method}
\title{netWorkSpace Class Method}

\description{
Attempt to find value associates with a variable from the shared netWorkSpace; 
a non-blocking version of \code{\link{nwsFind}}.
}

\usage{
\S4method{nwsFindTry}{netWorkSpace}(.Object, xName, defaultVal=NULL)
}

\arguments{
\item{.Object}{a netWorkSpace class object}
\item{xName}{name of variable to be found}
\item{defaultVal}{value to return if xName is not found}
}

\details{
Look in the shared netWorkSpace for a value bound to xName. 
Once found, return the value associated with xName, but 
the value is not removed from the shared netWorkSpace.
If there is more than one value associated with xName, the
particular value returned depends on varName's behavior. See
\code{\link{nwsDeclare}} for details.

If variable is not found, return immediately rather than block on the 
operation (as in the case of \code{\link{nwsFind}}), and
the value of argument defaultVal is returned. 
By default, defaultVal is NULL.
}

\seealso{\code{\link{nwsDeclare}}, \code{\link{nwsFind}}}


\examples{
\dontrun{
ws <- netWorkSpace('nws example')
x <- nwsFindTry(ws, 'abc', -1)
}
}

\keyword{methods}
