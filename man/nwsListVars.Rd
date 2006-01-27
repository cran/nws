\name{nwsListVars}
\docType{methods}
\alias{nwsListVars}
\alias{nwsListVars,netWorkSpace-method}
\title{netWorkSpace Class Method}
\description{
List variables in a netWorkSpace.
}

\usage{
nwsListVars(.Object, wsName='')
}

\arguments{
\item{.Object}{a netWorkSpace class object}
\item{wsName}{name of the netWorkSpace}
}

\details{
Return listing of variables in the netWorkSpace with the name passed by wsName argument.
If wsName is empty, then return variables in the netWorkSpace 
represented by .Object.
To see list output clearly, use: write(nwsListVars(.Object), stdout()).
}

\examples{
\dontrun{
ws = new('netWorkSpace', 'nws example')
write(nwsListVars(ws), stdout())
}
}

\keyword{methods}
