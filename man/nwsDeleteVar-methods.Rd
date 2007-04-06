\name{nwsDeleteVar}
\docType{methods}
\alias{nwsDeleteVar}
\alias{nwsDeleteVar-methods}
\alias{nwsDeleteVar,netWorkSpace-method}
\title{netWorkSpace Class Method}
\description{
Delete a variable from the shared netWorkSpace.
}

\usage{
\S4method{nwsDeleteVar}{netWorkSpace}(.Object, xName)
}

\arguments{
  \item{.Object}{a netWorkSpace class object}
  \item{xName}{name of the variable to be deleted}
}

\examples{
\dontrun{
ws <- netWorkSpace('nws example')
nwsStore(ws, 'x', 10)
nwsDeleteVar(ws, 'x')
}
}

\keyword{methods}
