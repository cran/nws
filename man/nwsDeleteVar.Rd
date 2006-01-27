\name{nwsDeleteVar}
\docType{methods}
\alias{nwsDeleteVar}
\alias{nwsDeleteVar,netWorkSpace-method}
\title{netWorkSpace Class Method}
\description{
Delete a variable from the shared netWorkSpace.
}

\usage{
nwsDeleteVar(.Object, xName)
}

\arguments{
  \item{.Object}{a netWorkSpace class object}
  \item{xName}{name of the variable to be deleted}
}

\examples{
\dontrun{
ws = new('netWorkSpace', 'nws example')
nwsStore(ws, 'x', 10)
nwsDeleteVar(ws, 'x')
}
}

\keyword{methods}
