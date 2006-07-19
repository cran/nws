\name{nwsUseWs}
\docType{methods}
\alias{nwsUseWs}
\alias{nwsUseWs-methods}
\alias{nwsUseWs,nwsServer-method}
\title{nwsServer Class Method}
\description{
Connect to a netWorkSpace but does not claim ownership. 
If the netWorkSpace does not exist, then it will be created, 
but no ownership will be claimed.
}

\usage{
\S4method{nwsUseWs}{nwsServer}(.Object, wsName, space=NULL)
}

\arguments{
\item{.Object}{a nwsServer class object}
\item{wsName}{name of the netWorkSpace to open}
\item{space}{a netWorkSpace class object}
}

\seealso{\code{\link{nwsOpenWs}}}

\examples{
\dontrun{
nwss = new("nwsServer")
ws = nwsUseWs(nwss, "nws example")
}
}

\keyword{methods}
