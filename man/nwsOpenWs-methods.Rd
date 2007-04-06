\name{nwsOpenWs}
\docType{methods}
\alias{nwsOpenWs}
\alias{nwsOpenWs-methods}
\alias{nwsOpenWs,nwsServer-method}
\title{nwsServer Class Method}
\description{
Create and owned a netWorkSpace, if it does not already exist. If the netWorkSpace already
exists, but no one owns it, then caller will claim the ownership of the netWorkspace. 
If the netWorkSpace already exists and someone already claimed the ownership of it, then caller
simply make connection to the netWorkspace. 
}

\usage{
\S4method{nwsOpenWs}{nwsServer}(.Object, wsName, space=NULL, ...)
}

\arguments{
\item{.Object}{a nwsServer class object}
\item{wsName}{name of the netWorkSpace to be created}
\item{space}{a netWorkSpace class object}
\item{...}{optional arguments related to the persistent state of a netWorkSpace}
}

\details{
If space argument is not provided, then create a shared netWorkSpace with the name wsName
to the server represented by .Object. Otherwise, use the existing netWorkSpace object
provided by argument space.

Optional argument, 'persistent', is a boolean value indicating whether the shared
netWorkSpace is persistent or not. If the netWorkSpace is persisent (TRUE or 1), then
the netWorkspace is not purged when the owner of the shared netWorkSpace exited. Otherwise,
the netWorkSpace will be purged.
}


\seealso{\code{\link{nwsUseWs}}}

\examples{
\dontrun{
# example 1
nwss <- nwsServer()
ws <- nwsOpenWs(nwss, "nws example")

# example 2
xs <- nwsOpenWs(nwss, wsName='nws example', space=ws)

# example 3
ys <- nwsOpenWs(nwss, "persistent space", persistent=TRUE)
}
}

\keyword{methods}
