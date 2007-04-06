\name{nwsMktempWs-methods}
\docType{methods}
\alias{nwsMktempWs}
\alias{nwsMktempWs-methods}
\alias{nwsMktempWs,nwsServer-method}
\title{nwsServer class method}
\description{
Create a unique temporary netWorkSpace using the template string.
}

\usage{
\S4method{nwsMktempWs}{nwsServer}(.Object, wsNameTemplate)
}

\arguments{
\item{.Object}{a nwsServer class object}
\item{wsNameTemplate}{template for the netWorkSpace name}
}

\details{
nwsMktempWs(nwss, wsNameTemplate) returns the name of a temporary space
created on the netWorkSpaces server.
The template should contain a '\%d'-like construct which will be
replaced by a serial counter maintained by the server to generate
a unique new netWorkSpace name.
The user must then invoke nwsOpenWs() or nwsUseWs()
with this name to create  an object to acceess this workspace. 
WsNameTemplate defaults to '\_\_Rws\_\_\%010d'
}

\examples{
\dontrun{
s <- nwsServer()
tempWsName <- nwsMktempWs(s, 'temp_\%d')
ws <- nwsOpenWs(s, tempWsName)
}
}

\keyword{methods}
