\name{nwsListWss}
\docType{methods}
\alias{nwsListWss}
\alias{nwsListWss,nwsServer-method}
\title{nwsServer Class Method}
\description{
List all netWorkSpaces in the netWorkSpaces server.
}

\usage{
nwsListWss(.Object)
}

\arguments{
\item{.Object}{a nwsServer class object}
}

\details{
The return value is a text string containing a list of workspaces in the
netWorkSpaces server.
To see list output clearly, use: write(nwsListWss(.Object), stdout()).
}

\examples{
\dontrun{
   nwss = new('nwsServer')
   ws1 = nwsOpenWs(nwss, 'my space')
   ws2 = nwsOpenWs(nwss, 'other space')
   write(nwsListWss(nwss), stdout())
}
}

\keyword{methods}
