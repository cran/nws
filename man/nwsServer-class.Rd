\name{nwsServer-class}
\docType{class}
\alias{nwsServer-class}
\alias{initialize,nwsServer-method}
\title{nwsServer class}
\description{Class representing nwsServer.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("nwsServer", serverHost, port)}.
\describe{
   \item {\bf serverHost:} server host name. Default value is local machine.
   \item {\bf port:} server port number. Default value is 8765.
}
}
\section{Slots}{
  \describe{
    \item{\code{nwsSocket}:}{Object of class \code{"ANY"} representation of the socket connection to the server.}
    \item{\code{port}:}{Object of class \code{"numeric"} representation of the server port number.}
    \item{\code{serverHost}:}{Object of class \code{"character"} representation of the server host name.}
  }
}
\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "nwsServer")}: nwsServer class constructor.}
    \item{nwsDeleteWs}{\code{signature(.Object = "nwsServer")}: delete a netWorkSpace from the server.}
    \item{nwsListWss}{\code{signature(.Object = "nwsServer")}: list all netWorkSpaces in the server.}
    \item{nwsMktempWs}{\code{signature(.Object = "nwsServer")}: create a unique temporary workspace using the default or specified template.}
    \item{nwsOpenWs}{\code{signature(.Object = "nwsServer")}: create and owned a netWorkSpace.}
    \item{nwsUseWs}{\code{signature(.Object = "nwsServer")}: connect to a netWorkSpace but does not claim ownership.}
  }
}

\examples{
\dontrun{
# example 1
nwss = new("nwsServer")

# example 2
nwss = new("nwsServer", serverHost="node1", port=5555)
}
}
\keyword{classes}
