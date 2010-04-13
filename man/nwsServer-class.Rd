\name{nwsServer}
\docType{class}
\alias{nwsServer-class}
\alias{nwsServer}
\alias{initialize,nwsServer-method}
\title{Class Representing NetWorkSpaces Server}
\description{Class representing nwsServer.}
\section{Objects from the Class}{
Objects can be created by calls of the form \cr
\code{nwsServer(serverHost, port)}\cr
or\cr
\code{new("nwsServer", serverHost, port)}.
\describe{
   \item{\code{serverHost}:}{server host name. Default value is \code{"localhost"}.}
   \item{\code{port}:}{server port number. Default value is \code{8765}.}
}
}
\section{Slots}{
  \describe{
    \item{\code{cookieProtocol}:}{Currently not used.}
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
nwss = nwsServer()
# Or,
nwss = new("nwsServer")

# example 2
nwss = nwsServer(serverHost="node1", port=5555)
}
}
\keyword{classes}
