\name{netWorkSpace}
\docType{class}
\alias{netWorkSpace-class}
\alias{netWorkSpace}
\alias{initialize,netWorkSpace-method}

\title{Class "netWorkSpace"}
\description{Class representing netWorkSpace.}
\section{Objects from the Class}{
Objects can be created by calls of the form \cr \code{new("netWorkSpace", wsName, serverHost, port, useUse, serverWrap, ...)}.
\describe{
  \item wsName: name of the netWorkSpace to be created.
  \item serverHost: host name of the server this netWorkSpace will be connected to. By default, local machine is used.
  \item port: port number of the server this netWorkSpace will be connected to. Default port number is 8765.
  \item useUse: a boolean value indicating whether ownership will be claimed for this netWorkSpace. By default,
	useUse=FALSE, which means ownership will be claimed.
  \item serverWrap: a netWorkSpaces server object. Reuse an existing server connection, instead of creating a new server connection.
}

}

\section{Slots}{
  \describe{
    \item{\code{server}:}{Object of class \code{"nwsServer"} representation of the server that this netWorkSpace connects to.}
    \item{\code{wsName}:}{Object of class \code{"character"} representation of this netWorkSpace's name.}
  }
}

\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "netWorkSpace")}: netWorkSpace class constructor.}
    \item{nwsClose}{\code{signature(.Object = "netWorkSpace")}: close connection to the shared netWorkSpace server.}
    \item{nwsDeclare}{\code{signature(.Object = "netWorkSpace")}: declare a variable with specific mode in the shared netWorkSpace.}
    \item{nwsDeleteVar}{\code{signature(.Object = "netWorkSpace")}: delete a variable from the shared netWorkSpace.}
    \item{nwsFetch}{\code{signature(.Object = "netWorkSpace")}: fetch value associates with a variable from the shared netWorkSpace.}
    \item{nwsFetchTry}{\code{signature(.Object = "netWorkSpace")}:
      attempt to fetch value associate with a variable from the shared netWorkSpace; a non-blocking version of fetch.}
    \item{nwsFind}{\code{signature(.Object = "netWorkSpace")}: find value associates with a variable from the shared netWorkSpace.}
    \item{nwsFindTry}{\code{signature(.Object = "netWorkSpace")}:
      attempt to find value associates with a variable from the shared netWorkSpace;  a non-blocking version of find.}
    \item{nwsListVars}{\code{signature(.Object = "netWorkSpace")}: list variables in a netWorkSpace.}
    \item{nwsStore}{\code{signature(.Object = "netWorkSpace")}: store value associate with a variable to the shared netWorkSpace.}
    \item{nwsWsName}{\code{signature(.Object = "netWorkSpace")}: return the name of a netWorkSpace.}
    \item{close}{\code{signature(.Object = "netWorkSpace")}: alias to nwsClose.}
  }
}

\examples{
\dontrun{
# To create a new workspace with the name "my space" use:
ws = netWorkSpace('my space')
# Or,
ws = new('netWorkSpace', 'my space')

# To create a new workspace called "my space2" on nws server 
# running on port 8245 on machine zeus:
ws2 = netWorkSpace(wsName='my space2', serverHost='zeus', port=8245)
}
}

\keyword{classes}
\keyword{methods}
