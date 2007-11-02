\name{netWorkSpace}
\docType{class}
\alias{netWorkSpace-class}
\alias{netWorkSpace}
\alias{initialize,netWorkSpace-method}

\title{Class ``netWorkSpace"}
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
    \item{nwsFetch}{\code{signature(.Object = "netWorkSpace")}: fetch a value of a workspace variable.}
    \item{nwsFetchTry}{\code{signature(.Object = "netWorkSpace")}: try to fetch a value of a workspace variable.}
    \item{nwsFind}{\code{signature(.Object = "netWorkSpace")}: find a value of a workspace variable.}
    \item{nwsFindTry}{\code{signature(.Object = "netWorkSpace")}: try to find a value of a workspace variable.}
    \item{nwsStore}{\code{signature(.Object = "netWorkSpace")}: store a value into a workspace variable.}
    \item{nwsFetchFile}{\code{signature(.Object = "netWorkSpace")}: fetch a value of a workspace variable and write it to a file.}
    \item{nwsFetchTryFile}{\code{signature(.Object = "netWorkSpace")}: try to fetch a value of a workspace variable and write it to a file.}
    \item{nwsFindFile}{\code{signature(.Object = "netWorkSpace")}: find a value of a workspace variable and write it to a file.}
    \item{nwsFindTryFile}{\code{signature(.Object = "netWorkSpace")}: try to find a value of a workspace variable and write it to a file.}
    \item{nwsStoreFile}{\code{signature(.Object = "netWorkSpace")}: store data from a file into a workspace variable.}
    \item{nwsIFetch}{\code{signature(.Object = "netWorkSpace")}:
      create a function that acts as a destructive iterator over the values of the specified variable.}
    \item{nwsIFetchTry}{\code{signature(.Object = "netWorkSpace")}:
      create a function that acts as a destructive iterator over the values of the specified variable.}
    \item{nwsIFind}{\code{signature(.Object = "netWorkSpace")}:
      create a function that acts as a non-destructive iterator over the values of the specified variable.}
    \item{nwsIFindTry}{\code{signature(.Object = "netWorkSpace")}:
      create a function that acts as a non-destructive iterator over the values of the specified variable.}
    \item{nwsClose}{\code{signature(.Object = "netWorkSpace")}: close the connection to the NWS server.}
    \item{nwsDeclare}{\code{signature(.Object = "netWorkSpace")}: declare the mode of a workspace variable.}
    \item{nwsDeleteVar}{\code{signature(.Object = "netWorkSpace")}: delete a variable from a workspace.}
    \item{nwsListVars}{\code{signature(.Object = "netWorkSpace")}: list all variables in a workspace.}
    \item{ServerObject}{\code{signature(.Object = "netWorkSpace")}: return the associated NwsServer object.}
    \item{nwsVariable}{\code{signature(.Object = "netWorkSpace")}: create an Active Binding for a NetWorkSpace Variable}
    \item{nwsWsName}{\code{signature(.Object = "netWorkSpace")}: return the name of the workspace.}
  }
}

\examples{
\dontrun{
# To create a new workspace with the name "my space" use:
ws = netWorkSpace('my space')

# To create a new workspace called "my space2" on nws server 
# running on port 8245 on machine zeus:
ws2 = netWorkSpace(wsName='my space2', serverHost='zeus', port=8245)
}
}

\keyword{classes}
\keyword{methods}
