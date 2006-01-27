\name{sleigh-class}
\docType{class}
\alias{sleigh-class}
\alias{sleigh}
\alias{initialize,sleigh-method}
\alias{initialize}

\title{Class "sleigh"}
\description{Class representing sleigh}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("sleigh", nodeList, ...)}.
\item {\bf nodeList:} a list of hosts that workers will be created. Default is to start up three workers on local machine.
\item {\bf launch:} method to launch remote workers. Default is ssh.
\item {\bf master:} master sleigh's machine name. Default is the value returned from Sys.info()$[$'nodename'$]$.
\item {\bf nwsWsName:} template name to create sleigh workspace. Default is 'sleigh\_ride\_\%010d'.
\item {\bf nwsHost:} host name of the netWorkSpaces server. Default is the machine where sleigh starts up.
\item {\bf nwsPort:} port number of the netWorkSpaces server. Default is 8765.
\item {\bf outfile:} remote workers' standard errors will be redirected to this file. Default is /dev/null.
\item {\bf scriptDir:} location of the sleigh worker script.
\item {\bf user:} user name. Default is the value returned from Sys.info()$[$'user'$]$.
}
}
\section{Slots}{
  \describe{
    \item{\code{nodeList}:}{Object of class \code{"character"} representation of a list of host names where workers are created.}
    \item{\code{nws}:}{Object of class \code{"netWorkSpace"} representation of the sleigh workspace.}
    \item{\code{nwsName}:}{Object of class \code{"character"} representation of the sleigh workspace name.}
    \item{\code{nwss}:}{Object of class \code{"nwsServer"} representation of the netWorkSpaces server that this sleigh workspace connects to.}
    \item{\code{options}:}{Object of class \code{"environment"} representation of the optional enviroment variables.}
    \item{\code{state}:}{Object of class \code{"environment"} representation of the sleigh state.}
    \item{\code{workerCount}:}{Object of class \code{"numeric"} representation of the number of sleigh workers.}
  }
}

\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "sleigh")}: sleigh class constructor.}
    \item{eachElem}{\code{signature(.Object = "sleigh")}: evaluate the given function with multiple argument sets using the workers in sleigh.}
    \item{eachWorker}{\code{signature(.Object = "sleigh")}: evaluate given function exactly once for each worker in sleigh.}
    \item{stopSleigh}{\code{signature(.Object = "sleigh")}: shutdown workers and remove sleigh workspace.}
  }
}

\section{Details}{
There are three different launch methods (ssh, lsf, and web) to tailor client's working environment. see examples section.
}

\examples{
\dontrun{
# Default option: create three sleigh workers on local host
s = sleigh()
# Or, 
s = new('sleigh')


# Create sleigh workers on multiple machines
s = sleigh(c('n1', 'n2', 'n3'))

# Use LSF instead of SSH for remote login. 
# to login to remote machines. 
s = sleigh(launch=lsfcmd)

# Use web launch
s = sleigh(launch='web')

}
}

\keyword{classes}
\keyword{methods}
