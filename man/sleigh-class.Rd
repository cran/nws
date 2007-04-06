\name{sleigh}
\docType{class}
\alias{sleigh-class}
\alias{sleigh}
\alias{initialize,sleigh-method}
\alias{initialize}
\title{Class ``sleigh"}
\description{Class representing sleigh.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("sleigh", ...)}, where `...' can
be one or more of the following named arguments. 
\item nodeList: a list of hosts that workers will be created. This argument is irrelevant when launch equals `local'. Default is to start up three workers on local machine.
\item workerCount: number of workers that will be created. This argument is only relevant when launch equals `local'. Default is three workers. 
\item launch: method to launch remote workers. Default is `local'.
\item nwsHost: host name of the netWorkSpaces server. Default is the machine where sleigh starts up.
\item nwsPort: port number of the netWorkSpaces server. Default is 8765.
\item scriptExec: command to execute worker script.  Default uses scriptcmd function on Windows, and uses envcmd function on other platforms. 
\item scriptDir: location of the sleigh worker script. Default is the bin directory under where nws library is installed on the system. If library cannot be found, then
use current working directory. 
\item scriptName: worker script file name. Default is RNWSSleighWorker.py on Windows, and RNWSSleighWorker.sh on other platforms. 
\item workingDir: worker's working directory. Default to master's current working directory.
\item logDir: location where log files will be stored. Default is NULL. 
\item outfile: remote workers' standard errors will be redirected to this file. Default is NULL.
\item wsNameTemplate: template name to create sleigh workspace. \cr Default is 'sleigh\_ride\_\%010d'.
\item user: user name. Default is the value returned from Sys.info()$[$'user'$]$.
\item verbose: a boolean value indicating whether to print out debug messages. Default is FALSE.
}
}
\section{Slots}{
  \describe{
    \item{\code{nodeList}:}{Object of class \code{"character"} representation of a list of host names where workers are created.}
    \item{\code{nws}:}{Object of class \code{"netWorkSpace"} representation of the sleigh workspace.}
    \item{\code{nwsName}:}{Object of class \code{"character"} representation of the sleigh workspace name.}
    \item{\code{nwss}:}{Object of class \code{"nwsServer"} representation of the netWorkSpaces server that this sleigh workspace connects to.}
    \item{\code{state}:}{Object of class \code{"environment"} representation of the sleigh state.}
  }
}

\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "sleigh")}: sleigh class constructor.}
    \item{eachElem}{\code{signature(.Object = "sleigh")}: evaluate the given function with multiple argument sets using the workers in sleigh.}
    \item{eachWorker}{\code{signature(.Object = "sleigh")}: evaluate the
      given function exactly once for each worker in sleigh.}
    \item{stopSleigh}{\code{signature(.Object = "sleigh")}: shutdown workers and remove sleigh workspace.}
    \item{rankCount}{\code{signature(.Object = "sleigh")}: get sleigh's rankCount.}
    \item{workerCount}{\code{signature(.Object = "sleigh")}: get number of workers started in sleigh.}
  }
}

\section{Details}{
There are five different launch types ('local', sshcmd, rshcmd, lsfcmd,
and 'web') to tailor client's working environment. This is done by setting
launch variable to a function (sshcmd, rshcmd, or lsfcmd) or a string
('local' and 'web'). see examples section.
}

\examples{
\dontrun{
# Default option: create three sleigh workers on local host
s <- sleigh()
# Or, 
s <- new('sleigh')
# Or,
s <- sleigh(launch='local')


# Create sleigh workers on multiple machines using SSH
s <- sleigh(c('n1', 'n2', 'n3'), launch=sshcmd)

# Use LSF to remote login to remote machines. 
s <- sleigh(launch=lsfcmd)

# Use web launch
s <- sleigh(launch='web')

}
}

\keyword{classes}
\keyword{methods}
