\name{sleigh}
\docType{class}
\alias{sleigh-class}
\alias{sleigh}
\alias{sleighPro}
\alias{initialize,sleigh-method}
\alias{initialize}
\title{Class ``sleigh"}
\description{
Represents a collection of R processes used to execute tasks.

The sleigh allows R functions to be
executed in parallel using the \code{eachElem} and 
\code{eachWorker} methods.

The sleigh workers are started when the sleigh object is
constructed.  When tasks are submitted to the sleigh, using the
\code{eachWorker} and \code{eachElem} methods, the workers execute the tasks
and return the results.  When the \code{stopSleigh} method is called, the
workers are stopped.

A given R program can create multiple sleigh objects,
each of which will have its own set of workers.  This can be useful
if tasks have different requirements.  For example, you could create
a Linux sleigh and a Windows sleigh, and submit Windows-specific tasks only
to your Windows sleigh.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \cr
\code{sleigh(...)} \cr
or \cr
\code{new("sleigh",...)}\cr
where \code{...} can be one or more of the following named arguments:
\describe{
\item{\code{nodeList}:}{a list of hosts on which workers will be created. 
This argument is ignored when \code{launch='local'}.  The default value
is \code{c('localhost', 'localhost', 'localhost')}.}
\item{\code{workerCount}:}{number of workers that will be created. This argument
is only relevant when \code{launch='local'}.  The default value is 3.}
\item{\code{launch}:}{method to launch remote workers.  This can be set to the
strings \code{'local'} (the default) or \code{'web'}, or to a function object.  

The function is called once for each worker listed in \code{nodeList}.  It 
is passed two arguments: a name from \code{nodeList}, and an 
environment object that was constructed by merging \code{defaultSleighOptions} 
with the arguments that were passed to the sleigh constructor.  The 
function should return a character vector, where the first element is the 
command to execute, and the subsequent elements are the command arguments.  
For example, the function could return the vector 
\code{c('ssh', '-f', 'host')}, where \code{'host'} is the first 
argument to the launch function.  This 
isn't the complete command to be executed; it's the 'remote execution' 
portion of the command.
The sleigh constructor will add the rest of the command based on the
\code{scriptExec} argument.  Note that the command is expected to return after
launching the worker.  That is why the ssh -f option is used in the example.

}

\item{\code{nwsHost}:}{host name of the netWorkSpaces server. Default is the
machine where sleigh starts up.}
\item{\code{nwsPort}:}{port number of the netWorkSpaces server. Default is 
8765.}
\item{\code{scriptExec}:}{command to execute worker script.  Default uses
\code{scriptcmd} function on Windows, and uses \code{envcmd} function on 
other platforms.}
\item{\code{scriptDir}:}{location of the sleigh worker script. Default is the 
bin directory under the nws package location. If library
cannot be found, then use current working directory.} 
\item{\code{scriptName}:}{worker script file name. Default is 
\code{RNWSSleighWorker.py}
on Windows, and \code{RNWSSleighWorker.sh} on other platforms.}
\item{\code{workingDir}:}{worker's working directory. Default is 
master's current working directory.}
\item{\code{logDir}:}{location where log files will be stored. Default is 
\code{NULL}.}
\item{\code{outfile}:}{remote workers' standard errors will be redirected to 
this file. Default is \code{NULL}.}
\item{\code{wsNameTemplate}:}{template name to create sleigh workspace. \cr
Default is \code{'sleigh_ride_\%010d'}.}
\item{\code{user}:}{user name used for remote execution.  Default is 
\code{NULL}.}
\item{\code{verbose}:}{a logical value indicating whether to print out debug
messages. Default is \code{FALSE}.}
}
}
\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "sleigh")}:
    sleigh class constructor.}
    \item{eachElem}{\code{signature(.Object = "sleigh")}:
    evaluate the given function with multiple argument sets using
    the workers in sleigh.}
    \item{eachWorker}{\code{signature(.Object = "sleigh")}:
    evaluate the given function exactly once for each worker in sleigh.}
    \item{rankCount}{\code{signature(.Object = "sleigh")}:
    get sleigh's rankCount.}
    \item{status}{\code{signature(.Object = "sleigh")}:
    return the status of the sleigh.}
    \item{stopSleigh}{\code{signature(.Object = "sleigh")}:
    shut down workers and remove sleigh workspace.}
    \item{workerCount}{\code{signature(.Object = "sleigh")}:
    get number of workers started in sleigh.}
  }
}

\section{Details}{
There are six different standard launch types (\code{'local'}, \code{sshcmd}, 
\code{rshcmd}, \code{lsfcmd}, \code{sshforwardcmd}, and \code{'web'}) to 
tailor the client's working environment.  This is done
by setting launch variable to a function (\code{sshcmd}, \code{rshcmd}, 
\code{sshforwardcmd} or \code{lsfcmd})
or a string ('local' and 'web').  See the examples section.
}

\examples{
\dontrun{
# Default option: create three sleigh workers on local host:
s <- sleigh()
# which is equivalent to:
s <- sleigh(launch='local')

# Create sleigh workers on multiple machines using SSH:
s <- sleigh(nodeList=c('n1', 'n2', 'n3'), launch=sshcmd)

# Use the LSF bsub command to launch ten workers:
s <- sleigh(nodeList=rep('fake', 10), launch=lsfcmd)

# Use web launch:
s <- sleigh(launch='web')
}
}

\keyword{classes}
