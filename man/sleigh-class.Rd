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
executed in parallel using the eachElem and eachWorker methods.

The sleigh workers are started when the sleigh object is
constructed.  When tasks are submitted to the sleigh, using the
eachWorker and eachElem methods, the workers will execute the tasks,
and return the results.  When the stopSleigh method is called, the
workers are stopped.

Note that a given R program can create multiple sleigh objects,
which will each have it's own set of workers.  This could be useful
if tasks have different requirements.  For example, you could create
a Linux sleigh and a Windows sleigh, and Windows-specific tasks only
to your Windows sleigh.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{sleigh(...)},
where `...' can be one or more of the following named arguments:
\item nodeList: a list of hosts that workers will be created. Note that
this argument is ignored when launch equals `local'.  The default value
is c('localhost', 'localhost', 'localhost').
\item workerCount: number of workers that will be created. This argument
is only relevant when launch equals `local'.  The default value is 3.
\item launch: method to launch remote workers.  This can be set to the
string 'local', 'web', or to a function object.  The function will be
called once for each worker listed in nodeList.  It will be passed two
arguments: a name from nodeList, and an environment object that was
constructed by merging defaultSleighOptions with the arguments that were
passed to the sleigh constructor.  The function should return a character
mode vector, where the first element is the command to execute, and the
subsequent elements are the command arguments.  For example, the function
could return the vector: c('ssh', '-f', host), where 'host' is the first
argument to the launch function.  This isn't the complete command that
will be executed: it's the 'remote execution' portion of the command.
The sleigh constructor will add rest of the command based on the
scriptExec argument.  Note that the command is expected to return after
launching the worker.  That is why the ssh -f option is used in the example.

The default value of launch is `local'.

\item nwsHost: host name of the netWorkSpaces server. Default is the
machine where sleigh starts up.
\item nwsPort: port number of the netWorkSpaces server. Default is 8765.
\item scriptExec: command to execute worker script.  Default uses
scriptcmd function on Windows, and uses envcmd function on other platforms. 
\item scriptDir: location of the sleigh worker script. Default is the bin
directory under where nws library is installed on the system. If library
cannot be found, then use current working directory. 
\item scriptName: worker script file name. Default is RNWSSleighWorker.py
on Windows, and RNWSSleighWorker.sh on other platforms. 
\item workingDir: worker's working directory. Default to master's current
working directory.
\item logDir: location where log files will be stored. Default is NULL. 
\item outfile: remote workers' standard errors will be redirected to this
file. Default is NULL.
\item wsNameTemplate: template name to create sleigh workspace. \cr
Default is 'sleigh\_ride\_\%010d'.
\item user: user name used for remote execution.  Default is NULL.
\item verbose: a boolean value indicating whether to print out debug
messages. Default is FALSE.
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
    shutdown workers and remove sleigh workspace.}
    \item{workerCount}{\code{signature(.Object = "sleigh")}:
    get number of workers started in sleigh.}
  }
}

\section{Details}{
There are five different standard launch types ('local', sshcmd, rshcmd,
lsfcmd, and 'web') to tailor client's working environment.  This is done
by setting launch variable to a function (sshcmd, rshcmd, or lsfcmd)
or a string ('local' and 'web').  See the examples section.
}

\examples{
\dontrun{
# Default option: create three sleigh workers on local host:
s <- sleigh()
# which is equivalent to:
s <- sleigh(launch='local')

# Create sleigh workers on multiple machines using SSH:
s <- sleigh(c('n1', 'n2', 'n3'), launch=sshcmd)

# Use the LSF bsub command to launch ten workers:
s <- sleigh(launch=lsfcmd, rep('fake', 10))

# Use web launch:
s <- sleigh(launch='web')
}
}

\keyword{classes}
