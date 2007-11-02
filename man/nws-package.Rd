\name{nws-package}
\alias{nws-package}
\alias{nwsPro-package}
\alias{nws}
\alias{nwsPro}
\docType{package}
\title{
The NetWorkSpaces Package
}
\description{
NetWorkSpaces makes it easy to create and experiment with
parallel programs without requiring specialized tools or hardware.
You must install both the NetWorkSpaces server (on one machine) and
the R NetWorkSpaces package (on all machines involved in the computation).
The server is implemented using Python and Twisted (a Python package),
which are required.  To download and install the server, go to the
R NetWorkSpaces project on Source Forge at
\url{http://sourceforge.net/projects/nws-r}.

NetWorkSpaces Pro (nwsPro) is the commercial version of NetWorkSpaces
which has additional fault tolerance features.  More information is
available at \url{http://www.lindaspaces.com/products/NWS_overview.html}.
}
\details{
Further information is available in the following
\tabular{ll}{
\code{sleigh} \tab Create a sleigh for parallel execution\cr
\code{eachWorker} \tab Execute a function on each of the workers
in the sleigh\cr
\code{eachElem} \tab Execute a function for a set of arguments\cr
\code{export} \tab Export variables to worker processes\cr
}

For a complete list of functions with individual help pages,
use \code{library(help="nws")}.
}
\keyword{package}
