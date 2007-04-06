\name{sleighPending}
\docType{class}
\alias{sleighPending-class}
\alias{sleighPending}
\alias{initialize,sleighPending-method}
\title{Class ``sleighPending"}
\description{Class representing sleighPending.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("sleighPending", nws, numTasks, bn, ss)}.
\describe{
  \item nws: netWorkSpace class object.
  \item numTasks: number of submitted tasks.
  \item bn: barrier names.
  \item ss: sleigh state.
}
}

\section{Slots}{
  \describe{
    \item{\code{nws}:}{Object of class \code{"netWorkSpace"} representation of the netWorkSpace class.}
    \item{\code{numTasks}:}{Object of class \code{"numeric"} representation of the number of pending tasks in sleigh.}
    \item{\code{barrierName}:}{Object of class \code{"character"} representation of the barrier name.}
    \item{\code{sleighState}:}{Object of class \code{"environment"} representation of the sleigh state.}
    \item{\code{state}:}{Object of class \code{"environment"} representation of the sleighPending state.}
  }
}
\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "sleighPending")}: sleighPending class constructor.}
    \item{checkSleigh}{\code{signature(.Object = "sleighPending")}: return the number of results yet to be generated for the pending sleigh job.}
    \item{waitSleigh}{\code{signature(.Object = "sleighPending")}: wait and block for the results to be generated for the pending sleigh job.}
  }
}

\details{
This class object is usually obtained from non-blocking eachElem
or non-blocking eachWorker. 
}

\seealso{ \code{\link{eachWorker}}, \code{\link{eachElem}} }


\keyword{classes}
\keyword{methods}
