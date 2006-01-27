\name{eachElem}
\docType{methods}
\alias{eachElem-methods}
\alias{eachElem,sleigh-method}
\alias{eachElem}
\title{sleigh Class Method}
\description{
Evaluate the given function with multiple argument sets using the workers in sleigh.
}

\usage{
eachElem(.Object, fun, elementArgs=list(), fixedArgs=list(), eo=NULL)
}

\arguments{
\item{.Object}{a sleigh class object}
\item{fun}{function to be evaluated by sleigh workers}
\item{elementArgs}{variable arguments}
\item{fixedArgs}{fixed arguments}
\item{eo}{optional arguments, see details}
}

\details{
eachElem forms argument sets from vectors passed in elementArgs and fixedArgs. elementArgs
is a list of vectors (x, y, ... z), where all vectors must be of the same length. Argument set i consists of
the ith element of x, y, ... z. If fixedArgs is provided, then these additional arguments are
appended to the argument set. The ordering of arguments can be changed by using argPermute vector, which
is defined as part of eo argument (see below). 

eo argument can be defined as a list of three values, or an environment variable with three fields.
Those three fields are: argPermute, blocking, and loadFactor.

{\bf argPermute:} a vector of integers used to represent the ordering of arguments. \cr
For example: \cr
   eo = list(argPermute=c(3, 2, 1))\cr
   elemList=list(1:5, 11:15)\cr
   fixedArgs=list(1111)\cr
   reordering = function(x, y, z) \{paste(`(x, y, z) =', x, y, z)\}\cr
   eachElem(s, reordering, elementArgs=elementList, fixedArgs=fixedArgs, eo=eo)

   The original argument pairs without permute argument are (1, 11, 1111), (2, 12, 1111), 
   (3, 13, 1111), (4, 14, 1111), and (5, 15, 1111).
   With permute argument, the pairs would become (1111, 11, 1), (1111, 12, 2), (1111, 13, 3), 
   (1111, 14, 4), and (1111, 15, 5)


{\bf blocking:} set blocking mode to 0 or 1. 
   By default, blocking mode is set to 1, which means eachElem does not return until all tasks
   are finished. If blocking mode is 0, then eachElem returns immediately with a sleighPending 
   class object. You can then use this to monitor the status of the tasks, and eventually used
   it to retrieve results. You must wait for the results to be completed and retrieved 
   using waitSleigh method before submitting more tasks to the sleigh workspace.

{\bf loadFactor:} to restrict the number of tasks put out to the sleigh workspace.\cr
   If set, no more than loadFactor*number of worker tasks will be posted at once. This helps to
   control resource demands on the netWorkSpaces server. This field is mutually exclusive with 'blocking'
   set to 0. 
}

\examples{
\dontrun{
# simple hello world
s = sleigh()
hello = function(x) {paste("hello world", x, "from worker", SleighRank)}
eachElem(s, hello, list(1:10))


# matrix addition
A<-matrix(1:6, 3, 3)
B<-matrix(1:6, 3, 3)
elemList = list(1:3)
fixedArgs = list(A, B)
mat_add = function(index, A, B) {
   row = A[index,]+B[index,]
   list(row)
}

result=eachElem(s, mat_add, elementArgs=elemList, fixedArgs=fixedArgs)
print(result)

# clean up sleigh
stopSleigh(s)	
}
}

\keyword{methods}

