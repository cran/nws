#
# Copyright 2005, Scientific Computing Associates, Inc.
#
# This file is part of NetWorkSpaces for R.
#
# NetWorkSpaces for R  is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
#
#
# An enterprise-class, fully supported version of NetWorkSpace for R
# is available from Scientific Computing Associates Inc.
# Please contact sales@lindaspaces.com for details.
#

# We use alternating barriers to synchronize eachWorker
# invocations. Their names are common to workers and sleighs.
barrierNames <- list('barrier0', 'barrier1')

############################################################################
# Worker code.
#

# enquote and docall copied verbatim from snow
enquote <- function(x) as.call(list(as.name('quote'), x))

docall <- function(fun, args) {
  if ((is.character(fun) && length(fun) == 1) || is.name(fun))
    fun <- get(as.character(fun), env = .GlobalEnv, mode = 'function')
  do.call('fun', lapply(args, enquote))
}


openNwsWs <- function() {
  nwsHost <- Sys.getenv('RSleighNwsHost')
  nwsWsName <- Sys.getenv('RSleighNwsName')
  nwsPort <- as.integer(Sys.getenv('RSleighNwsPort'))

  # put these into global enironment so both worker loop and worker
  # code have access.
  SleighName <<- Sys.getenv('RSleighName')
  SleighNws <<- new('netWorkSpace', nwsWsName, nwsHost, nwsPort, useUse=TRUE)  
  SleighRank <<- as.integer(Sys.getenv('RSleighRank'))

  if (SleighRank == -1) {
    # implies web start up --- perhaps should make explicit
    SleighRank <<- nwsFetch(SleighNws, 'rankCount')+1
    nwsStore(SleighNws, 'rankCount', SleighRank)
    # initialize for monitoring
    node = sprintf('%s@%d', SleighName, SleighRank)
    nwsDeclare(SleighNws, node, 'single')
    nwsStore(SleighNws, node, '0')
    nodeList = nwsFetch(SleighNws, 'nodeList')
    if (nodeList == '.') {
      nodeList = node
    }
    else {
      nodeList = paste(nodeList, node)
    }
    nwsStore(SleighNws, 'nodeList', nodeList)
  }
}


workerLoop <- function(wsName) {
  bx <- 1
  openNwsWs()

  # monitoring stuff here
  tasks <- 0
  nodename <- paste(SleighName, SleighRank, sep='@')

  # post some info about this worker.
  nwsStore(SleighNws, 'worker info', list(sysInfo=Sys.info(), pid=Sys.getpid()))

  repeat {
    # update the number of tasks executed
    nwsStore(SleighNws, nodename, as.character(tasks))

    t <- nwsFetch(SleighNws, 'task')
    #print(t) # use to generate an informal log.
    if (!is.list(t)) { break }

    # do something more interesting than return try error if this fails?
    value <- try(docall(t$data$fun, t$data$args))

    # to debug a bit, uncomment the following.
    #print(value)

    nwsStore(SleighNws, 'result', list(type = 'VALUE', value = value, tag = t$tag))

    tasks <- tasks + 1

    if (t$barrier) {
      nwsFind(SleighNws, barrierNames[[bx]])
      bx <- bx%%2 + 1
    }
  }
}


############################################################################
#  Sleigh code
#

sshcmd <- function(user, host, options) {
  wrapper <- file.path(options$scriptDir, 'SleighWorkerWrapper.sh')
  if (file.access(wrapper) == 0) {
    sprintf("%s ssh -f -x -l %s %s", wrapper, user, host)
  }
  else {
    sprintf("ssh -f -x -l %s %s", user, host)
  }
}

rshcmd <- function(user, host, options) {
  sprintf("rsh -l %s %s", user, host)
}

lsfcmd <- function(user, host, options) {
  "bsub"
}

sleigh <- function(...) {
  new("sleigh",...)
}

# We side effect options here. at invocation, we generated a new env
# if desired.
blendOptions <- function(options, new) {
  if (! is.null(new)) {
    names <- names(new)
    for (i in seq(along = new))
      assign(names[i], new[[i]], env = options)
  }
  options
}

####
# sleighPending class
#
# represents a sleigh eachWorker/eachElem invocation in progress.
setClass('sleighPending',
         representation(nws='netWorkSpace', numTasks='numeric',
                        barrierName='character', sleighState='environment',
                        state='environment'))
setMethod('initialize', 'sleighPending',
function(.Object, nws, numTasks, bn, ss) {
  .Object@nws = nws
  .Object@numTasks = numTasks
  .Object@barrierName = bn
  .Object@sleighState = ss
  .Object@state = new.env()
  .Object@state$done = FALSE;
  .Object
})

setMethod('show', 'sleighPending', function(object) {
  cat('\n')
  cat('NWS Sleigh Pending Object\n')
  show(object@nws)

  cat('Tasks submitted:', object@numTasks, '\n', sep='')

  status <- checkSleigh(object)
  if (status == 0)
    message <- 'Work completed.'
  else
    message <- paste(status, 'jobs still pending.')

  cat('Status:\t', message, '\n', sep='')
  cat('\n')
})

# return the number of results still outstanding.
setGeneric('checkSleigh', function(.Object) standardGeneric('checkSleigh'))
setMethod('checkSleigh', 'sleighPending',
function(.Object) {
  if (.Object@state$done) return (0) # could argue either way here... .
  
  tc = textConnection(nwsListVars(.Object@nws))
  vl = scan(file=tc, what=list('', 1, 1, 1, ''), sep ='\t', quiet=TRUE)
  close(tc)

  for (x in 1:length(vl[[1]]))
    if ('result' == vl[[1]][x]) return (.Object@numTasks - vl[[2]][x])

  # didn't find the variable 'result' --- assume no results have yet
  # been generated.
  return (.Object@numTasks) 
})

# collect all results.
#
# note: a lot of code is duplicated here and in the non-blocking sections of
# eachWorker and eachElem. refactor?
setGeneric('waitSleigh', function(.Object) standardGeneric('waitSleigh'))
setMethod('waitSleigh', 'sleighPending', function(.Object) {
  if (.Object@state$done) {
    stop('results already gathered.')
  }

  val <- vector('list', length(.Object@numTasks))
  for (i in 1:.Object@numTasks) {
    r = nwsFetch(.Object@nws, 'result')
    if (is.null(r)) {
      stop('abnormal shutdown of sleigh workers???')
    }
    if (! is.null(r$value)) {
      val[[r$tag]] = r$value
    }
  }

  if (.Object@barrierName != '') {
    nwsStore(.Object@nws, .Object@barrierName, 1)
  }
  .Object@sleighState$occupied = FALSE
  .Object@state$done = TRUE
  val
})

####
# sleigh class
#
# represents a collection R processes running on a simple network of
# workstation pulling in tasks and generating results.

setClass('sleigh',
         representation(nodeList='character', nws='netWorkSpace',
                        nwsName='character', nwss='nwsServer',
                        options='environment', state='environment',
                        workerCount='numeric'))

# could this be a method --- it is invoked in the constructor?
addWorker <- function(machine, wsName, rank, options) {
  # basic idea is (or should be): if we can get the appropriate
  # RNWSleighWorker.sh script running on the remote node, we just need to
  # give it enough env info to take care of the rest
  script = file.path(options$scriptDir, 'RNWSSleighWorker.sh')

  envVars = paste(
    ' RSleighScriptDir=', options$scriptDir,
    ' RSleighNwsHost=', options$nwsHost,
    ' RSleighNwsName=', wsName,
    ' RSleighNwsPort=', options$nwsPort,
    ' RSleighRank=', rank,
    ' RSleighWorkerOut=', options$outfile,
    ' RSleighName=', machine,
    sep='')

  cmd = paste(options$launch(options$user, machine, options), 'env', envVars, script, "\n")
  # cat("Executing command: ", cmd )
  system(cmd)
}

setMethod('initialize', 'sleigh',
function(.Object, nodeList=c('localhost', 'localhost', 'localhost'), ...) {
  .Object@nodeList = nodeList
  .Object@workerCount = length(nodeList)

  # compute default value for scriptDir
  nwsDir = .path.package('nws', quiet=TRUE)
  if (is.null(nwsDir)) {
    scriptDir = getwd()
  }
  else {
    scriptDir = file.path(nwsDir, 'bin')
  }

  # compute default value for nwsHost
  nwsHost = Sys.getenv('RSleighNwsHost')
  if (is.null(nwsHost) || (nchar(nwsHost) < 1))
    nwsHost = Sys.info()['nodename']

  # compute default value for nwsPort
  nwsPort = as.integer(Sys.getenv('RSleighNwsPort'))
  if (is.na(nwsPort))
    nwsPort = 8765

  defaultSleighOptions <-
    list(
         master =  Sys.info()['nodename'],
         nwsWsName = 'sleigh_ride_%010d',
         nwsHost = nwsHost,
         outfile = '/dev/null',
         nwsPort = nwsPort,
         launch = sshcmd,
         scriptDir = scriptDir,
         user = Sys.info()['user'],
         )

  .Object@options = new.env()
  blendOptions(.Object@options, defaultSleighOptions)
  blendOptions(.Object@options, list(...))
  opts = .Object@options

   # set up the sleigh's netWorkSpace.
  .Object@nwss = new('nwsServer', serverHost=opts$nwsHost, port=opts$nwsPort)
  .Object@nwsName = nwsMktempWs(.Object@nwss, opts$nwsWsName)
  .Object@nws = nwsOpenWs(.Object@nwss, .Object@nwsName)

  # initialize for monitoring
  nwsDeclare(.Object@nws, 'nodeList', 'single')
  nwsDeclare(.Object@nws, 'totalTasks', 'single')
  nwsStore(.Object@nws, 'totalTasks', '0')

  if (is.function(opts$launch)) {
    myList = c()
    for (i in 1:.Object@workerCount) {
      node = paste(nodeList[i], i, sep='@')
      myList[i] = node
      nwsDeclare(.Object@nws, node, 'single')
      nwsStore(.Object@nws, node, '0')
      opts$outfile = sprintf('%s_%04d', .Object@nwsName, i)

                                        # since we are calling it in the constructor, maybe this cannot be
                                        # a method?
      addWorker(.Object@nodeList[[i]], .Object@nwsName, i, opts)
    }
    nwsStore(.Object@nws, 'nodeList', paste(myList, collapse=' '))
  }
  else if (opts$launch == 'web') {
                                        # joiners will build this list on the fly.
    nwsStore(.Object@nws, 'nodeList', '.')
    nwsStore(.Object@nws, 'rankCount', 0)
    nwsStore(.Object@nws, 'runMe',
             sprintf("Sys.putenv('RSleighNwsHost'='%s', 'RSleighNwsPort'='%d', 'RSleighNwsName'='%s', 'RSleighRank'='-1', 'RSleighName'=Sys.info()['nodename']); workerLoop();", opts$nwsHost, opts$nwsPort, .Object@nwsName))
    try(nwsFetch(.Object@nws, 'deleteMeWhenAllWorkersStarted'))
    .Object@workerCount = nwsFind(.Object@nws, 'rankCount')
    
  }
  else stop('unknown launch protocol.')

  .Object@state = new.env()
  .Object@state$bx = 1
  .Object@state$occupied = FALSE
  .Object@state$totalTasks = 0

  .Object
})

setMethod('show', 'sleigh', function(object) {
  cat('\n')
  cat('NWS Sleigh Object\n')
  show(object@nws)
  cat(object@workerCount, ' Worker Nodes:\t',
      paste(object@nodeList, collapse=', '), '\n', sep='')
  cat('\n')
})

if (! isGeneric('close'))
  setGeneric('close', function(con, ...) standardGeneric('close'))
setMethod('close', 'sleigh', function(con, ...) stopSleigh(con))

setGeneric('stopSleigh', function(.Object) standardGeneric('stopSleigh'))
setMethod('stopSleigh', 'sleigh', function(.Object) {
  nwsStore(.Object@nws, 'Sleigh ride over', 1)
  Sys.sleep(3)
  exitCount = 0
  while (nwsFetchTry(.Object@nws, 'bye', FALSE)) {
    exitCount = exitCount + 1
  }
  if (exitCount != .Object@workerCount) {
    cat(sprintf('Only %d of %d have exited.\n', exitCount, .Object@workerCount))
  }
  nwsDeleteWs(.Object@nwss, .Object@nwsName)
})

storeTask <- function(nws, fun, args,
                      tag = 'anon', barrier = FALSE, return = TRUE) {
  nwsStore(nws,
           'task',
           list(type = 'EXEC',
                barrier = barrier,
                data = list(fun = fun, args = args, return = return),
                tag = tag))
}

# run fun once on each worker of the sleigh. pass in a val from the
# range 1:#Workers
setGeneric('eachWorker',
           function(.Object, fun, ..., eo=NULL) standardGeneric('eachWorker'))
setMethod('eachWorker', 'sleigh',
function(.Object, fun, ..., eo=NULL) {
  if (.Object@state$occupied) {
    stop('wait your turn.')
  }

  fun <- fun # need to force the argument (NJC: why?)

  nws = .Object@nws
  wc = .Object@workerCount
  
  blocking = 1
  if (!is.null(eo)) {
    if (is.environment(eo) || is.list(eo)) {
      blocking = eo$blocking;
      if (is.null(blocking)) blocking = 1
    }
    else {
      stop('options arg must be a list or environment.')
    }
  }
  
  # use alternating barrier to sync eachWorker invocations with the workers.
  bx = .Object@state$bx
  bn = barrierNames[[bx]]
  .Object@state$bx = bx%%2 + 1

  nwsFetchTry(.Object@nws, bn)

  # update the total number of submitted tasks
  .Object@state$totalTasks <- .Object@state$totalTasks + wc
  nwsStore(.Object@nws, 'totalTasks', as.character(.Object@state$totalTasks))
  
  for (i in 1:wc) {
    storeTask(nws, fun, list(...), tag=i, barrier=TRUE)
  }

  if (!blocking) {
    .Object@state$occupied = TRUE
    return (new('sleighPending', nws, wc, bn, .Object@state))
  }

  val <- vector('list', length(wc))
  for (i in 1:wc) {
    r = nwsFetch(nws, 'result')
    if (is.null(r)) {
      stop('abnormal shutdown of sleigh workers???')
    }
    if (! is.null(r$value)) {
      val[[r$tag]] = r$value
    }
  }

  nwsStore(.Object@nws, bn, 1)
  val
})

getElement <- function(x, i, by=c("row","column","cell"))
  {
    by <- match.arg(by) # do partial matching to get one of the options

    if(is.matrix(x) || is.data.frame(x))
      switch(by,
             "row"=x[i,],
             "column"=x[,i],
             "cell"=x[i]
             )
    else
      x[[i]]
  }

countElement <- function(x, by=c("row","column","cell"))
  {
    by <- match.arg(by) # do partial matching to get one of the options

    if(is.matrix(x) || is.data.frame(x))
      switch(by,
             "row"=nrow(x),
             "column"=ncol(x),
             "cell"=length(x)
             )
    else
      length(x)
  }

# run fun once for each element of a vector.
setGeneric('eachElem',
           function(.Object, fun, elementArgs=list(), fixedArgs=list(), eo=NULL) standardGeneric('eachElem'))
setMethod('eachElem', 'sleigh',
function(.Object, fun, elementArgs=list(), fixedArgs=list(), eo=NULL) {
  if (.Object@state$occupied) {
    stop('wait your turn.')
  }

  fun <- fun # need to force the argument (NJC: why?)

  nws = .Object@nws
  wc = .Object@workerCount

  argPermute = NULL
  blocking = 1
  lf = 0
  by = "row"
  if (!is.null(eo)) {
    if (is.environment(eo) || is.list(eo)) {
      argPermute = eo$argPermute
      blocking = eo$blocking;
      if (is.null(blocking)) blocking = 1
      lf = eo$loadFactor;
      if (is.null(lf)) lf = 0
      by = eo$by
      if (is.null(by)) by = "row"
    }
    else {
      stop('options arg must be a list or environment.')
    }
  }
  numTasks = countElement(elementArgs[[1]], by=by)
  taskLimit = numTasks

  if (blocking && wc < numTasks && lf > 0) {
    taskLimit = eo$loadFactor * wc
    if (taskLimit < wc) {
      taskLimit = wc
    }
    else if (taskLimit > numTasks) {
      taskLimit = numTasks
    }
  }

  # check the extra args
  if (length(elementArgs)>1) { 
    for (ea in elementArgs[2:length(elementArgs)]) {
      if (countElement(ea, by=by) != numTasks) {
        stop('all vector arguments must be the same length.')
      }
    }
  }

  # update the total number of submitted tasks
  .Object@state$totalTasks <- .Object@state$totalTasks + numTasks
  nwsStore(.Object@nws, 'totalTasks', as.character(.Object@state$totalTasks))

  # fill the pool
  for (i in 1:taskLimit) {
    args = c(lapply(elementArgs, getElement, i=i, by=by), fixedArgs)
    if (!is.null(argPermute)) { args = args[argPermute] }
    storeTask(nws, fun, args, tag=i, barrier=FALSE)
  }

  if (!blocking) {
    .Object@state$occupied = TRUE
    return (new('sleighPending', nws, numTasks, '', .Object@state))
  }

  val <- vector('list', numTasks)

  # submit one new task for every task completed
  if (taskLimit < numTasks) {
    for (i in (taskLimit+1):numTasks) {
      r = nwsFetch(nws, 'result')
      if (is.null(r)) {
        stop('abnormal shutdown of sleigh workers???')
      }
      if (! is.null(r$value)) {
        val[[r$tag]] = r$value
      }
      args = c(lapply(elementArgs, getElement, i=i, by=by), fixedArgs)
      if (!is.null(argPermute)) { args = args[argPermute] }
      storeTask(nws, fun, args, tag=i, barrier=FALSE)
    }
  }

  # drain the pool
  for (i in (numTasks-taskLimit+1):numTasks) {
    r = nwsFetch(nws, 'result')
    if (is.null(r)) {
      stop('abnormal shutdown of sleigh workers???')
    }
    if (! is.null(r$value)) {
      val[[r$tag]] = r$value
    }
  }

  val
})
