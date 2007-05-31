#
# Copyright (c) 2005-2007, Scientific Computing Associates, Inc.
#
# NetWorkSpaces is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as published
# by the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
# USA
#

tostr <- function(obj) {
  tc <- textConnection('rval', open='w')
  sink(tc)
  on.exit({sink(); close(tc)})
  print(obj)
  paste(rval, collapse='\n')
}

logMsg <- function(..., var) {
  msg <- sub('[[:space:]]+$', '', paste(..., sep='\n'))
  logMsg <- sprintf('[%s] %s -- %s', date(), SleighName, msg)
  cat(msg, '\n')
  flush.console()
  nwsStore(SleighNws, var, logMsg)
  invisible()
}

logError <- function(...) {
  logMsg(..., var='logError')
}

logDebug <- function(...) {
  logMsg(..., var='logDebug')
}

workerLoop <- function(nws, displayName, rank, workerCount, verbose, userNws) {
  bx <- 1

  # put these into global environment so both worker loop and worker
  # code have access
  SleighName <<- displayName
  SleighNws <<- nws
  SleighUserNws <<- userNws
  SleighRank <<- rank
  SleighWorkerCount <<- workerCount

  ## set RNG seed to a pseudo-unique value
  ## FIXME: Use sprng instead!
  setRNGSeed <- function() {
    now <- as.numeric(Sys.time())
    seedval <- as.integer(SleighRank)
    set.seed(seedval)
    seedval
  }

  setRNGSeed()

  # monitoring stuffs
  tasks <- 0

  # post some info about this worker.
  logfile <- Sys.getenv('RSleighLogFile')
  names(logfile) <- NULL
  nwsStore(SleighNws, 'worker info',
      list(sysInfo=paste(Sys.info()[-3], collapse=" "),
           version=R.version.string,
           logfile=logfile,
           pid=Sys.getpid()))

  repeat {
    # update the number of tasks executed
    nwsStore(SleighNws, SleighName, as.character(tasks))

    # wait for a task to execute
    t <- tryCatch(nwsFetch(SleighNws, 'task'), error=function(e) NULL)
    if (is.null(t)) {
      logDebug("Shutting down")
      break
    }

    # sanity check
    if (!is.list(t) || t$type != 'EXEC') {
      logError("Bad task: ignoring", tostr(t))
      next
    }

    if (verbose) {
      logDebug("Task:", tostr(t))
    }

    # for testing purposes
    if (identical(t$testing, "preallocation")) {
      cat("testing mode: quiting before sending allocation message\n")
      quit()
    }

    # send allocation message to the master
    nwsStore(SleighNws, 'result', list(type='ALLOCATION', tag=t$tag,
             job=t$job, resubmitted=t$resubmitted, rank=SleighRank))

    # for testing purposes
    if (identical(t$testing, "postallocation")) {
      cat("testing mode: quiting just after sending allocation message\n")
      quit()
    }

    # execute the task
    arg <- t$data$args
    dotask <- function(i) {
      tryCatch(docall(t$data$fun, arg[[i]]), error = function(e) {
        logError(as.character(e))
        e
      })
    }

    tm <- system.time(value <- lapply(seq(arg), dotask))

    if (verbose) {
      logDebug("Value:", value)
    }

    # send back the task results
    nwsStore(SleighNws, 'result', list(type='VALUE', value=value, tag=t$tag,
             job=t$job, resubmitted=t$resubmitted, time=tm, rank=SleighRank))

    tasks <- tasks + length(arg)

    if (t$barrier) {
      nwsFind(SleighNws, barrierNames[[bx]])
      bx <- bx%%2 + 1
    }

    # for testing purposes
    if (identical(t$testing, "postresults")) {
      cat("testing mode: quiting just after sending results\n")
      quit()
    }
  }
}

