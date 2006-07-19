#
# Copyright (c) 2005-2006, Scientific Computing Associates, Inc.
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

workerLoop <- function(nws, displayName, rank, workerCount, verbose) {
  bx <- 1

  # put these into global environment so both worker loop and worker
  # code have access
  SleighName <<- displayName
  SleighNws <<- nws
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
  nwsStore(SleighNws, 'worker info', list(sysInfo=Sys.info(), pid=Sys.getpid()))

  repeat {
    # update the number of tasks executed
    nwsStore(SleighNws, SleighName, as.character(tasks))

    t <- try(nwsFetch(SleighNws, 'task'))
    
    # use to generate an informal log    
    if (verbose) {
      cat("Task: \n")
      print(t)
    }
    if (!is.list(t)) { break }

    arg <- t$data$args
    value <- lapply(seq(arg), function(i) try(docall(t$data$fun, arg[[i]])))

    if (verbose) {
      cat("Value:\n")
      print(value)
    }

    nwsStore(SleighNws, 'result', list(type = 'VALUE', value = value, tag = t$tag))

    tasks <- tasks + length(arg)

    if (t$barrier) {
      nwsFind(SleighNws, barrierNames[[bx]])
      bx <- bx%%2 + 1
    }
  }
}

