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

# internal functions for Sleigh class

# enquote and docall copied verbatim from snow
enquote <- function(x) as.call(list(as.name('quote'), x))

docall <- function(fun, args) {
  if ((is.character(fun) && length(fun) == 1) || is.name(fun))
    fun <- get(as.character(fun), env = .GlobalEnv, mode = 'function')
  do.call('fun', lapply(args, enquote))
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

# could this be a method --- it is invoked in the constructor?
addWorker <- function(machine, wsName, id, workerCount, options) {
  # basic idea is (or should be): if we can get the appropriate
  # worker script running on the remote node, we just need to
  # give it enough env info to take care of the rest

  envVars = list(
    paste('RSleighName=', machine, sep=''),
    paste('RSleighNwsName=', wsName, sep=''),
    paste('RSleighID=', id, sep=''),
    paste('RSleighWorkerCount=', workerCount, sep=''),
    paste('RSleighScriptDir=', options$scriptDir, sep=''),
    paste('RSleighNwsHost=', options$nwsHost, sep=''),
    paste('RSleighNwsPort=', options$nwsPort, sep=''),
    paste('RSleighWorkingDir=', options$workingDir, sep=''),
    paste('RProg=', options$rprog, sep='')
  )

  elen = length(envVars)

  if (!is.null(options$outfile)) {
    elen = elen + 1
    envVars[[elen]] = paste('RSleighWorkerOut=', options$outfile, sep='')
  }

  if (!is.null(options$logDir)) {
    elen = elen + 1
    envVars[[elen]] = paste('RSleighLogDir=', options$logDir, sep='')
  }

  if (is.character(options$launch) && options$launch == 'local') {
    background <- file.path(options$wrapperDir, 'BackgroundLaunch.py')
    if (!is.null(options$python)) {
      launchcmd = c(options$python, background)
    }
    else {
      launchcmd = c('python', background)
    }
    workerstart = options$scriptExec(machine, envVars, options)
  }
  else if (is.function(options$launch)) {
    launchcmd = options$launch(machine, options)
    # XXX do we need to add an extra level of quoting to workerstart?
    # XXX for ssh? or rsh?
    workerstart = options$scriptExec(machine, envVars, options)
  }

  argv = c(launchcmd, workerstart)
  cmd = argv2str(argv)
  if (options$verbose) cat("Executing command: ", cmd, "\n")
  system(cmd)
}

storeTask <- function(nws, fun, args,
                      tag = 'anon', barrier = FALSE, return = TRUE, job = -1) {
  nwsStore(nws, 'task',
           list(type='EXEC',
                barrier=barrier,
                data=list(fun=fun, args=args, return=return),
                tag=tag,
                job=job))
}

## This is necessary since x[i] gets the i'th *column* of a data
## frame object rather than the i'th cell
dfGetElement <- function(x, obs) {
  row <- (obs - 1) %% nrow(x) + 1
  col <- floor((obs - 1) / nrow(x)) + 1
  x[cbind(row, col)]  # pair (r,c) to select elements
}

getChunk <- function(x, iv, by) {
  if (is.matrix(x))
    switch(by, "row"=x[iv,,drop=FALSE], "column"=x[,iv,drop=FALSE], "cell"=x[iv])
  else if (is.data.frame(x))
    switch(by, "row"=x[iv,,drop=FALSE], "column"=x[,iv,drop=FALSE],
        "cell"=dfGetElement(x,iv))
  else
    # this case works for vectors and lists
    x[iv]
}

getElement <- function(x, i, by) {
  if (is.matrix(x))
    switch(by, "row"=x[i,,drop=TRUE], "column"=x[,i,drop=TRUE], "cell"=x[i])
  else if (is.data.frame(x))
    switch(by, "row"=x[i,,drop=TRUE], "column"=x[,i,drop=TRUE], "cell"=dfGetElement(x,i))
  else if (is.list(x))
    x[[i]]
  else
    # this case only works for vectors
    x[i]
}

countElement <- function(x, by) {
  if(is.matrix(x))
    switch(by, "row"=nrow(x), "column"=ncol(x), "cell"=length(x))
  else if (is.data.frame(x))
    switch(by, "row"=nrow(x), "column"=ncol(x), "cell"=nrow(x) * ncol(x))
  else
    length(x)
}

msc_quote <- function(arg) {
  q <- '"'
  nbs <- 0
  v <- strsplit(arg, split='')[[1]]
  for (c in v) {
    if (c == '\\') {
      q <- paste(q, c, sep='')
      nbs <- nbs + 1
    }
    else if (c == '"') {
      q <- paste(q, paste(rep('\\', nbs + 1), collapse=''), c, sep='')
      nbs <- 0
    }
    else {
      q <- paste(q, c, sep='')
      nbs <- 0
    }
  }

  paste(q, paste(rep('\\', nbs), collapse=''), '"', sep='')
}

unix_quote <- function(arg) {
  q <- "'"
  v <- strsplit(arg, split='')[[1]]
  for (c in v) {
    if (c == "'") {
      c <- "'\\''"
    }
    q <- paste(q, c, sep='')
  }

  paste(q, "'", sep='')
}

argv2str <- function(argv) {
  if (Sys.info()[['sysname']] == 'Windows')
    paste(lapply(argv, msc_quote), collapse=' ')
  else
    paste(lapply(argv, unix_quote), collapse=' ')
}
