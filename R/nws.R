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

# this is a manifest constant, how to approriately handle?
nwsRFP = 3*2^24

# a utility function to read exactly n bytes from the socket connection.
nwsRecvN <- function(s, n) {
  if (n > 0) {
    b = readBin(s, what='raw', n=n)
    m = length(b)
    n = n - m
    if (n <= 0) return(rawToChar(b))
    if (m == 0) stop("failed to read from nws socket")

    # we didn't get all of the data, so save the raw vector in a list
    # that we'll concatenate when we do have it all
    r = vector('list', 100)  # preallocate 100, but it will extend as needed
    i = 1
    r[i] = list(b)

    repeat {
      b = readBin(s, what='raw', n=n)
      i = i + 1
      r[i] = list(b)
      m = length(b)
      n = n - m
      if (n <= 0) break
      if (m == 0) stop("failed to read from nws socket")
    }

    # truncate the list, concatenate the raw vectors,
    # and convert to a single character string
    length(r) = i
    return(rawToChar(do.call(c, r)))
  }
  else {
    return('')
  }
}

nwsServer <- function(...) {
  new("nwsServer", ...)
}

# class respresenting connection to a netWorkSpace server.
setClass('nwsServer', representation(nwsSocket='ANY', port='numeric', serverHost='character'))

setMethod('initialize', 'nwsServer',
          function(.Object, serverHost='localhost', port=8765) {
            .Object@serverHost = serverHost
            .Object@port = port

            if (Sys.info()[['sysname']] == 'Windows') {
              # on windows, socketConnection will wait for the full timeout,
              # even if no one is listening on the specified server port.
              # make.socket doesn't, so we'll use it to throw an exception
              # if no one is listening.
              tmpsock = make.socket(serverHost, port)
              close.socket(tmpsock)
            }

            # temporarily change the timeout while creating the socketConnection.
            # we will block for up to a year for data on this socket.
            old.timeout = options(timeout=32140800)[[1]]
            .Object@nwsSocket = tryCatch(socketConnection(serverHost, port=port, open='a+b', blocking=TRUE),
                                         finally=options(timeout=old.timeout))

            # handshaking that does nothing at the moment.
            writeBin(charToRaw('0000'), .Object@nwsSocket)
            nwsRecvN(.Object@nwsSocket, 4)
            .Object
          })

setGeneric('nwsDeleteWs', function(.Object, wsName) standardGeneric('nwsDeleteWs'))
setGeneric('nwsListWss', function(.Object, showDataFrame=FALSE) standardGeneric('nwsListWss'))
setGeneric('nwsMktempWs', function(.Object, wsNameTemplate='__Rws__%010d') standardGeneric('nwsMktempWs'))
setGeneric('nwsOpenWs', function(.Object, wsName, space=NULL, ...) standardGeneric('nwsOpenWs'))
setGeneric('nwsUseWs', function(.Object, wsName, space=NULL, ...) standardGeneric('nwsUseWs'))

setMethod('nwsDeleteWs', 'nwsServer',
          function(.Object, wsName) {
            op = 'delete ws'
            s = .Object@nwsSocket

            writeBin(charToRaw(sprintf('0002%020d%s%020d%s', nchar(op), op, nchar(wsName), wsName)), s)

            # status, unused at the moment.
            bb = nwsRecvN(s, 4)
          })

setMethod('nwsListWss', 'nwsServer',
          function(.Object, showDataFrame=FALSE) {
            op = 'list wss'
            s = .Object@nwsSocket
            writeBin(charToRaw(sprintf('0001%020d%s', nchar(op), op)), s)

            status = nwsRecvN(s, 4)
            desc = nwsRecvN(s, 20)

            ret <- nwsRecvN(s, as.integer(nwsRecvN(s, 20)))
            if (showDataFrame==FALSE)
              ret
            else {
              ## convert response into an R data frame
              ret <- unlist(strsplit(ret, "\n"))
              retval <- list()
              fields <- list()
              i = 1
              while (i <= length(ret)) {
                line <- unlist(strsplit(ret[i], "\t"))

                # convert each field to correct type
                fields[1] = FALSE
                if (substr(line[1], 1, 1)=='>')
                  fields[1] = TRUE
                fields[2] = substr(line[1], 2, nchar(line[1]))  # workspace name
                fields[3] = line[2]
                fields[4] = as.logical(line[3])
                fields[5] = as.integer(line[4])
                if (is.na(line[5]))
                  fields[6] = ""
                else
                  fields[6] = line[5]

                retval = c(retval, list(fields))
                i = i+1
              }

              if (length(retval) > 0) {
                names(retval) <- seq(along=retval)
                retval <- do.call(rbind, retval)
                colnames(retval) <-
                  c("Owned", "Name", "Owner", "Persistent", "NumVariables", "Variables")
              }
              retval <- data.frame(retval)
              retval
            }

          })

setMethod('nwsMktempWs', 'nwsServer',
          function(.Object, wsNameTemplate) {
            op = 'mktemp ws'
            s = .Object@nwsSocket
            writeBin(charToRaw(sprintf('0002%020d%s%020d%s', nchar(op), op, nchar(wsNameTemplate), wsNameTemplate)), s)

            # status, unused at the moment
            status = nwsRecvN(s, 4)
            desc = nwsRecvN(s, 20)

            nwsRecvN(s, as.integer(nwsRecvN(s, 20)))
          })

setMethod('nwsOpenWs', 'nwsServer',
          function(.Object, wsName, space=NULL, ...) {
            # if invoked diectly by user, we need to create a space
            # instance. if invoked via networkspace constructor, use the
            # space passed in.

            # ... because there may be option args related to persistence.
            if (is.null(space)) {
              serverWrap = new.env()
              serverWrap$server = .Object
              space = new('netWorkSpace', wsName=wsName, serverWrap=serverWrap)
            }

            op = 'open ws'
            owner = sprintf('%d', Sys.getpid())
            opts = list(...)
            p = 'no'
            if (!is.null(opts$persistent) && opts$persistent) p = 'yes'

            s = .Object@nwsSocket

            if (is.null(opts$create) || opts$create) {
              writeBin(charToRaw(sprintf('0004%020d%s%020d%s%020d%s%020d%s',
                                      nchar(op), op,
                                      nchar(wsName), wsName,
                                      nchar(owner), owner,
                                      nchar(p), p)), s)
            }
            else {
              create = 'no'
              writeBin(charToRaw(sprintf('0005%020d%s%020d%s%020d%s%020d%s%020d%s',
                                      nchar(op), op,
                                      nchar(wsName), wsName,
                                      nchar(owner), owner,
                                      nchar(p), p,
                                      nchar(create), create)), s)
            }

            status = as.integer(nwsRecvN(s, 4))
            if (status != 0) stop(paste("workspace", wsName, "doesn't exist"))
            space
          })

setMethod('nwsUseWs', 'nwsServer',
          function(.Object, wsName, space=NULL, ...) {
            # see nwsOpenWs
            if (is.null(space)) {
              serverWrap = new.env()
              serverWrap$server = .Object
              space = new('netWorkSpace', wsName=wsName, serverWrap=serverWrap)
            }

            op = 'use ws'
            owner = ''
            opts = list(...)
            p = 'no'

            s = .Object@nwsSocket

            if (is.null(opts$create) || opts$create) {
              writeBin(charToRaw(sprintf('0004%020d%s%020d%s%020d%s%020d%s',
                                      nchar(op), op,
                                      nchar(wsName), wsName,
                                      nchar(owner), owner,
                                      nchar(p), p)), s)
            }
            else {
              create = 'no'
              writeBin(charToRaw(sprintf('0005%020d%s%020d%s%020d%s%020d%s%020d%s',
                                      nchar(op), op,
                                      nchar(wsName), wsName,
                                      nchar(owner), owner,
                                      nchar(p), p,
                                      nchar(create), create)), s)
            }

            status = as.integer(nwsRecvN(s, 4))
            if (status != 0) stop(paste("workspace", wsName, "doesn't exist"))
            space
          })

if (!isGeneric('close'))
  setGeneric('close', function(con, ...) standardGeneric('close'))
setMethod('close', 'nwsServer', function(con, ...) close(con@nwsSocket))

netWorkSpace <- function(...) {
  new("netWorkSpace", ...)
}

# class representing a netWorkSpace.
setClass('netWorkSpace', representation(server='nwsServer', wsName='character'))

setMethod('initialize', 'netWorkSpace',
          function(.Object, wsName='__default', serverHost='localhost', port=8765, useUse=FALSE, serverWrap=NULL, ...) {
            # ask gw what is the right way to overload init/constructor func. and call by ref too.
            # ... because there may be option args related to persistence.
            .Object@wsName = wsName

            # if invoked (indirectly) via a server openWs or useWs
            # method, the server will be passed in and used. if
            # invoked directly, need to create a new server instance.
            if (!is.null(serverWrap)) {
              # recycle existing server instance.
              .Object@server = serverWrap$server
            }
            else {
              # create new server instance.
              .Object@server = new('nwsServer', serverHost=serverHost, port=port)
              # now give the server a chance to do its thing.
              spaceWrap = new.env()
              spaceWrap$space = .Object
              handler = function(e) { close(.Object@server@nwsSocket); stop(e) }
              if (useUse) {
                # don't claim this space.
                tryCatch(nwsUseWs(.Object@server, wsName, spaceWrap, ...), error=handler)
              }
              else {
                # attempt to claim ownership
                tryCatch(nwsOpenWs(.Object@server, wsName, spaceWrap, ...), error=handler)
              }
            }

            .Object
          })

showNetWorkSpace <- function(object) {
    nws <- object
    server <- nws@server

    cat('\n')
    cat('NWS Host:\t', server@serverHost, ':', server@port, '\n', sep='')
    cat('Workspace Name:\t', nws@wsName, '\n', sep='')
    cat('\n')
}

setMethod('show', 'netWorkSpace', showNetWorkSpace)

setGeneric('nwsClose', function(.Object) standardGeneric('nwsClose'))
setGeneric('nwsDeclare', function(.Object, xName, mode) standardGeneric('nwsDeclare'))
setGeneric('nwsDeleteVar', function(.Object, xName) standardGeneric('nwsDeleteVar'))
setGeneric('nwsFetch', function(.Object, xName) standardGeneric('nwsFetch'))
setGeneric('nwsFetchTry', function(.Object, xName, defaultVal=NULL) standardGeneric('nwsFetchTry'))
setGeneric('nwsFind', function(.Object, xName) standardGeneric('nwsFind'))
setGeneric('nwsFindTry', function(.Object, xName, defaultVal=NULL) standardGeneric('nwsFindTry'))
setGeneric('nwsListVars', function(.Object, wsName='', showDataFrame=FALSE) standardGeneric('nwsListVars'))
setGeneric('nwsStore', function(.Object, xName, xVal) standardGeneric('nwsStore'))
setGeneric('nwsWsName', function(.Object) standardGeneric('nwsWsName'))
setGeneric('nwsVariable', function(.Object, xName, mode=c('fifo','lifo','multi','single'),
           env=parent.frame(), force=FALSE, quietly=FALSE) standardGeneric('nwsVariable'))

setMethod('nwsClose', 'netWorkSpace',
          function(.Object) {
            # XXX this seems wrong
            close(.Object@server)
          })

# setGeneric('close', function(con, ...) standardGeneric('close'))
# setMethod('close','netWorkSpace', function(con) nwsClose(con))

# helper function for nwsDeclare method.
nwsDeclareInternal <- function(s, ws, xName, mode) {
  op = 'declare var'

  writeBin(charToRaw(sprintf('0004%020d%s%020d%s%020d%s%020d%s',
                             nchar(op), op,
                             nchar(ws), ws,
                             nchar(xName), xName,
                             nchar(mode), mode)), s)

  as.integer(nwsRecvN(s, 4))
}

setMethod('nwsDeclare', 'netWorkSpace',
          function(.Object, xName, mode) {
            status = nwsDeclareInternal(.Object@server@nwsSocket, .Object@wsName, xName, mode)
            if (status != 0) {
              stop('variable declaration failed')
            }
          })

setMethod('nwsDeleteVar', 'netWorkSpace',
          function(.Object, xName) {
            op = 'delete var'
            s = .Object@server@nwsSocket
            ws = .Object@wsName

            writeBin(charToRaw(sprintf('0003%020d%s%020d%s%020d%s',
                                    nchar(op), op,
                                    nchar(ws), ws,
                                    nchar(xName), xName)), s)

            # status, unused at the moment.
            bb = nwsRecvN(s, 4)
          })

# helper function for fetch/find methods.
nwsRetrieve <- function(s, ws, xName, op, defaultVal=NULL) {
  sn = nchar(c(op, ws, xName))
  writeBin(charToRaw(sprintf('0003%020d%s%020d%s%020d%s',
                          sn[1], op,
                          sn[2], ws,
                          sn[3], xName)), s)

  status = as.integer(nwsRecvN(s, 4))

  desc = as.integer(nwsRecvN(s, 20))
  envId = desc %/% 16777216 #(2^24)
  isString = desc %% 2

  sVal = nwsRecvN(s, as.integer(nwsRecvN(s, 20)))

  if (status != 0) {
    stop('retrieval failed')
  }

  if (isString) {
    sVal
  }
  else if (nchar(sVal) > 0) {
    unserialize(sVal)
  }
  else {
    defaultVal
  }
}

setMethod('nwsFetch', 'netWorkSpace',
          function(.Object, xName) {
            nwsRetrieve(.Object@server@nwsSocket, .Object@wsName, xName, 'fetch')
          })

setMethod('nwsFetchTry', 'netWorkSpace',
          function(.Object, xName, defaultVal=NULL) {
            tryCatch(nwsRetrieve(.Object@server@nwsSocket, .Object@wsName, xName, 'fetchTry', defaultVal),
                    error=function(e) defaultVal)
          })

setMethod('nwsFind', 'netWorkSpace',
          function(.Object, xName) {
            nwsRetrieve(.Object@server@nwsSocket, .Object@wsName, xName, 'find')
          })

setMethod('nwsFindTry', 'netWorkSpace',
          function(.Object, xName, defaultVal=NULL) {
            tryCatch(nwsRetrieve(.Object@server@nwsSocket, .Object@wsName, xName, 'findTry', defaultVal),
                    error=function(e) defaultVal)
          })

# to see list output clearly use: write(nwsList...(), stdout())
setMethod('nwsListVars', 'netWorkSpace',
          function(.Object, wsName='', showDataFrame=FALSE) {
            op = 'list vars'
            s = .Object@server@nwsSocket
            if (wsName == '') wsName = .Object@wsName

            writeBin(charToRaw(sprintf('0002%020d%s%020d%s',
                                    nchar(op), op,
                                    nchar(wsName), wsName)), s)

            # status, unused at the moment
            status = nwsRecvN(s, 4)
            desc = nwsRecvN(s, 20)

            ret <- nwsRecvN(s, as.integer(nwsRecvN(s, 20)))
            if (showDataFrame==FALSE)
              ret
            else {
              ## convert response into an R data frame
              ret <- unlist(strsplit(ret, "\n"))
              retval <- list()
              fields <- list()

              i = 1
              while (i<=length(ret)) {
                line <- unlist(strsplit(ret[i], "\t"))

                # convert each field to correct type
                fields[1] = line[1]
                fields[2] = as.integer(line[2])
                fields[3] = as.integer(line[3])
                fields[4] = as.integer(line[4])
                fields[5] = line[5]
                retval = c(retval, list(fields))
                i = i+1
              }

              if (length(retval)>0) {
                names(retval) <- seq(along=retval)
                retval <- do.call(rbind, retval)
                colnames(retval) <-
                c("Variable", "NumValues", "NumFetchers", "NumFinders", "Mode")
              }

              retval <- data.frame(retval)
              retval
            }

          })

# helper function for store method
nwsStoreInternal <- function(s, ws, xName, xVal) {
  op = 'store'

  desc = nwsRFP # R Fingerprint

  if (missing(xVal)) {
    stop('no value specified for xVal argument')
  }

  if (!is.character(xVal) || (length(xVal) != 1)) {
    xVal = serialize(xVal, ascii=FALSE, connection=NULL)
  }
  else {
    desc = desc + 1 # in other systems, we use a manifest constant and a bit or here... .
  }
  descTxt = sprintf('%020i', desc) # would prefer to use unsigned here.

  sn = nchar(c(op, ws, xName, descTxt, xVal))
  writeBin(c(charToRaw(sprintf('0005%020d%s%020d%s%020d%s%020d%s%020d',
                             sn[1], op,
                             sn[2], ws,
                             sn[3], xName,
                             sn[4], descTxt,
                             sn[5])), charToRaw(xVal)), s)

  # status, barely used at the moment.
  status = as.integer(nwsRecvN(s, 4))

  if (status != 0) {
    stop('store failed')
  }
}

setMethod('nwsStore', 'netWorkSpace',
          function(.Object, xName, xVal) {
            nwsStoreInternal(.Object@server@nwsSocket, .Object@wsName, xName, xVal)
          })

setMethod('nwsWsName', 'netWorkSpace', function(.Object) {.Object@wsName})

setMethod('nwsVariable', 'netWorkSpace',
          function(.Object, xName, mode=c('fifo','lifo','multi','single'),
                   env=parent.frame(), force=FALSE, quietly=FALSE) {
            missingMode = missing(mode)
            mode <- match.arg(mode)

            # be careful, because 'exists' will cause an active binding function
            # to be called, which is a side effect that we don't want
            if (force ||
                (!tryCatch(bindingIsActive(xName, env), error=function(...) FALSE) &&
                 !exists(xName, envir=env, inherits=FALSE))) {
              s = .Object@server@nwsSocket
              ws = .Object@wsName
              if (missingMode) {
                mlist = c(mode, 'fifo', 'lifo', 'multi', 'single')
                mode = NA
                for (m in mlist) {
                  if (nwsDeclareInternal(s, ws, xName, m) == 0) {
                    mode = m
                    break
                  }
                }
                if (is.na(mode))
                  stop('unable to declare variable')
              } else {
                if (nwsDeclareInternal(s, ws, xName, mode) != 0)
                  stop('variable declaration failed')
              }

              if (identical(mode, 'single')) {
                mf <- function(val)
                  if (missing(val))
                    nwsRetrieve(s, ws, xName, 'find')
                  else
                    nwsStoreInternal(s, ws, xName, val)
              } else {
                mf <- function(val)
                  if (missing(val))
                    nwsRetrieve(s, ws, xName, 'fetch')
                  else
                    nwsStoreInternal(s, ws, xName, val)
              }

              t <- makeActiveBinding(xName, mf, env)
            } else {
              if (! quietly)
                warning('not overwriting previous binding for ', xName)
            }
          })
