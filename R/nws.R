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

# this is a manifest constant, how to approriately handle?
nwsRFP = 3*2^24

# a utility function to read exactly n bytes from the connection.
nwsRecvN <- function(s, n) {
  r = ''
  while (nchar(r) < n) {
    b = read.socket(s, n - nchar(r))
    if (b == '') stop("failed to read from nws socket")
    r = paste(r, b, sep='') 
  }
  r
}

# class respresenting connection to a netWorkSpace server.
setClass('nwsServer', representation(nwsSocket='ANY', port='numeric', serverHost='character'))

setMethod('initialize', 'nwsServer',
          function(.Object, serverHost='localhost', port=8765) {
            .Object@serverHost = serverHost
            .Object@port = port			

            .Object@nwsSocket = make.socket(serverHost, port)
            # try to de-nagle this socket
            # XXX unable to really test this yet, since the gtools package
            # XXX didn't contain the .so file
            try({library(gtools); setTCPNoDelay(.Object@nwsSocket, value=FALSE)},
                silent=TRUE)

            # handshaking that does nothing at the moment.
            write.socket(.Object@nwsSocket, '0000')
            nwsRecvN(.Object@nwsSocket, 4)
            .Object
	  })

setGeneric('nwsDeleteWs', function(.Object, wsName) standardGeneric('nwsDeleteWs'))
setGeneric('nwsListWss', function(.Object) standardGeneric('nwsListWss'))
setGeneric('nwsMktempWs', function(.Object, wsNameTemplate='__Rws__%010d') standardGeneric('nwsMktempWs'))
setGeneric('nwsOpenWs', function(.Object, wsName, space=NULL, ...) standardGeneric('nwsOpenWs'))
setGeneric('nwsUseWs', function(.Object, wsName, space=NULL) standardGeneric('nwsUseWs'))

setMethod('nwsDeleteWs', 'nwsServer',
          function(.Object, wsName) {
            op = 'delete ws'
            s = .Object@nwsSocket

            write.socket(s, sprintf('0002%020d%s%020d%s', nchar(op), op, nchar(wsName), wsName))

            # status, unused at the moment.
            bb = nwsRecvN(s, 4)
	  })

setMethod('nwsListWss', 'nwsServer',
          function(.Object) {
            op = 'list wss'
            s = .Object@nwsSocket
            write.socket(s, sprintf('0001%020d%s', nchar(op), op))

            status = nwsRecvN(s, 4)
            desc = nwsRecvN(s, 20)

            nwsRecvN(s, as.integer(nwsRecvN(s, 20)))
	  })

setMethod('nwsMktempWs', 'nwsServer',
          function(.Object, wsNameTemplate) {
            op = 'mktemp ws'
            s = .Object@nwsSocket
            write.socket(s, sprintf('0002%020d%s%020d%s', nchar(op), op, nchar(wsNameTemplate), wsNameTemplate))

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
            if (is.null(opts$persistent))
              p = 'no'
            else {
	      if (opts$persistent)
	        p = 'yes'
              else
                p = 'no'
            }
            s = .Object@nwsSocket
            write.socket(s, sprintf('0004%020d%s%020d%s%020d%s%020d%s', nchar(op), op, nchar(wsName), wsName, nchar(owner), owner, nchar(p), p))

            # status, unused at the moment
            status = nwsRecvN(s, 4)

            space
	  })

setMethod('nwsUseWs', 'nwsServer',
          function(.Object, wsName, space=NULL) {
            # see nwsOpenWs
            if (is.null(space)) {
              serverWrap = new.env()
              serverWrap$server = .Object
              space = new('netWorkSpace', wsName=wsName, serverWrap=serverWrap)
            }

	    op = 'use ws'
	    owner = ''
	    p = 'no'

            s = .Object@nwsSocket
            write.socket(s, sprintf('0004%020d%s%020d%s%020d%s%020d%s', nchar(op), op, nchar(wsName), wsName, nchar(owner), owner, nchar(p), p))

            # status, unused at the moment
            status = nwsRecvN(s, 4)

            space
	  })

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
              if (useUse) {
                # don't claim this space.
                nwsUseWs(.Object@server, wsName, spaceWrap)
              }
              else {
                # attempt to claim ownership
                nwsOpenWs(.Object@server, wsName, spaceWrap)
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
setGeneric('nwsListVars', function(.Object, wsName='') standardGeneric('nwsListVars'))
setGeneric('nwsStore', function(.Object, xName, xVal) standardGeneric('nwsStore'))
setGeneric('nwsWsName', function(.Object) standardGeneric('nwsWsName'))

setMethod('nwsClose', 'netWorkSpace',
          function(.Object) {
            close.socket(.Object@server@nwsSocket)
	  })

setMethod('nwsDeclare', 'netWorkSpace',
          function(.Object, xName, mode) {
            op = 'declare var'
            s = .Object@server@nwsSocket
            ws = .Object@wsName

            write.socket(s, sprintf('0004%020d%s%020d%s%020d%s%020d%s',
                                    nchar(op), op,
                                    nchar(ws), ws,
                                    nchar(xName), xName,
                                    nchar(mode), mode))

            # status, unused at the moment.
            bb = nwsRecvN(s, 4)
          })

setMethod('nwsDeleteVar', 'netWorkSpace',
          function(.Object, xName) {
            op = 'delete var'
            s = .Object@server@nwsSocket
            ws = .Object@wsName

            write.socket(s, sprintf('0003%020d%s%020d%s%020d%s',
                                    nchar(op), op,
                                    nchar(ws), ws,
                                    nchar(xName), xName))

            # status, unused at the moment.
            bb = nwsRecvN(s, 4)
          })

# helper function for fetch/find methods.
nwsRetrieve <- function(s, ws, xName, op, defaultVal=NULL) {
  write.socket(s, sprintf('0003%020d%s%020d%s%020d%s',
                          nchar(op), op,
                          nchar(ws), ws,
                          nchar(xName), xName))

  status = nwsRecvN(s, 4)
  desc = as.integer(nwsRecvN(s, 20))
  envId = desc %/% 16777216 #(2^24)
  isString = desc %% 2	

  sVal = nwsRecvN(s, as.integer(nwsRecvN(s, 20)))
  if (sVal == '') {
    defaultVal
  }
  else {
    if (isString) {
      sVal
    }
    else {
      unserialize(sVal)
    }
  }
}

setMethod('nwsFetch', 'netWorkSpace',
          function(.Object, xName) {
            nwsRetrieve(.Object@server@nwsSocket, .Object@wsName, xName, 'fetch')
          })

setMethod('nwsFetchTry', 'netWorkSpace',
          function(.Object, xName, defaultVal=NULL) {
            nwsRetrieve(.Object@server@nwsSocket, .Object@wsName, xName, 'fetchTry', defaultVal)
          })

setMethod('nwsFind', 'netWorkSpace',
          function(.Object, xName) {
            nwsRetrieve(.Object@server@nwsSocket, .Object@wsName, xName, 'find')
          })

setMethod('nwsFindTry', 'netWorkSpace',
          function(.Object, xName, defaultVal=NULL) {
            nwsRetrieve(.Object@server@nwsSocket, .Object@wsName, xName, 'findTry', defaultVal)
          })

# to see list output clearly use: write(nwsList...(), stdout())
setMethod('nwsListVars', 'netWorkSpace',
          function(.Object, wsName='') {
            op = 'list vars'
            s = .Object@server@nwsSocket
            if (wsName == '') wsName = .Object@wsName

            write.socket(s, sprintf('0002%020d%s%020d%s',
                                    nchar(op), op,
                                    nchar(wsName), wsName))

            # status, unused at the moment
            status = nwsRecvN(s, 4)
            desc = nwsRecvN(s, 20)

            nwsRecvN(s, as.integer(nwsRecvN(s, 20)))
          })

setMethod('nwsStore', 'netWorkSpace',
          function(.Object, xName, xVal) {
            op = 'store'
            s = .Object@server@nwsSocket
            ws = .Object@wsName

	    desc = nwsRFP # R Fingerprint
            if (!is(xVal, 'character') || (length(xVal) != 1)) {
	      xVal = serialize(xVal, ascii=TRUE, connection=NULL)
	    }
            else {
	      desc = desc + 1 # in other systems, we use a manifest constant and a bit or here... .
            }
	    descTxt = sprintf('%020i', desc) # would prefer to use unsigned here.

            write.socket(s, sprintf('0005%020d%s%020d%s%020d%s%020d%s%020d',
                                    nchar(op), op,
                                    nchar(ws), ws,
                                    nchar(xName), xName,
                                    nchar(descTxt), descTxt,
                                    nchar(xVal, type='bytes')))

            # unfortunately, it looks like socket is assuming ascii null-terminated string behavior... 
            write.socket(s, xVal)

            # status, unused at the moment.
            bb = nwsRecvN(s, 4)
          })

setMethod('nwsWsName', 'netWorkSpace', function(.Object) {.Object@wsName})
