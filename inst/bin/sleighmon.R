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

library(nws)

monitor <- function(ws, filename) {
  nodelist <- nwsFind(ws, 'nodeList')
  nodes <- unlist(strsplit(nodelist, " "))
  tasks <- unlist(lapply(nodes, function(n) as.integer(nwsFind(ws, n))))
  tasks <- c(sum(tasks), tasks)
  labels <- c('Total', sub('localhost@', '', nodes))
  totalTasks <- max(as.integer(nwsFind(ws, 'totalTasks')), tasks[1])
  xlim <- c(0, max(totalTasks, 1))
  main <- nwsFindTry(ws, 'MainTitle', 'Sleigh Monitor')
  sub <- paste('Completed', tasks[1], 'of', totalTasks, 'Tasks')
  sub <- nwsFindTry(ws, 'SubTitle', sub)

  if (length(tasks) > 20) {
    tasks <- tasks[1:20]
    labels <- labels[1:20]
  }
  ntasks <- length(tasks)
  col <- rainbow(ntasks)

  if (ntasks < 4) {
    h <- 7
    labels <- abbreviate(labels, 11)
  } else if (ntasks < 7) {
    h <- 8
    labels <- abbreviate(labels, 9)
  } else if (ntasks < 12) {
    h <- 9
    labels <- abbreviate(labels, 6)
  } else {
    h <- 11
    labels <- abbreviate(labels, 3)
  }
  res <- 72
  pixels <- res * h

  tryCatch(png(filename=filename, height=pixels, width=576, pointsize=12),
           error=function(e) bitmap(file=filename, height=h, width=8, res=res, pointsize=12))

  barplot(tasks, horiz=TRUE, names.arg=labels, main=main, sub=sub,
          xlab='Tasks Executed', ylab='Workers',
          xlim=xlim, legend.text=as.character(tasks), col=col)

  dev.off()
}

host <- 'localhost'
port <- 8765

gotargs <- FALSE
args <- commandArgs()

while (length(args) > 0) {
  a <- args[1]
  args <- args[-1]

  if (!gotargs) {
    if (a == '--args') gotargs <- TRUE
  } else {
    a <- match.arg(a, c('-host', '-port'))
    if (length(args) == 0) stop('option ', a, ' takes a required argument')
    v <- args[1]
    args <- args[-1]
    assign(substring(a, 2), switch(a, '-host' = v, '-port' = as.integer(v)))
  }
}

bws <- netWorkSpace('Sleigh Monitor', host, port)

reqnum <- 0
while (TRUE) {
  # cat('waiting for request\n')
  vwsname <- NA
  replyVarName <- 'reply'
  a <- nwsFetch(bws, 'request')
  # cat('number of arguments', a, '\n')
  numargs <- as.integer(a)
  if (!is.na(numargs) && numargs > 0) {
    for (i in 1:numargs) {
      # cat('waiting for argument', i, '\n')
      val <- nwsFetch(bws, 'request')
      # cat('argument:', val, '\n')
      v <- strsplit(val, "")
      x <- grep("=", v[[1]])
      if (length(x) > 0 && x[1] > 1) {
        name <- substring(val, 1, x[1] - 1)
        # cat('arg name:', name, '\n')
        value <- substring(val, x[1] + 1)
        # cat('arg value:', value, '\n')
        if (name == 'wsName') {
          vwsname <- value
        } else if (name == 'replyVarName') {
          replyVarName <- value
        }
      } else {
        cat('bad argument:', val)
        vwsname <- NA
        break
      }
    }
  }

  if (!is.na(vwsname)) {
    # cat('got request for workspace', vwsname, '\n')

    # open the workspace and create an image file from it
    vws <- nwsUseWs(bws@server, vwsname)  # XXX should use 'create=FALSE'
    # cat('opening sleigh workspace\n')
    reqnum <- reqnum + 1
    imagefile <- sprintf('mon_%d.png', reqnum)
    monitor(vws, imagefile)
    # cat('returned from monitor function\n')

    # read the image file into a variable
    size <- file.info(imagefile)$size
    # cat(imagefile, 'has', size, 'bytes\n')
    image <- rawToChar(readBin(imagefile, 'raw', size))

    # send the image data to the web interface and remove the image file
    # cat('sending reply', nchar(image), 'bytes long\n')
    nwsStore(bws, replyVarName, '3')
    nwsStore(bws, replyVarName, 'content-type=image/png')
    nwsStore(bws, replyVarName, 'cache-control=no-cache')
    nwsStore(bws, replyVarName, 'refresh=5')
    nwsStore(bws, replyVarName, image)
    # cat('removing image file\n')
    file.remove(imagefile)
  } else {
    cat('bad request\n')
    nwsStore(bws, replyVarName, '2')
    nwsStore(bws, replyVarName, 'content-type=text/plain')
    nwsStore(bws, replyVarName, 'cache-control=no-cache')
    nwsStore(bws, replyVarName, 'wsName not specified')
  }
}
