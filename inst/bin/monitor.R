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

src = function(...) { source('nws.R') }
tryCatch(library(nws), error=src)

wsname = Sys.getenv('RSLEIGHWS')
if (wsname == '') {
    stop('error: the RSLEIGHWS environment variable must be set')
}

hostname = Sys.getenv('NWSHOSTNAME')
if (hostname == '') {
    hostname = 'localhost'
}

defport = 8765
tmpport = Sys.getenv('NWSPORT')
if (tmpport == '') {
    port = defport
} else {
    port = tryCatch(as.integer(tmpport), warning = function(x) defport)
}

ws = new('netWorkSpace', wsname, hostname, port)

x11()
# On the Mac use:
#quartz()

col = c('red', 'orange', 'yellow', 'green', 'blue', 'purple', 'violet')

repeat {
    nodelist = nwsFind(ws, 'nodeList')
    if (is.null(nodelist)) {
        stop('workspace has been destroyed')
    }
    nodes = unlist(strsplit(nodelist, " "))

    tasks = c(0)
    labels = c('Total')

    for (i in 1:length(nodes)) {
        x = nwsFind(ws, nodes[i])
        if (is.null(nodelist)) {
            stop('workspace has been destroyed')
        }
        tasks[i + 1] = as.integer(x)
        labels[i + 1] = nodes[i]
    }

    tasks[1] = sum(tasks)

    total = nwsFind(ws, 'totalTasks')
    if (is.null(nodelist)) {
        stop('workspace has been destroyed')
    }
    totalTasks = as.integer(total)
    ylim = c(0, max(totalTasks, 1))

    barplot(tasks, names.arg=labels, main='R Sleigh Monitor',
            ylab='Tasks Executed', xlab='Hosts', ylim=ylim,
            legend.text=as.character(tasks), col=col)

    Sys.sleep(3)
}
