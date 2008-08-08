#! /bin/sh
#
# Copyright (c) 2005-2008, REvolution Computing, Inc.
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

exec > /dev/null 2>&1

RProg=${RProg:-'R'}
cd ${RSleighWorkingDir:-'/tmp'}
LogDir=${RSleighLogDir:-'/tmp'}
if [ -n "${RSleighWorkerOut}" ]; then
    RSleighVerbose='TRUE'
    RSleighLogFile=${LogDir}/`basename "${RSleighWorkerOut}"`
else
    RSleighVerbose='FALSE'
    RSleighLogFile='/dev/null'
fi
export RSleighVerbose RSleighLogFile

# compute engine
$RProg --vanilla --slave <<'EOF' > ${RSleighLogFile} 2>&1 &

# use RSleighScriptDir to add the directory that contains
# the nws package to the library path if possible
# (and without modifying the worker's global environment)
nwsPkg <- ''
local({
    scriptDir <- Sys.getenv('RSleighScriptDir')
    if (basename(scriptDir) == 'bin') {
        nwsDir <- dirname(scriptDir)
        nwsPkg <<- basename(nwsDir)
        if (nwsPkg %in% c('nws', 'nwsPro')) {
            libDir <- dirname(nwsDir)
            oldPaths <- .libPaths()
            newPaths <- c(libDir, oldPaths)
            cat("setting library paths to", newPaths, "\n")
            .libPaths(newPaths)
        }
    }
})

if (nwsPkg == 'nws' || ! suppressWarnings(require(nwsPro, quietly=TRUE)))
    library(nws)
rm(nwsPkg)
cmdLaunch(as.logical(Sys.getenv('RSleighVerbose')))
EOF

RCEPid=$!
RCEHost=`hostname`
export RCEPid RCEHost

# sentinel
USERID=`id -u`
$RProg --vanilla --slave <<'EOF' > ${LogDir}/RSleighSentinelLog_${USERID}_${RSleighID} 2>&1

nwsPkg <- ''
local({
    scriptDir <- Sys.getenv('RSleighScriptDir')
    if (basename(scriptDir) == 'bin') {
        nwsDir <- dirname(scriptDir)
        nwsPkg <<- basename(nwsDir)
        if (nwsPkg %in% c('nws', 'nwsPro')) {
            libDir <- dirname(nwsDir)
            oldPaths <- .libPaths()
            newPaths <- c(libDir, oldPaths)
            cat("setting library paths to", newPaths, "\n")
            .libPaths(newPaths)
        }
    }
})

if (nwsPkg == 'nws' || ! suppressWarnings(require(nwsPro, quietly=TRUE)))
    library(nws)

cePid <- Sys.getenv('RCEPid')
ceHost <- Sys.getenv('RCEHost')
nws <- netWorkSpace(wsName=Sys.getenv('RSleighNwsName'),
    serverHost=Sys.getenv('RSleighNwsHost'),
    port=as.integer(Sys.getenv('RSleighNwsPort')),
    useUse=TRUE, create=FALSE)

waitForEnd <- function(nws) {
    nwsFind(nws, 'Sleigh ride over')
    system(sprintf('kill %s', cePid))
    nwsStore(nws, 'bye', 1)
    quit(save='no')
}

try(waitForEnd(nws))

Sys.sleep(3)
# still here (see trap in sh script --- but that's probably not working)?
# kill the subjob (still a possible race here...)
system(sprintf('kill %s', cePid))
nwsStore(nws, 'bye', 101)

EOF
