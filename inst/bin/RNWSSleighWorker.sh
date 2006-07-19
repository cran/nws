#! /bin/sh
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

RProg=${RProg:-'R'}
cd ${RSleighWorkingDir:-'/tmp'}
LogDir=${RSleighLogDir:-'/tmp'}
if [ -n "${RSleighWorkerOut}" ]; then
    RSleighVerbose='TRUE'
    WorkerOut="${LogDir}/`basename ${RSleighWorkerOut}`"
else
    RSleighVerbose='FALSE'
    WorkerOut='/dev/null'
fi
export RSleighVerbose

# compute engine
$RProg --vanilla --slave <<'EOF' > ${WorkerOut} 2>&1 &

scriptDir = Sys.getenv('RSleighScriptDir')
library(nws)
cmdLaunch(as.logical(Sys.getenv('RSleighVerbose')))
EOF

export RCEPid=$!
export RCEHost=`hostname`

# sentinel
$RProg --vanilla --slave <<'EOF' > ${LogDir}/RSleighSentinelLog_${UID}_${RSleighID} 2>&1

scriptDir = Sys.getenv('RSleighScriptDir')
library(nws)

cePid = Sys.getenv('RCEPid');
ceHost = Sys.getenv('RCEHost');


nws = new('netWorkSpace', wsName=Sys.getenv('RSleighNwsName'), serverHost=Sys.getenv('RSleighNwsHost'), port=as.integer(Sys.getenv('RSleighNwsPort')), useUse=TRUE);

waitForEnd <- function(nws) {
 nwsFind(nws, 'Sleigh ride over')
 system(sprintf('kill %s', cePid))
 nwsStore(nws, 'bye', 1)
 quit(save='no')
}

try(waitForEnd(nws))

Sys.sleep(3)
# still here (see trap in sh script --- but that's probably not working)?, kill the subjob (still a possible race here...)
system(sprintf('kill %s', cePid))
nwsStore(nws, 'bye', 101)

EOF
