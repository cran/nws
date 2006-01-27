#! /bin/sh
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

trap exit SIGCHLD

# need to figure out how to get to the correct directory (use a temp dir?)
cd ${RNWSSleighWorkingDirectory:-/tmp}

${RPROG:-R} --vanilla --slave <<EOF > ${RSleighWorkerOut:-/dev/null} 2>&1 &

scriptDir = Sys.getenv('RSleighScriptDir')
library(nws)
workerLoop()
EOF

export RCEPid=$!
export RCEHost=`hostname`

# start sentinel
${RPROG:-R} --vanilla --slave <<EOF > RSleighSentinelLog_${UID}_${RSleighRank} 2>&1

scriptDir = Sys.getenv('RSleighScriptDir')
library(nws)

cePid = Sys.getenv('RCEPid');
ceHost = Sys.getenv('RCEHost');

nws = new('netWorkSpace', wsName=Sys.getenv('RSleighNwsName'), serverHost=Sys.getenv('RSleighNwsHost'), port=as.integer(Sys.getenv('RSleighNwsPort')), useUse=TRUE);
nwsStore(nws, sprintf('Worker %s on host %s', cePid, ceHost), 1)

waitForEnd = function(nws) {
 nwsFind(nws, 'Sleigh ride over')
 system(sprintf('kill %s', cePid))
 nwsStore(nws, 'bye', 1)
 quit(save='no')
}

try(waitForEnd(nws))

# hmmm ... looks like the rug was pulled out from under us. wait a
# bit for the shell scripts trap to fire ...
Sys.sleep(3)
# still here (see trap in sh script --- but that's probably not working)?, kill the subjob (still a possible race here...)
system(sprintf('kill %s', cePid))
nwsStore(nws, 'bye', 101)

EOF
