#!/usr/bin/env python
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

import os, sys, string, getopt, time
from nwsClient import NwsServer

def usage(code, msg=""):
    print >> sys.stderr, "usage: %s [-h host] [-p port] [-R path]" % \
            os.path.basename(sys.argv[0])

    if msg:
      print >> sys.stderr, msg

    sys.exit(code)

host = 'localhost'
port = 8765
rcmd = '/usr/bin/R'

try:
    opts, args = getopt.getopt(sys.argv[1:], "h:p:R:")
    for opt, val in opts:
        if opt == '-h':
            host = val
        elif opt == '-p':
            port = int(val)
        elif opt == '-R':
            rcmd = val
        else:
            usage(2, "internal error")
except ValueError:
    usage(1, "option " + opt + " requires an integer argument")
except getopt.GetoptError, e:
    usage(1, str(e))
except:
    usage(1)

if len(args) > 0:
    usage(1, "illegal arguments: " + " ".join(args))

if not os.path.isfile(rcmd):
    usage(1, rcmd + " doesn't exist (try using the -R option?)")

nwss = NwsServer(host, port)

dir, base = os.path.split(sys.argv[0])
rmon = os.path.join(dir, "monitor.R")
argv = ["--vanilla", "--quiet", "CMD", "BATCH", rmon]

sleighs = {}

while 1:
    listing = nwss.listWss()
    lines = listing.split(os.linesep)
    for line in lines:
        if line[1:].startswith("sleigh_ride"):
            sleigh = line.split()[0]
            if sleigh not in sleighs:
                os.environ['RSLEIGHWS'] = sleigh
                sleighs[sleigh] = os.spawnv(os.P_NOWAIT, rcmd, argv)
                print "Started a monitor for %s (%d)" % \
                        (sleigh, sleighs[sleigh])

    time.sleep(10)
