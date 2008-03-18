#!/usr/bin/python
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

import sys, os

def main(argv):
    try:
        from signal import signal, SIGHUP, SIG_IGN
        signal(SIGHUP, SIG_IGN)

        for i in range(3, 128):
            try: os.close(i)
            except: pass

        os.spawnvp(os.P_NOWAIT, argv[0], argv)
    except ImportError:
        # presumably we're on Windows
        import win32process
        try:
            from nws.util import msc_argv2str, which
        except ImportError:
            # nws-python isn't installed, but we just need nwsutil.py
            from nwsutil import msc_argv2str, which

        if not os.path.isabs(argv[0]):
            argv[0] = which(argv[0])[0]

        commandLine = msc_argv2str(argv)
        processSecurityAttributes = None
        threadSecurityAttributes = None
        fInheritHandles = 0
        creationFlags = win32process.CREATE_NO_WINDOW
        environment = None
        currentDirectory = None
        startupInfo = win32process.STARTUPINFO()

        procHandle, threadHandle, procId, threadId = win32process.CreateProcess(
                argv[0], commandLine,
                processSecurityAttributes, threadSecurityAttributes,
                fInheritHandles, creationFlags,
                environment, currentDirectory,
                startupInfo)

if __name__ == '__main__':
    main(sys.argv[1:])
