#!/usr/bin/env python
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

import sys, os, tempfile, traceback, subprocess
from os import environ as Env

try:
    import win32api
    _winuser = win32api.GetUserName()
except ImportError:
    _winuser = None

if sys.platform.startswith('win'):
    _TMPDIR = tempfile.gettempdir() or '\\TEMP'
    _NULFILE = 'NUL'
else:
    _TMPDIR = tempfile.gettempdir() or '/tmp'
    _NULFILE = '/dev/null'

# this function is called by the worker process via the preexec_fn parameter
def setpg():
    try: os.setpgid(0, 0)
    except: pass

def main():
    # initialize variables from environment
    modulePath = Env.get('PythonSleighModulePath', '')
    logDir = Env.get('RSleighLogDir')
    if not logDir or not os.path.isdir(logDir):
        logDir = _TMPDIR
    outfile = Env.get('RSleighWorkerOut')
    if outfile:
        outfile = os.path.join(logDir, os.path.split(outfile)[1])
    else:
        outfile = _NULFILE
    Env['RSleighLogFile'] = outfile
    verbose = Env.has_key('RSleighWorkerOut') and 'TRUE' or 'FALSE'
    nwsName = Env['RSleighNwsName']
    nwsHost = Env.get('RSleighNwsHost', 'localhost')
    nwsPort = int(Env.get('RSleighNwsPort', '8765'))
    print nwsName, nwsHost, nwsPort

    # create the script file for the worker to execute
    script = '''\
# use RSleighScriptDir to add the directory that contains
# the nws package to the library path if possible
# (and without modifying the worker's global environment)
nwsPkg <- ''
local({
    scriptDir <- Sys.getenv('RSleighScriptDir')
    if (basename(scriptDir) == 'bin') {
        nwsDir <- dirname(scriptDir)
        nwsPkg <<- basename(nwsDir)
        if (nwsPkg %%in%% c('nws', 'nwsPro')) {
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
cmdLaunch(%s)
''' % verbose
    fd, tmpname = tempfile.mkstemp(suffix='.R', prefix='__nws', text=True)
    tmpfile = os.fdopen(fd, 'w')
    tmpfile.write(script)
    tmpfile.close()

    print "executing R worker"
    rprog = Env.get('RProg', 'R')
    argv = [rprog, '--vanilla', '--slave']
    out = open(outfile, 'w')

    try:
        if sys.platform.startswith('win'):
            p = subprocess.Popen(argv, stdin=open(tmpname), stdout=out,
                    stderr=subprocess.STDOUT)
        else:
            p = subprocess.Popen(argv, stdin=open(tmpname), stdout=out,
                    stderr=subprocess.STDOUT, preexec_fn=setpg)
    except OSError, e:
        print >> sys.stderr, 'error executing command:', argv
        if not os.path.exists(rprog):
            print >> sys.stderr, rprog, 'does not exist'
        raise e

    if hasattr(p, '_handle'):
        wpid = int(p._handle)  # Windows
    else:
        wpid = p.pid  # Unix
        try: os.setpgid(wpid, 0)
        except: pass

    print "waiting for shutdown"
    sys.path[1:1] = modulePath.split(os.pathsep)

    try:
        try:
            from nws.client import NetWorkSpace
        except ImportError:
            from nwsclient import NetWorkSpace
        nws = NetWorkSpace(nwsName, nwsHost, nwsPort, useUse=True, create=False)
        sentinel(nws)

        print "killing R worker"
        kill(wpid)
    except ImportError:
        # nws-python isn't installed, so just wait for the worker to exit
        p.wait()

    print "all done"

    # XXX might need to wait for child to die on Windows
    try: os.remove(tmpname)
    except: pass

    # just being paranoid
    sys.stdout.flush()
    sys.stderr.flush()

def getenv(args):
    e = {}
    for kv in args:
        try:
            k, v = [x.strip() for x in kv.split('=', 1)]
            if k: e[k] = v
        except:
            print "warning: bad argument:", kv
    return e

def setup(f):
    if os.path.isabs(f):
        log = f
    else:
        log = os.path.join(Env.get('RSleighLogDir', _TMPDIR), f)

    # open the output file and redirect stdout and stderr
    try:
        # create an unbuffered file for writing
        outfile = open(log, 'w', 0)
        sys.stdout = outfile
        sys.stderr = outfile
    except:
        traceback.print_exc()
        print "warning: unable to create file:", log

    # cd to the specified directory
    wd = Env.get('RSleighWorkingDir')
    if not wd or not os.path.isdir(wd):
        wd = _TMPDIR

    try:
        os.chdir(wd)
    except:
        traceback.print_exc()
        print "warning: unable to cd to", wd

    # this information will normally seem rather obvious
    print "current working directory:", os.getcwd()

def sentinel(nws):
    print "waiting for sleigh to be stopped"

    try:
        nws.find('Sleigh ride over')
        nws.store('bye', 'Sleigh ride over')
    except:
        try: nws.store('bye', str(sys.exc_info()[1]))
        except: pass

    print "sentinel returning"

def kill(pid):
    try:
        try:
            import win32api
            # the "pid" is really a handle on Windows
            win32api.TerminateProcess(pid, -1)
            win32api.CloseHandle(pid)  # XXX not sure about this
        except ImportError:
            try:
                from signal import SIGTERM as sig
            except ImportError:
                print "couldn't import signal module"
                sig = 15
            try: os.kill(-pid, sig)
            except: os.kill(pid, sig)
    except OSError:
        # process is already dead, so ignore it
        pass
    except:
        traceback.print_exc()

if __name__ == '__main__':
    try:
        # treat all arguments as environment settings
        Env.update(getenv(sys.argv[1:]))

        # user name is used in log file name to avoid permission problems
        user = Env.get('USER') or Env.get('USERNAME') or \
                _winuser or 'nwsuser'
        f = 'RSleighSentinelLog_' + user + '_' + \
                Env.get('RSleighID', 'X') + '.txt'
        setup(f)

        if not Env.has_key('RSleighNwsName'):
            print "RSleighNwsName variable is not set"
            print >> sys.__stderr__, "RSleighNwsName variable is not set"
        else:
            main()
    except:
        traceback.print_exc()
