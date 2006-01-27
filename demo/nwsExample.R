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

# assume that this has already been done nws.R 
#source('nws.R')

ws = new('netWorkSpace', 'r place')
cat('connected, listing contents of netWorkSpace (should be nothing there).\n', nwsListVars(ws), '\n')

nwsStore(ws, 'x', 1)
cat('should now see x.\n', nwsListVars(ws), '\n')

cat('nwsFind (but don\'t consume) x.\n', nwsFind(ws, 'x'), '\n')
cat('check that it is still there.\n', nwsListVars(ws), '\n')

cat('associate another value with x.\n')
nwsStore(ws, 'x', 2)
cat(nwsListVars(ws), '\n')

cat('consume values for x, should see them in order saved.\n',
    nwsFetch(ws, 'x'), '\n', nwsFetch(ws, 'x'), '\n')
cat('no more values for x... .\n', nwsListVars(ws), '\n')

cat('so try to nwsFetch and see what happens... .\n',
    nwsFetchTry(ws, 'x', 'no go'), '\n')

cat('create a single-value variable.\n')
nwsDeclare(ws, 'pi', 'single')
cat(nwsListVars(ws), '\n')

cat('get rid of x.\n')
nwsDeleteVar(ws, 'x')
cat(nwsListVars(ws), '\n')

cat('try to nwsStore two values to pi.\n')
nwsStore(ws, 'pi', 2.171828182)
nwsStore(ws, 'pi', 3.141592654)
cat(nwsListVars(ws), '\n')

cat('check that the right one was kept.\n', nwsFind(ws, 'pi'), '\n')

cat('what about the rest of the world?', nwsListWss(ws@server), '\n')

