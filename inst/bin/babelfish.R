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

src = function(...) {
    source('nws.R')
}
tryCatch(library(nws), error=src)

bws = new('netWorkSpace', 'R babelfish')
while (1) {
  nwsStore(bws, 'doof', paste(capture.output(nwsFetch(bws, 'food')), collapse = '<p>'))
}
