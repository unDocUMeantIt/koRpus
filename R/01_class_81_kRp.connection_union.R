# Copyright 2019 Meik Michalke <meik.michalke@hhu.de>
#
# This file is part of the R package koRpus.
#
# koRpus is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# koRpus is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with koRpus.  If not, see <http://www.gnu.org/licenses/>.

setOldClass("file")
setOldClass("pipe")
setOldClass("fifo")
setOldClass("url")
setOldClass("gzfile")
setOldClass("bzfile")
setOldClass("xzfile")
setOldClass("unz")
setOldClass("socketConnection")
setClassUnion("kRp.connection", members=c("file","pipe","fifo","url","gzfile","bzfile","xzfile","unz","socketConnection"))
