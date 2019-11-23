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

# define class union to make life easier
#' @include 01_class_01_kRp.tagged.R
#' @include 01_class_02_kRp.TTR.R
#' @include 01_class_03_kRp.txt.freq.R
#' @include 01_class_05_kRp.analysis.R
#' @include 01_class_06_kRp.corp.freq.R
#' @include 01_class_09_kRp.lang.R
#' @include 01_class_10_kRp.readability.R
setClassUnion("kRp.taggedText", members=c("kRp.tagged", "kRp.analysis", "kRp.txt.freq"))
