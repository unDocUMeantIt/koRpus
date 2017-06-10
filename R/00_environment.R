# Copyright 2016-2017 Meik Michalke <meik.michalke@hhu.de>
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

## setting up the internal environment

# empty environment for TreeTagger information
.koRpus.env <- new.env()
# set default for tag descriptions in objects
.koRpus.env[["add.desc"]] <- FALSE

## wrapper for paste0() needed?
if(isTRUE(R_system_version(getRversion()) < 2.15)){
  # if this is an older R version, we need a wrapper function for paste0()
  # which was introduced with R 2.15 as a more efficient shortcut to paste(..., sep="")
  paste0 <- function(..., collapse=NULL){
    return(paste(..., sep="", collapse=collapse))
  }
} else {}

# make sure the language packs are fully loaded
# after using 'imports' instead of 'depends' for these packages,
# we now need to call the resepective functions once
#' @importFrom sylly.de hyph.support.de
#' @importFrom sylly.en hyph.support.en
#' @importFrom sylly.es hyph.support.es
#' @importFrom sylly.fr hyph.support.fr
#' @importFrom sylly.it hyph.support.it
#' @importFrom sylly.ru hyph.support.ru
.onLoad <- function(...){
  sylly.de::hyph.support.de()
  sylly.en::hyph.support.en()
  sylly.es::hyph.support.es()
  sylly.fr::hyph.support.fr()
  sylly.it::hyph.support.it()
  sylly.ru::hyph.support.ru()
}
