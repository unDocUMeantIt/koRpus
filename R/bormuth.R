# Copyright 2010-2014 Meik Michalke <meik.michalke@hhu.de>
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


#' Readability: Bormuth's Mean Cloze and Grade Placement
#' 
#' This is just a convenient wrapper function for \code{\link[koRpus:readability]{readability}}.
#'
#' Calculates Bormuth's Mean Cloze and estimted grade placement. In contrast to
#' \code{\link[koRpus:readability]{readability}}, which by default calculates all possible indices,
#' this function will only calculate the index value.
#'
#' This formula doesn't need syllable count.
#'
#' @usage bormuth(txt.file, word.list, clz=35,
#'    meanc=c(const=0.886593, awl=0.08364, afw=0.161911,
#'      asl1=0.021401, asl2=0.000577, asl3=0.000005),
#'    grade=c(const=4.275, m1=12.881, m2=34.934, m3=20.388,
#'      c1=26.194, c2=2.046, c3=11.767, mc1=44.285, mc2=97.62,
#'      mc3=59.538), ...)
#' @param txt.file Either an object of class \code{\link[koRpus]{kRp.tagged-class}}, a character vector which must be be
#'    a valid path to a file containing the text to be analyzed, or a list of text features. If the latter, calculation
#'    is done by \code{\link[koRpus:readability.num]{readability.num}}. 
#' @param clz Integer, the cloze criterion score in percent.
#' @param meanc A numeric vector with named magic numbers, defining the relevant parameters for Mean Cloze calculation.
#' @param grade A numeric vector with named magic numbers, defining the relevant parameters for Grade Placement calculation.
#'    If omitted, Grade Placement will not be calculated.
#' @param word.list A vector or matrix (with exactly one column) which defines familiar words. For valid results
#'    the long Dale-Chall list with 3000 words should be used.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:readability]{readability}} for details.
#' @return An object of class \code{\link[koRpus]{kRp.readability-class}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords readability
#' @export
#' @examples
#' \dontrun{
#'   bormuth(tagged.text, word.list=new.dale.chall.wl)
#' }

bormuth <- function(txt.file, word.list, clz=35,
      meanc=c(const=0.886593, awl=0.08364, afw=0.161911, asl1=0.021401, asl2=0.000577, asl3=0.000005),
      grade=c(const=4.275, m1=12.881, m2=34.934, m3=20.388, c1=26.194, c2=2.046, c3=11.767, mc1=44.285, mc2=97.62, mc3=59.538), ...){
  param.list <- list(clz=clz, meanc=meanc, grade=grade)
  if(is.list(txt.file)){
    results <- readability.num(txt.features=txt.file, index="Bormuth", parameters=list(Bormuth=param.list), ...)
  } else {
    results <- readability(txt.file=txt.file, index="Bormuth", parameters=list(Bormuth=param.list), word.lists=list(Bormuth=word.list), ...)
  }
  return(results)
}
