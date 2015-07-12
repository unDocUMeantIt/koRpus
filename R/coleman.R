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


#' Readability: Coleman's Formulas
#' 
#' This is just a convenient wrapper function for \code{\link[koRpus:readability]{readability}}.
#'
#' This function calculates the four readability formulas by Coleman. In contrast to
#' \code{\link[koRpus:readability]{readability}}, which by default calculates all possible
#' indices, this function will only calculate the index value.
#'
#' @param txt.file Either an object of class \code{\link[koRpus]{kRp.tagged-class}}, a character vector which must be be
#'    a valid path to a file containing the text to be analyzed, or a list of text features. If the latter, calculation
#'    is done by \code{\link[koRpus:readability.num]{readability.num}}. 
#' @param hyphen An object of class kRp.hyphen. If \code{NULL}, the text will be hyphenated automatically.
#' @param parameters A numeric vector with named magic numbers, defining the relevant parameters for all formulas of the index.
#' @param clz1 A numeric vector with named magic numbers for the first formula.
#' @param clz2 A numeric vector with named magic numbers for the second formula.
#' @param clz3 A numeric vector with named magic numbers for the third formula.
#' @param clz4 A numeric vector with named magic numbers for the fourth formula.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:readability]{readability}} for details.
#' @return An object of class \code{\link[koRpus]{kRp.readability-class}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords readability
#' @export
#' @examples
#' \dontrun{
#' coleman(tagged.text)
#' }
coleman <- function(txt.file, hyphen=NULL,
    parameters=c(syll=1),
    clz1=c(word=1.29, const=38.45),
    clz2=c(word=1.16, sntc=1.48, const=37.95),
    clz3=c(word=1.07, sntc=1.18, pron=0.76, const=34.02),
    clz4=c(word=1.04, sntc=1.06, pron=0.56, prep=0.36, const=26.01), ...){
  all.parameters <- list(syll=parameters[["syll"]], clz1=clz1, clz2=clz2,  clz3=clz3,  clz4=clz4)
  if(is.list(txt.file)){
    results <- readability.num(txt.features=txt.file, hyphen=hyphen, index="Coleman", parameters=list(Coleman=all.parameters), ...)
  } else {
    results <- readability(txt.file=txt.file, hyphen=hyphen, index="Coleman", parameters=list(Coleman=all.parameters), ...)
  }
  return(results)
}
