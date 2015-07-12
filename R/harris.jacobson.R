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


#' Readability: Harris-Jacobson indices
#'
#' This is just a convenient wrapper function for \code{\link[koRpus:readability]{readability}}.
#'
#' This function calculates the revised Harris-Jacobson readability formulas (1 to 5), as described in their paper for the
#' 18th Annual Meeting of the College Reading Association (Harris & Jacobson, 1974). In contrast to \code{\link[koRpus:readability]{readability}},
#' which by default calculates all possible indices, this function will only calculate the index values.
#'
#' This formula doesn't need syllable count.
#'
#' @param txt.file Either an object of class \code{\link[koRpus]{kRp.tagged-class}}, a character vector which must be be
#'    a valid path to a file containing the text to be analyzed, or a list of text features. If the latter, calculation
#'    is done by \code{\link[koRpus:readability.num]{readability.num}}. 
#' @param word.list A vector or matrix (with exactly one column) which defines familiar words. For valid results
#'    the short Harris-Jacobson word list for grades 1 and 2 (english) should be used.
#' @param parameters A numeric vector with named magic numbers, defining the relevant parameters for all formulas of the index.
#' @param hj1 A numeric vector with named magic numbers for the first of the formulas.
#' @param hj2 A numeric vector with named magic numbers for the second of the formulas.
#' @param hj3 A numeric vector with named magic numbers for the third of the formulas.
#' @param hj4 A numeric vector with named magic numbers for the fourth of the formulas.
#' @param hj5 A numeric vector with named magic numbers for the fifth of the formulas.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:readability]{readability}} for details.
#' @return An object of class \code{\link[koRpus]{kRp.readability-class}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @references
#'    Harris, A.J. & Jacobson, M.D. (1974). Revised Harris-Jacobson readability formulas. In \emph{18th Annual Meeting of the College Reading Association}, Bethesda.
#' @keywords readability
#' @export
#' @examples
#' \dontrun{
#' harris.jacobson(tagged.text, word.list=harris.jacobson.wl)
#' }

harris.jacobson <- function(txt.file, word.list,
  parameters=c(char=6),
  hj1=c(dword=0.094, asl=0.168, const=0.502),
  hj2=c(dword=0.140, asl=0.153, const=0.560),
  hj3=c(asl=0.158, lword=0.055, const=0.355),
  hj4=c(dword=0.070, asl=0.125, lword=0.037, const=0.497),
  hj5=c(dword=0.118, asl=0.134, lword=0.032, const=0.424), ...){
  all.parameters <- list(char=parameters[["char"]], hj1=hj1, hj2=hj2,  hj3=hj3,  hj4=hj4,  hj5=hj5)
  if(is.list(txt.file)){
    results <- readability.num(txt.features=txt.file, hyphen=hyphen, index="Harris.Jacobson", parameters=list(Harris.Jacobson=all.parameters), ...)
  } else {
    results <- readability(txt.file=txt.file, hyphen=hyphen, index="Harris.Jacobson", parameters=list(Harris.Jacobson=all.parameters), word.lists=list(Harris.Jacobson=word.list), ...)
  }
  return(results)
}
