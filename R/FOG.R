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


#' Readability: Gunning FOG Index
#' 
#' This is just a convenient wrapper function for \code{\link[koRpus:readability]{readability}}.
#'
#' Calculates the Gunning FOG index. In contrast to \code{\link[koRpus:readability]{readability}},
#' which by default calculates all possible indices, this function will only calculate the index value.
#'
#' If \code{parameters="PSK"}, the revised parameters by Powers-Sumner-Kearl (1958) are used, and
#' if \code{parameters="NRI"}, the simplified parameters from the Navy Readability Indexes, respectively.
#'
#' @param txt.file Either an object of class \code{\link[koRpus]{kRp.tagged-class}}, a character vector which must be be
#'    a valid path to a file containing the text to be analyzed, or a list of text features. If the latter, calculation
#'    is done by \code{\link[koRpus:readability.num]{readability.num}}. 
#' @param hyphen An object of class kRp.hyphen. If \code{NULL}, the text will be hyphenated automatically.
#' @param parameters A list with named magic numbers and a vector with verb suffixes, defining the relevant parameters for the index,
#'    or one of \code{"PSK"} or \code{"NRI"}.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:readability]{readability}} for details.
#' @return An object of class \code{\link[koRpus]{kRp.readability-class}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @references
#'    DuBay, W.H. (2004). \emph{The Principles of Readability}. Costa Mesa: Impact Information.
#'      WWW: \url{http://www.impact-information.com/impactinfo/readability02.pdf}; 22.03.2011.
#'
#'    Powers, R.D, Sumner, W.A, & Kearl, B.E. (1958). A recalculation of four adult readability formulas,
#'      \emph{Journal of Educational Psychology}, 49(2), 99--105.
#' @keywords readability
#' @export
#' @examples
#' \dontrun{
#' FOG(tagged.text)
#' }

FOG <- function(txt.file, hyphen=NULL, parameters=list(syll=3, const=0.4, suffix=c("es", "ed", "ing")), ...){
  if(is.list(txt.file)){
    results <- readability.num(txt.features=txt.file, hyphen=hyphen, index="FOG", parameters=list(FOG=parameters), ...)
  } else {
    results <- readability(txt.file=txt.file, hyphen=hyphen, index="FOG", parameters=list(FOG=parameters), ...)
  }
  return(results)
}
