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


#' Readability: Simple Measure of Gobbledygook (SMOG)
#'
#' This is just a convenient wrapper function for \code{\link[koRpus:readability]{readability}}.
#'
#' This function calculates the Simple Measure of Gobbledygook (SMOG). In contrast to \code{\link[koRpus:readability]{readability}},
#' which by default calculates all possible indices, this function will only calculate the index value.
#'
#' By default calculates formula D by McLaughlin (1969).
#' If \code{parameters} is set to \code{SMOG="C"}, formula C will be calculated.
#' If \code{parameters} is set to \code{SMOG="simple"}, the simplified formula is used, and
#' if \code{parameters="de"}, the formula adapted to German texts ("Qu", Bamberger & Vanecek, 1984, p. 78).
#'
#' @param txt.file Either an object of class \code{\link[koRpus]{kRp.tagged-class}}, a character vector which must be be
#'    a valid path to a file containing the text to be analyzed, or a list of text features. If the latter, calculation
#'    is done by \code{\link[koRpus:readability.num]{readability.num}}. 
#' @param hyphen An object of class kRp.hyphen. If \code{NULL}, the text will be hyphenated automatically.
#' @param parameters A numeric vector with named magic numbers, defining the relevant parameters for the index.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:readability]{readability}} for details.
#' @return An object of class \code{\link[koRpus]{kRp.readability-class}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @references
#'    Bamberger, R. & Vanecek, E. (1984). \emph{Lesen--Verstehen--Lernen--Schreiben}. Wien: Jugend und Volk.
#'
#'    McLaughlin, G.H. (1969). SMOG grading -- A new readability formula. \emph{Journal of Reading}, 12(8), 639--646.
#' @keywords readability
#' @export
#' @examples
#' \dontrun{
#' SMOG(tagged.text)
#' }

SMOG <- function(txt.file, hyphen=NULL, parameters=c(syll=3, sqrt=1.043, fact=30, const=3.1291, sqrt.const=0), ...){
  if(is.list(txt.file)){
    results <- readability.num(txt.features=txt.file, hyphen=hyphen, index="SMOG", parameters=list(SMOG=parameters), ...)
  } else {
    results <- readability(txt.file=txt.file, hyphen=hyphen, index="SMOG", parameters=list(SMOG=parameters), ...)
  }
  return(results)
}
