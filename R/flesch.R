# Copyright 2010-2016 Meik Michalke <meik.michalke@hhu.de>
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


#' Readability: Flesch Readability Ease
#' 
#' This is just a convenient wrapper function for \code{\link[koRpus:readability]{readability}}.
#'
#' Calculates the Flesch Readability Ease index. In contrast to
#' \code{\link[koRpus:readability]{readability}}, which by default calculates all possible indices,
#' this function will only calculate the Flesch RE value.
#'
#' Certain internationalisations of the parameters are also implemented. They can be used by setting
#' \code{parameters} to \code{"es"} (Fernandez-Huerta),  \code{"es-s"} (Szigriszt), \code{"nl"} (Douma), \code{"de"}
#' (Amstad) or \code{"fr"} (Kandel-Moles).
#' If \code{parameters="PSK"}, the revised parameters by Powers-Sumner-Kearl (1958) are used
#' to calculate a grade level.
#'
#' @param txt.file Either an object of class \code{\link[koRpus]{kRp.tagged-class}}, a character vector which must be be
#'    a valid path to a file containing the text to be analyzed, or a list of text features. If the latter, calculation
#'    is done by \code{\link[koRpus:readability.num]{readability.num}}. 
#' @param hyphen An object of class kRp.hyphen. If \code{NULL}, the text will be hyphenated automatically.
#' @param parameters Either a numeric vector with named magic numbers, defining the relevant parameters for the index, or
#'    a valid character string naming a preset for implemented languages (\code{"de"}, \code{"es"}, \code{"nl"}, \code{"fr"}).
#' @param ... Further valid options for the main function, see \code{\link[koRpus:readability]{readability}} for details.
#' @return An object of class \code{\link[koRpus]{kRp.readability-class}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords readability
#' @seealso \code{\link[koRpus:flesch.kincaid]{flesch.kincaid}} for grade levels,
#' \code{\link[koRpus:farr.jenkins.paterson]{farr.jenkins.paterson}} for a simplified Flesch formula.
#' @export
#' @examples
#' \dontrun{
#' flesch(german.tagged.text, parameters="de")
#' }

flesch <- function(txt.file, hyphen=NULL, parameters=c(const=206.835, asl=1.015, asw=84.6), ...){
  if(is.list(txt.file)){
    results <- readability.num(txt.features=txt.file, hyphen=hyphen, index="Flesch", parameters=list(Flesch=parameters), ...)
  } else {
    results <- readability(txt.file=txt.file, hyphen=hyphen, index="Flesch", parameters=list(Flesch=parameters), ...)
  }
  return(results)
}
