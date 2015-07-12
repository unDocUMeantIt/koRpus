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


#' Readability: Coleman-Liau Index
#' 
#' This is just a convenient wrapper function for \code{\link[koRpus:readability]{readability}}.
#'
#' Calculates the Coleman-Liau index. In contrast to \code{\link[koRpus:readability]{readability}},
#' which by default calculates all possible indices, this function will only calculate the index value.
#'
#' This formula doesn't need syllable count.
#'
#' @param txt.file Either an object of class \code{\link[koRpus]{kRp.tagged-class}}, a character vector which must be be
#'    a valid path to a file containing the text to be analyzed, or a list of text features. If the latter, calculation
#'    is done by \code{\link[koRpus:readability.num]{readability.num}}. 
#' @param ecp A numeric vector with named magic numbers, defining the relevant parameters for the cloze percentage estimate.
#' @param grade A numeric vector with named magic numbers, defining the relevant parameters to calculate grade equvalent for ECP values.
#' @param short A numeric vector with named magic numbers, defining the relevant parameters for the short form of the formula.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:readability]{readability}} for details.
#' @return An object of class \code{\link[koRpus]{kRp.readability-class}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords readability
#' @export
#' @examples
#' \dontrun{
#' coleman.liau(tagged.text)
#' }

coleman.liau <- function(txt.file,
        ecp=c(const=141.8401, char=0.21459, sntc=1.079812),
        grade=c(ecp=-27.4004, const=23.06395),
        short=c(awl=5.88, spw=29.6, const=15.8), ...){
  # combine parameters
  param.list <- list(ecp=ecp, grade=grade, short=short)
  if(is.list(txt.file)){
    results <- readability.num(txt.features=txt.file, index="Coleman.Liau", parameters=list(Coleman.Liau=param.list), ...)
  } else {
    results <- readability(txt.file=txt.file, index="Coleman.Liau", parameters=list(Coleman.Liau=param.list), ...)
  }
  return(results)
}
