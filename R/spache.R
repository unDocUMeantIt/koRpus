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


#' Readability: Spache Formula
#'
#' This is just a convenient wrapper function for \code{\link[koRpus:readability]{readability}}.
#'
#' Calculates the Spache Formula. In contrast to \code{\link[koRpus:readability]{readability}},
#' which by default calculates all possible indices, this function will only calculate the index value.
#'
#' By default the revised Spache formula is calculated. If \code{parameters="old"}, the original
#' parameters are used.
#'
#' This formula doesn't need syllable count.
#'
#' @param txt.file Either an object of class \code{\link[koRpus]{kRp.tagged-class}}, a character vector which must be be
#'    a valid path to a file containing the text to be analyzed, or a list of text features. If the latter, calculation
#'    is done by \code{\link[koRpus:readability.num]{readability.num}}. 
#' @param word.list A vector or matrix (with exactly one column) which defines familiar words. For valid results
#'    the short Dale-Chall list with 769 easy words should be used.
#' @param parameters A numeric vector with named magic numbers, defining the relevant parameters for the index.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:readability]{readability}} for details.
#' @return An object of class \code{\link[koRpus]{kRp.readability-class}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords readability
#' @export
#' @examples
#' \dontrun{
#' spache(tagged.text, word.list=spache.revised.wl)
#' }

spache <- function(txt.file, word.list, parameters=c(asl=0.121, dword=0.082, const=0.659), ...){
  # combine parameters
  if(identical(parameters, "old")){
    param.list <- "old"
  } else {
    param.list <- list(asl=parameters[["asl"]], dword=parameters[["dword"]], const=parameters[["const"]])
  }
  if(is.list(txt.file)){
    results <- readability.num(txt.features=txt.file, index="Spache", parameters=list(Spache=param.list), ...)
  } else {
    results <- readability(txt.file=txt.file, index="Spache", parameters=list(Spache=param.list), word.lists=list(Spache=word.list), ...)
  }
  return(results)
}
