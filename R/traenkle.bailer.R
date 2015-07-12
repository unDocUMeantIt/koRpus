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


#' Readability: Traenkle-Bailer Formeln
#' 
#' This is just a convenient wrapper function for \code{\link[koRpus:readability]{readability}}.
#'
#' This function calculates the two formulae by Tr\"ankle-Bailer, which are based on the Dickes-Steiwer formulae.
#' In contrast to \code{\link[koRpus:readability]{readability}}, which by default calculates all possible indices,
#' this function will only calculate the index values.
#'
#' This formula doesn't need syllable count.
#'
#' @param txt.file Either an object of class \code{\link[koRpus]{kRp.tagged-class}}, a character vector which must be be
#'    a valid path to a file containing the text to be analyzed, or a list of text features. If the latter, calculation
#'    is done by \code{\link[koRpus:readability.num]{readability.num}}. 
#' @param TB1 A numeric vector with named magic numbers for the first of the formulas.
#' @param TB2 A numeric vector with named magic numbers for the second of the formulas.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:readability]{readability}} for details.
#' @return An object of class \code{\link[koRpus]{kRp.readability-class}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords readability
#' @export
#' @examples
#' \dontrun{
#'   traenkle.bailer(tagged.text)
#' }

traenkle.bailer <- function(txt.file,
      TB1=c(const=224.6814, awl=79.8304, asl=12.24032, prep=1.292857),
      TB2=c(const=234.1063, awl=96.11069, prep=2.05444, conj=1.02805), ...){
  all.parameters <- list(TB1=TB1, TB2=TB2)
  if(is.list(txt.file)){
    results <- readability.num(txt.features=txt.file, index="Traenkle.Bailer", parameters=list(Traenkle.Bailer=all.parameters), ...)
  } else {
    results <- readability(txt.file=txt.file, index="Traenkle.Bailer", parameters=list(Traenkle.Bailer=all.parameters), ...)
  }
  return(results)
}
