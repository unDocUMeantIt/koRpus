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


#' Readability: Automated Readability Index (ARI)
#' 
#' This is just a convenient wrapper function for \code{\link[koRpus:readability]{readability}}.
#'
#' Calculates the Automated Readability Index (ARI). In contrast to \code{\link[koRpus:readability]{readability}},
#' which by default calculates all possible indices, this function will only calculate the index value.
#'
#' If \code{parameters="NRI"}, the simplified parameters from the Navy Readability Indexes are used, if set to
#' \code{ARI="simple"}, the simplified formula is calculated.
#'
#' This formula doesn't need syllable count.
#'
#' @param txt.file Either an object of class \code{\link[koRpus]{kRp.tagged-class}}, a character vector which must be be
#'    a valid path to a file containing the text to be analyzed, or a list of text features. If the latter, calculation
#'    is done by \code{\link[koRpus:readability.num]{readability.num}}. 
#' @param parameters A numeric vector with named magic numbers, defining the relevant parameters for the index.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:readability]{readability}} for details.
#' @return An object of class \code{\link[koRpus]{kRp.readability-class}}.
#' @references
#'    DuBay, W.H. (2004). \emph{The Principles of Readability}. Costa Mesa: Impact Information.
#'      WWW: \url{http://www.impact-information.com/impactinfo/readability02.pdf}; 22.03.2011.
#'
#'    Smith, E.A. & Senter, R.J. (1967). \emph{Automated readability index}. AMRL-TR-66-22. Wright-Paterson AFB, Ohio: Aerospace Medical Division.
#' @keywords readability
#' @export
#' @examples
#' \dontrun{
#' ARI(tagged.text)
#' }

ARI <- function(txt.file, parameters=c(asl=0.5, awl=4.71, const=21.43), ...){
  if(is.list(txt.file)){
    results <- readability.num(txt.features=txt.file, index="ARI", parameters=list(ARI=parameters), ...)
  } else {
    results <- readability(txt.file=txt.file, index="ARI", parameters=list(ARI=parameters), ...)
  }
  return(results)
}
