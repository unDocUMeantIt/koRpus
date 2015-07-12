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


#' Readability: Neue Wiener Sachtextformeln
#'
#' This is just a convenient wrapper function for \code{\link[koRpus:readability]{readability}}.
#'
#' This function calculates the new Wiener Sachtextformeln (formulas 1 to 4). In contrast to \code{\link[koRpus:readability]{readability}},
#' which by default calculates all possible indices, this function will only calculate the index values.
#'
#' @param txt.file Either an object of class \code{\link[koRpus]{kRp.tagged-class}}, a character vector which must be be
#'    a valid path to a file containing the text to be analyzed, or a list of text features. If the latter, calculation
#'    is done by \code{\link[koRpus:readability.num]{readability.num}}. 
#' @param hyphen An object of class kRp.hyphen. If \code{NULL}, the text will be hyphenated automatically.
#' @param parameters A numeric vector with named magic numbers, defining the relevant parameters for all formulas of the index.
#' @param nws1 A numeric vector with named magic numbers for the first of the formulas.
#' @param nws2 A numeric vector with named magic numbers for the second of the formulas.
#' @param nws3 A numeric vector with named magic numbers for the third of the formulas.
#' @param nws4 A numeric vector with named magic numbers for the fourth of the formulas.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:readability]{readability}} for details.
#' @return An object of class \code{\link[koRpus]{kRp.readability-class}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @aliases WSTF
#' @references
#'    Bamberger, R. & Vanecek, E. (1984). \emph{Lesen--Verstehen--Lernen--Schreiben}. Wien: Jugend und Volk.
#' @keywords readability
#' @export
#' @examples
#' \dontrun{
#' nWS(tagged.text)
#' }

nWS <- function(txt.file, hyphen=NULL,
  parameters=c(ms.syll=3, iw.char=6, es.syll=1),
  nws1=c(ms=19.35, sl=0.1672, iw=12.97, es=3.27, const=0.875),
  nws2=c(ms=20.07, sl=0.1682, iw=13.73, const=2.779),
  nws3=c(ms=29.63, sl=0.1905, const=1.1144),
  nws4=c(ms=27.44, sl=0.2656, const=1.693), ...){
  all.parameters <- list(ms.syll=parameters[["ms.syll"]], iw.char=parameters[["iw.char"]], es.syll=parameters[["es.syll"]], nws1=nws1, nws2=nws2,  nws3=nws3,  nws4=nws4)
  if(is.list(txt.file)){
    results <- readability.num(txt.features=txt.file, hyphen=hyphen, index="nWS", parameters=list(nWS=all.parameters), ...)
  } else {
    results <- readability(txt.file=txt.file, hyphen=hyphen, index="nWS", parameters=list(nWS=all.parameters), ...)
  }
  return(results)
}

#' @export
WSTF <- function(...) {
  stop(simpleError("This wrapper function is deprecated. Please use nWS() instead!"))
}
