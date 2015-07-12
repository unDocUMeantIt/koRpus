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


#' Lexical diversity: HD-D (vocd-d)
#' 
#' This is just a convenient wrapper function for \code{\link[koRpus:lex.div]{lex.div}}.
#'
#' This function calculates HD-D, an idealized version of vocd-d (see McCarthy & Jarvis, 2007). In contrast to
#' \code{\link[koRpus:lex.div]{lex.div}}, which by default calculates all possible measures and
#' their progressing characteristics, this function will only calculate the HD-D value, and characteristics are
#' off by default.
#'
#' @param txt An object of either class \code{\link[koRpus]{kRp.tagged-class}} or \code{\link[koRpus]{kRp.analysis-class}}, containing the tagged text to be analyzed.
#' @param rand.sample An integer value, how many tokens should be assumed to be drawn for calculating HD-D.
#' @param char Logical, defining whether data for plotting characteristic curves should be calculated.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:lex.div]{lex.div}} for details.
#' @return An object of class \code{\link[koRpus]{kRp.TTR-class}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @references
#'   McCarthy, P.M. & Jarvis, S. (2007). vocd: A theoretical and empirical evaluation. \emph{Language Testing}, 24(4), 459--488.
#' @keywords LD
#' @seealso \code{\link[koRpus:kRp.POS.tags]{kRp.POS.tags}},
#'  \code{\link[koRpus]{kRp.tagged-class}}, \code{\link[koRpus]{kRp.TTR-class}}
#' @export
#' @examples
#' \dontrun{
#' HDD(tagged.text)
#' }

HDD <- function(txt, rand.sample=42, char=FALSE, ...){
  if(isTRUE(char)){
    char.value <- "HD-D"
  } else {
    char.value <- c()
  }
  results <- lex.div(txt=txt, rand.sample=rand.sample, measure="HD-D", char=char.value, ...)
  return(results)
}
