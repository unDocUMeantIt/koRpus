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


#' Lexical diversity: Herdan's C
#' 
#' This is just a convenient wrapper function for \code{\link[koRpus:lex.div]{lex.div}}.
#'
#' Calculates Herdan's C. In contrast to
#' \code{\link[koRpus:lex.div]{lex.div}}, which by default calculates all possible measures and
#' their progressing characteristics, this function will only calculate the C value, and characteristics are
#' off by default.
#'
#' @param txt An object of either class \code{\link[koRpus]{kRp.tagged-class}} or \code{\link[koRpus]{kRp.analysis-class}}, containing the tagged text to be analyzed.
#' @param char Logical, defining whether data for plotting characteristic curves should be calculated.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:lex.div]{lex.div}} for details.
#' @return An object of class \code{\link[koRpus]{kRp.TTR-class}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords LD
#' @seealso \code{\link[koRpus:kRp.POS.tags]{kRp.POS.tags}},
#'  \code{\link[koRpus]{kRp.tagged-class}}, \code{\link[koRpus]{kRp.TTR-class}}
#' @export
#' @examples
#' \dontrun{
#' C.ld(tagged.text)
#' }

C.ld <- function(txt, char=FALSE, ...){
  if(isTRUE(char)){
    char.value <- "C"
  } else {
    char.value <- c()
  }
  results <- lex.div(txt=txt, measure="C", char=char.value, ...)
  return(results)
}
