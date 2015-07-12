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


#' Lexical diversity: Moving-Average Type-Token Ratio (MATTR)
#'
#' This is just a convenient wrapper function for \code{\link[koRpus:lex.div]{lex.div}}.
#'
#' This function calculates the moving-average type-token ratio (MATTR). In contrast to
#' \code{\link[koRpus:lex.div]{lex.div}}, which by default calculates all possible measures and
#' their progressing characteristics, this function will only calculate the MATTR value.
#'
#' @param txt An object of either class \code{\link[koRpus]{kRp.tagged-class}} or \code{\link[koRpus]{kRp.analysis-class}}, containing the tagged text to be analyzed.
#' @param window An integer value for MATTR, defining how many tokens the moving window should include.
#' @param char Logical, defining whether data for plotting characteristic curves should be calculated.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:lex.div]{lex.div}} for details.
#' @return An object of class \code{\link[koRpus]{kRp.TTR-class}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords LD
#' @seealso \code{\link[koRpus:kRp.POS.tags]{kRp.POS.tags}},
#'  \code{\link[koRpus]{kRp.tagged-class}}, \code{\link[koRpus]{kRp.TTR-class}}
#' @references
#'    Covington, M.A. & McFall, J.D. (2010). Cutting the Gordian Knot: The Moving-Average Type-Token Ratio (MATTR).
#'      \emph{Journal of Quantitative Linguistics}, 17(2), 94--100.
#' @export
#' @examples
#' \dontrun{
#' MATTR(tagged.text)
#' }

MATTR <- function(txt, window=100, char=FALSE, ...){
  if(isTRUE(char)){
    char.value <- "MATTR"
  } else {
    char.value <- c()
  }

  results <- lex.div(txt=txt, window=window, measure="MATTR", char=char.value, ...)
  return(results)
}
