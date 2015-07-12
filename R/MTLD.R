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


#' Lexical diversity: Measure of Textual Lexical Diversity (MTLD)
#'
#' This is just a convenient wrapper function for \code{\link[koRpus:lex.div]{lex.div}}.
#'
#' This function calculates the measure of textual lexical diversity (MTLD; see McCarthy & Jarvis, 2010). In contrast to
#' \code{\link[koRpus:lex.div]{lex.div}}, which by default calculates all possible measures and
#' their progressing characteristics, this function will only calculate the MTLD value, and characteristics are
#' off by default.
#' 
#' If you set \code{MA=TRUE}, the newer MTLD-MA (moving-average method) is used instead of the classic MTLD.
#'
#' @param txt An object of either class \code{\link[koRpus]{kRp.tagged-class}} or \code{\link[koRpus]{kRp.analysis-class}}, containing the tagged text to be analyzed.
#' @param factor.size A real number between 0 and 1, defining the MTLD factor size.
#' @param min.tokens An integer value, how many tokens a full factor must at least have to be considered for the MTLD-MA result.
#' @param detailed Logical, whether full details of the analysis should be calculated. It defines
#'    if all factors should be kept in the object. This slows down calculations considerably.
#' @param char Logical, defining whether data for plotting characteristic curves should be calculated.
#' @param MA Logical, defining whether the newer moving-average algorithm (MTLD-MA) should be calculated.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:lex.div]{lex.div}} for details.
#' @return An object of class \code{\link[koRpus]{kRp.TTR-class}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords LD
#' @seealso \code{\link[koRpus:kRp.POS.tags]{kRp.POS.tags}},
#'  \code{\link[koRpus]{kRp.tagged-class}}, \code{\link[koRpus]{kRp.TTR-class}}
#' @references McCarthy, P. M. & Jarvis, S. (2010). MTLD, vocd-D, and HD-D: A validation study of sophisticated approaces to lexical diversity assessment.
#'    \emph{Behaviour Research Methods}, 42(2), 381--392.
#' @export
#' @examples
#' \dontrun{
#' MTLD(tagged.text)
#' }

MTLD <- function(txt, factor.size=0.72, min.tokens=9, detailed=FALSE, char=FALSE, MA=FALSE, ...){
  if(isTRUE(MA)){
    measure <- "MTLD-MA"
  } else {
    measure <- "MTLD"
  }

  if(isTRUE(char)){
    char.value <- measure
  } else {
    char.value <- c()
  }

  results <- lex.div(txt=txt, factor.size=factor.size, min.tokens=min.tokens, detailed=detailed, measure=measure, char=char.value, ...)
  return(results)
}
