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


#' Calculate lexical diversity
#' 
#' This function is a stripped down version of \code{\link[koRpus:lex.div]{lex.div}}. It does not analyze text,
#' but takes the numbers of tokens and types directly to calculate measures for which this information is sufficient:
#' \itemize{
#'    \item \code{"TTR"}{The classic \emph{Type-Token Ratio}}
#'    \item {\code{"C"}}{Herdan's \emph{C}}
#'    \item {\code{"R"}}{Guiraud's \emph{Root TTR}}
#'    \item {\code{"CTTR"}}{Carroll's \emph{Corrected TTR}}
#'    \item {\code{"U"}}{Dugast's \emph{Uber Index}}
#'    \item {\code{"S"}}{Summer's index}
#'    \item {\code{"Maas"}} {Maas' (\eqn{a^2})}
#' }
#' See \code{\link[koRpus:lex.div]{lex.div}} for further details on the formulae.
#' 
#' @param num.tokens Numeric, the number of tokens.
#' @param num.types Numeric, the number of types.
#' @param measure A character vector defining the measures to calculate.
#' @param log.base A numeric value defining the base of the logarithm. See \code{\link[base:log]{log}} for details.
#' @param quiet Logical. If \code{FALSE}, short status messages will be shown.
#'    \code{TRUE} will also suppress all potential warnings regarding the validation status of measures.
#' @return An object of class \code{\link[koRpus]{kRp.TTR-class}}.
#' @seealso \code{\link[koRpus:lex.div]{lex.div}}
#' @keywords LD
#' @references
#'    Maas, H.-D., (1972). \"Uber den Zusammenhang zwischen Wortschatzumfang und L\"ange eines Textes. \emph{Zeitschrift f\"ur
#'      Literaturwissenschaft und Linguistik}, 2(8), 73--96.
#'
#'    Tweedie. F.J. & Baayen, R.H. (1998). How Variable May a Constant Be? Measures of Lexical Richness in Perspective.
#'     \emph{Computers and the Humanities}, 32(5), 323--352.
#' @export
#' @examples
#' lex.div.num(104, 43)

lex.div.num <- function(num.tokens, num.types, measure=c("TTR","C","R","CTTR","U","S","Maas"), log.base=10, quiet=FALSE){

  # initialize result object
  lex.div.results <- new("kRp.TTR")

  ###################################
  ## diversity measures start here ##
  ###################################

  ## calculate TTR
  if("TTR" %in% measure){
    lex.div.results@TTR <- ttr.calc(num.tokens=num.tokens, num.types=num.types, type="TTR")
  } else {}

  ## calculate Herdan's C: log(types) / log(tokens)
  if("C" %in% measure){
    lex.div.results@C.ld <- ttr.calc(num.tokens=num.tokens, num.types=num.types, type="C", log.base=log.base)
  } else {}

  ## calculate Guiraud's R: types / sqrt(tokens)
  if("R" %in% measure){
    lex.div.results@R.ld <- ttr.calc(num.tokens=num.tokens, num.types=num.types, type="R")
  } else {}

  ## calculate Carroll's CTTR: types / 2*sqrt(tokens)
  if("CTTR" %in% measure){
    lex.div.results@CTTR <- ttr.calc(num.tokens=num.tokens, num.types=num.types, type="CTTR")
  } else {}

  ## calculate Uber Index U: (log(tokens))^2 / (log(tokens) - log(types))
  if("U" %in% measure){
    lex.div.results@U.ld <- ttr.calc(num.tokens=num.tokens, num.types=num.types, type="U", log.base=log.base)
  } else {}

  ## calculate Summer's S: LogLog(types) / LogLog(tokens)
  if("S" %in% measure){
    lex.div.results@S.ld <- ttr.calc(num.tokens=num.tokens, num.types=num.types, type="S", log.base=log.base)
  } else {}

  ## calculate Maas' a^2 and lgV0 indices
  if("Maas" %in% measure){
    lex.div.results@Maas <- ttr.calc(num.tokens=num.tokens, num.types=num.types, type="Maas", log.base=log.base)
    lex.div.results@lgV0 <- lgV0.calc(num.tokens=num.tokens, num.types=num.types, x=0, log.base=10)
    lex.div.results@lgeV0 <- lgV0.calc(num.tokens=num.tokens, num.types=num.types, x=0, log.base=exp(1))
  } else {}

  # document log base
  lex.div.results@param[["log.base"]] <- log.base

  ## for the time being, give a warning until all implementations have been validated
  needs.warning <- measure %in% c("S")
  if(!isTRUE(quiet) && any(needs.warning)){
    warning(paste0("Note: The implementations of these formulas are still subject to validation:\n  ",
    paste(measure[needs.warning], collapse=", "),
    "\n  Use the results with caution, even if they seem plausible!"), call.=FALSE)
  } else {}
  return(lex.div.results)
}
