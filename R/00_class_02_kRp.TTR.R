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


#' S4 Class kRp.TTR
#'
#' This class is used for objects that are returned by \code{\link[koRpus:lex.div]{lex.div}} and its wrapper functions
#' (like \code{TTR}, \code{MSTTR}, \code{MTLD}, etc.).
#'
#' @slot param Relevant parameters of the given analysis, as given to the function call, see \code{\link[koRpus:lex.div]{lex.div}} for details.
#' @slot tt The analyzed text in tokenized form, with six elements ("tokens", "types", "lemmas", "num.tokens", "num.types", "num.lemmas").
#' @slot TTR Value of the classic type-token ratio. NA if not calculated.
#' @slot MSTTR Mean segmental type-token ratio, including the actual "MSTTR",
#'    TTR values of each segment ("TTR.seg"), and the number of dropped words due to segment size ("dropped"). NA if not calculated.
#' @slot MATTR Moving-average type-token ratio, including the actual "MATTR",
#'    TTR values of each window ("TTR.win"), and standard deviation of TTRs ("sd"). NA if not calculated.
#' @slot C.ld Herdan's C. NA if not calculated.
#' @slot R.ld Guiraud's R. NA if not calculated.
#' @slot CTTR Carroll's CTTR. NA if not calculated.
#' @slot U.ld Uber Index. NA if not calculated.
#' @slot S.ld Summer's S. NA if not calculated.
#' @slot K.ld Yule's K. NA if not calculated.
#' @slot Maas Maas' a. NA if not calculated.
#' @slot lgV0 Maas' \eqn{\lg{V_0}}. NA if not calculated.
#' @slot lgeV0 Maas' \eqn{\lg{}_{e}{V_0}}. NA if not calculated.
#' @slot Maas.grw Maas' relative type growth \eqn{V'}. NA if not calculated.
#' @slot HDD The actual HD-D value ("HDD"), a vector with the probabilies for each type ("type.probs"), a "summary"
#'    on these probabilities and their standard deviation "sd".
#' @slot MTLD Measure of textual lexical diversity, including the actual "MTLD", two matrices with detailed
#'    information on forward and backward factorization ("all.forw" & "all.back"), a named vector holding both calculated
#'    factors and their mean value ("factors"), and a named list with information on the number or tokens in each factor, both
#'    forward and backward, as well as their mean and standard deviation ("lengths"). NA if not calculated.
#' @slot MTLDMA Moving-average MTLD, including the actual "MTLDMA", its standard deviation, a list ("all") with detailed
#'    information on factorization and a named list with information on the number or tokens in each factor, as well as their mean
#'    and standard deviation ("lengths"). NA if not calculated.
#' @slot TTR.char TTR values, starting with the first steplength of tokens, then adding the next one, progressing until
#'    the whole text is analyzed. The matrix has two colums, one for the respective step ("token") and one for the actual values
#'    ("value"). Can be used to plot TTR characteristic curves. NA if not calculated.
#' @slot MATTR.char Equivalent to TTR.char, but calculated using MATTR algorithm. NA if not calculated.
#' @slot C.char Equivalent to TTR.char, but calculated using Herdan's C algorithm. NA if not calculated.
#' @slot R.char Equivalent to TTR.char, but calculated using Guiraud's R algorithm. NA if not calculated.
#' @slot CTTR.char Equivalent to TTR.char, but calculated using Carroll's CTTR algorithm. NA if not calculated.
#' @slot U.char Equivalent to TTR.char, but calculated using the Uber Index algorithm. NA if not calculated.
#' @slot S.char Equivalent to TTR.char, but calculated using Summer's S algorithm. NA if not calculated.
#' @slot K.char Equivalent to TTR.char, but calculated using Yule's K algorithm. NA if not calculated.
#' @slot Maas.char Equivalent to TTR.char, but calculated using Maas' a algorithm. NA if not calculated.
#' @slot lgV0.char Equivalent to TTR.char, but calculated using Maas' \eqn{\lg{V_0}} algorithm. NA if not calculated.
#' @slot lgeV0.char Equivalent to TTR.char, but calculated using Maas' \eqn{\lg{}_{e}{V_0}} algorithm. NA if not calculated.
#' @slot HDD.char Equivalent to TTR.char, but calculated using the HD-D algorithm. NA if not calculated.
#' @slot MTLD.char Equivalent to TTR.char, but calculated using the MTLD algorithm. NA if not calculated.
#' @slot MTLDMA.char Equivalent to TTR.char, but calculated using the moving-average MTLD algorithm. NA if not calculated.
#' @name kRp.TTR,-class
#' @import methods
#' @keywords classes
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @export
#' @aliases kRp.TTR,-class kRp.TTR-class
#' @rdname kRp.TTR-class

setClass("kRp.TTR",
    representation=representation(
      param="list",
      tt="list",
      TTR="numeric",
      MSTTR="list",
      MATTR="list",
      C.ld="numeric",
      R.ld="numeric",
      CTTR="numeric",
      U.ld="numeric",
      S.ld="numeric",
      K.ld="numeric",
      Maas="numeric",
      lgV0="numeric",
      lgeV0="numeric",
      Maas.grw="vector",
      HDD="list",
      MTLD="list",
      MTLDMA="list",
      TTR.char="matrix",
      MATTR.char="matrix",
      C.char="matrix",
      R.char="matrix",
      CTTR.char="matrix",
      U.char="matrix",
      S.char="matrix",
      K.char="matrix",
      Maas.char="matrix",
      lgV0.char="matrix",
      lgeV0.char="matrix",
      HDD.char="matrix",
      MTLD.char="matrix",
      MTLDMA.char="matrix"),
    prototype(
      param=list(segment=NA, factor.size=NA, min.tokens=NA, rand.sample=NA, window=NA,
        case.sens=NA, lemmatize=NA, log.base=NA),
      tt=list(tokens=NA, types=NA, lemmas=NA, num.tokens=numeric(), num.types=numeric(), num.lemmas=numeric()),
      TTR=numeric(),
      MSTTR=list(MSTTR=NA, TTR.seg=NA, dropped=NA, sd=NA),
      MATTR=list(MATTR=NA, TTR.win=NA, sd=NA),
      C.ld=numeric(),
      R.ld=numeric(),
      CTTR=numeric(),
      U.ld=numeric(),
      S.ld=numeric(),
      K.ld=numeric(),
      Maas=numeric(),
      lgV0=numeric(),
      lgeV0=numeric(),
      Maas.grw=c(a=NA, lgV0=NA, Vs=NA),
      HDD=list(HDD=NA, type.probs=NA, summary=NA, sd=NA),
      MTLD=list(MTLD=NA, all.forw=NA, all.back=NA, factors=c(forw=NA, mean=NA, back=NA),
        lengths=list(forw=NA, forw.compl=NA, mean=NA, mean.compl=NA, sd=NA, sd.compl=NA, back=NA, back.compl=NA)),
      MTLDMA=list(MTLDMA=NA, sd=NA, all=NA, lengths=list(factors=NA, mean=NA, sd=NA)),
      TTR.char=matrix(ncol=2, dimnames=list(c(), c("token", "value"))),
      MATTR.char=matrix(ncol=2, dimnames=list(c(), c("token", "value"))),
      C.char=matrix(ncol=2, dimnames=list(c(), c("token", "value"))),
      R.char=matrix(ncol=2, dimnames=list(c(), c("token", "value"))),
      CTTR.char=matrix(ncol=2, dimnames=list(c(), c("token", "value"))),
      U.char=matrix(ncol=2, dimnames=list(c(), c("token", "value"))),
      S.char=matrix(ncol=2, dimnames=list(c(), c("token", "value"))),
      K.char=matrix(ncol=2, dimnames=list(c(), c("token", "value"))),
      Maas.char=matrix(ncol=2, dimnames=list(c(), c("token", "value"))),
      lgV0.char=matrix(ncol=2, dimnames=list(c(), c("token", "value"))),
      lgeV0.char=matrix(ncol=2, dimnames=list(c(), c("token", "value"))),
      HDD.char=matrix(ncol=2, dimnames=list(c(), c("token", "value"))),
      MTLD.char=matrix(ncol=2, dimnames=list(c(), c("token", "value"))),
      MTLDMA.char=matrix(ncol=2, dimnames=list(c(), c("token", "value"))))
)

setValidity("kRp.TTR", function(object){
    param <- object@param
    tt <- object@tt
    MSTTR <- object@MSTTR
    MTLD <- object@MTLD
    MTLDMA <- object@MTLDMA

    param.names <- names(param)
    tt.names <- names(tt)
    MSTTR.names <- names(MSTTR)
    MTLD.names <- names(MTLD)
    MTLDMA.names <- names(MTLDMA)

    if(!identical(param.names, c("segment", "factor.size", "min.tokens", "rand.sample",
      "window", "case.sens", "lemmatize", "log.base"))){
      stop(simpleError("Invalid object: Wrong names (slot \"param\")."))
    } else {}

    if(!identical(tt.names, c("tokens", "types", "lemmas", "num.tokens", "num.types", "num.lemmas"))){
      stop(simpleError("Invalid object: Wrong names (slot \"tt\")."))
    } else {}

    if(!identical(MSTTR.names, c("MSTTR", "TTR.seg", "dropped")) |
        !identical(MTLD.names, c("MTLD", "all.forw", "all.back", "factors", "lengths")) |
        !identical(MTLDMA.names, c("MTLDMA", "factors", "mean", "lengths"))){
      stop(simpleError("Invalid object: Wrong names (slot \"MSTTR\", \"MTLD\" or \"MTLDMA\")."))
    } else {}

  return(TRUE)
})
