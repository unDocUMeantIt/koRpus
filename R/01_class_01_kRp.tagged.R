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

init.kRp.tagged.df <- function(rows=1){
  val <- rep(NA, rows)
  return(data.frame(
    doc_id=val,
    token=val,
    tag=val,
    lemma=val,
    lttr=val,
    wclass=val,
    desc=val,
    stop=val,
    stem=val,
    idx=val,
    sntc=val
  ))
}

valid.TT.res.kRp.tagged <- colnames(init.kRp.tagged.df())

#' S4 Class kRp.tagged
#'
#' This class is used for objects that are returned by \code{\link[koRpus:treetag]{treetag}} or \code{\link[koRpus:tokenize]{tokenize}}.
#'
#' @slot lang A character string, naming the language that is assumed for the tokenized text in this object.
#' @slot desc Descriptive statistics of the tagged text.
#' @slot TT.res Results of the called tokenizer and POS tagger. The data.frame has eight columns:
#'    \describe{
#'      \item{\code{doc_id}:}{Optional document identifier.}
#'      \item{\code{token}:}{The tokenized text.}
#'      \item{\code{tag}:}{POS tags for each token.}
#'      \item{\code{lemma}:}{Lemma for each token.}
#'      \item{\code{lttr}:}{Number of letters.}
#'      \item{\code{wclass}:}{Word class.}
#'      \item{\code{desc}:}{A short description of the POS tag.}
#'      \item{\code{stop}:}{Logical, \code{TRUE} if token is a stopword.}
#'      \item{\code{stem}:}{Stemmed token.}
#'      \item{\code{idx}:}{Index number of token in this document.}
#'      \item{\code{sntc}:}{Number of sentence in this document.}
#'    }
#'    This data.frame structure adheres to the "Text Interchange Formats" guidelines set out by rOpenSci[1].
#' @note There is also \code{as()} methods to transform objects from other koRpus classes into kRp.tagged.
#' @name kRp.tagged,-class
#' @aliases kRp.tagged,-class kRp.tagged-class
#' @import methods
#' @references
#'    [1] Text Interchange Formats (\url{https://github.com/ropensci/tif})
#' @keywords classes
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @export
#' @rdname kRp.tagged-class

setClass("kRp.tagged",
    representation=representation(
      lang="character",
      desc="list",
      TT.res="data.frame"),
    prototype(
      lang=character(),
      desc=list(),
      TT.res=init.kRp.tagged.df()
    )
)

setValidity("kRp.tagged", function(object){
    TT.res <- object@TT.res
    TT.res.names <- dimnames(TT.res)[[2]]

    if(!is.character(object@lang)){
      stop(simpleError("Invalid object: Slot \"lang\" must be of class character!"))
    } else {}

    if(!identical(TT.res.names, valid.TT.res.kRp.tagged)){
      wrongCols <- TT.res.names[!TT.res.names %in% valid.TT.res.kRp.tagged]
      missingCols <- valid.TT.res.kRp.tagged[!valid.TT.res.kRp.tagged %in% TT.res.names]
      if(length(wrongCols) > 0){
        warning(
          paste0(
            "Invalid object: Wrong columns in in slot \"TT.res\", please try fixObject():\n  ",
            paste0(wrongCols, collapse=", ")
          ),
          call.=FALSE
        )
      } else {}
      if(length(missingCols) > 0){
        warning(
          paste0(
            "Invalid object: Missing columns in slot \"TT.res\", please try fixObject():\n  ",
            paste0(missingCols, collapse=", ")
          ),
          call.=FALSE)
      }
    } else {}

  return(TRUE)
})
