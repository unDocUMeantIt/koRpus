# Copyright 2010-2019 Meik Michalke <meik.michalke@hhu.de>
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
    doc_id=factor(val),
    token=val,
    tag=factor(val),
    lemma=val,
    lttr=val,
    wclass=factor(val),
    desc=factor(val),
    stop=val,
    stem=val,
    idx=val,
    sntc=val
  ))
}

valid.tokens.kRp.tagged <- colnames(init.kRp.tagged.df())

#' S4 Class kRp.tagged
#'
#' This class is used for objects that are returned by \code{\link[koRpus:treetag]{treetag}} or \code{\link[koRpus:tokenize]{tokenize}}.
#'
#' @section Contructor function:
#' Should you need to manually generate objects of this class (which should rarely be the case), the contructor function 
#' \code{kRp_tagged(...)} can be used instead of
#' \code{new("kRp.tagged", ...)}.
#'
#' @slot lang A character string, naming the language that is assumed for the tokenized text in this object.
#' @slot desc Descriptive statistics of the tagged text.
#' @slot tokens Results of the called tokenizer and POS tagger. The data.frame usually has eleven columns:
#'    \describe{
#'      \item{\code{doc_id}:}{Factor, optional document identifier.}
#'      \item{\code{token}:}{Character, the tokenized text.}
#'      \item{\code{tag}:}{Factor, POS tags for each token.}
#'      \item{\code{lemma}:}{Character, lemma for each token.}
#'      \item{\code{lttr}:}{Integer, number of letters.}
#'      \item{\code{wclass}:}{Factor, word class.}
#'      \item{\code{desc}:}{Factor, a short description of the POS tag.}
#'      \item{\code{stop}:}{Logical, \code{TRUE} if token is a stopword.}
#'      \item{\code{stem}:}{Character, stemmed token.}
#'      \item{\code{idx}:}{Integer, index number of token in this document.}
#'      \item{\code{sntc}:}{Integer, number of sentence in this document.}
#'    }
#'    This data.frame structure adheres to the "Text Interchange Formats" guidelines set out by rOpenSci[1].
#' @note There is also \code{as()} methods to transform objects from other koRpus classes into kRp.tagged.
#' @name kRp.tagged,-class
#' @aliases kRp.tagged-class
#' @import methods
#' @references
#'    [1] Text Interchange Formats (\url{https://github.com/ropensci/tif})
#' @keywords classes
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @export kRp_tagged
#' @exportClass kRp.tagged
#' @rdname kRp.tagged-class

kRp_tagged <- setClass("kRp.tagged",
    representation=representation(
      lang="character",
      desc="list",
      tokens="data.frame"),
    prototype(
      lang=character(),
      desc=list(),
      tokens=init.kRp.tagged.df()
    )
)

setValidity("kRp.tagged", function(object){
  validate_df(
    df=slot(object, "tokens"),
    valid_cols=valid.tokens.kRp.tagged,
    strict=FALSE,
    warn_only=FALSE,
    name="tokens"
  )

  if(!is.character(slot(object, "lang"))){
    stop(simpleError("Invalid object: Slot \"lang\" must be of class character!"))
  } else {}

  return(TRUE)
})

## method validate_df()
# checks whether a given data frame provides all columns expected
#  - strict: if TRUE only allows columns defined by valid_cols
setGeneric(
  "validate_df",
  function(
    df,
    valid_cols=valid.tokens.kRp.tagged,
    strict=TRUE,
    warn_only=TRUE,
    name="tokens"
  ) standardGeneric("validate_df")
)
setMethod("validate_df",
  signature=signature(df="data.frame"),
  function(
    df,
    valid_cols=valid.tokens.kRp.tagged,
    strict=TRUE,
    warn_only=TRUE,
    name="tokens"
  ){
    df_cols <- colnames(df)
    result <- TRUE

    if(!identical(df_cols, valid_cols)){
      if(isTRUE(strict)){
        wrongCols <- df_cols[!df_cols %in% valid_cols]
        if(length(wrongCols) > 0){
          result <- FALSE
          wrongCols_msg <- paste0(
            "Invalid object: Wrong columns in data frame \"", name, "\":\n  ",
            paste0(wrongCols, collapse=", ")
          )
          ifelse(
            isTRUE(warn_only),
            warning(wrongCols_msg, call.=FALSE),
            stop(simpleError(wrongCols_msg))
          )
        } else {}
      } else {}
      missingCols <- valid_cols[!valid_cols %in% df_cols]
      if(length(missingCols) > 0){
        result <- FALSE
        missingCols_msg <- paste0(
          "Invalid object: Missing columns in data frame \"", name, "\":\n  ",
          paste0(missingCols, collapse=", ")
        )
        ifelse(
          isTRUE(warn_only),
          warning(missingCols_msg, call.=FALSE),
          stop(simpleError(missingCols_msg))
        )
      }
    } else {}
    return(result)
  }
)
