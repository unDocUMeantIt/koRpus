# Copyright 2019 Meik Michalke <meik.michalke@hhu.de>
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

deprecated.init.kRp.tagged.df <- function(rows=1){
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

valid.TT.res.kRp.tagged <- colnames(deprecated.init.kRp.tagged.df())

#' Deprecated object classes
#'
#' These classes are no longer used by the \code{koRpus} package and will be removed in a later version.
#' They are kept here for the time being so you can still load old objects and convert them into new objects using the
#' \code{fixObject} method.
#' 
#' @section S4 Class \code{kRp.tagged}:
#' This was used for objects returned by \code{\link[koRpus:treetag]{treetag}} or \code{\link[koRpus:tokenize]{tokenize}}.
#' It was replaced by \code{\link[koRpus:kRp.text-class]{kRp.text}}.
#'
#' @section S4 Class \code{kRp.txt.freq}:
#' This was used for objects returned by \code{\link[koRpus:freq.analysis]{freq.analysis}}.
#' It was replaced by \code{\link[koRpus:kRp.text-class]{kRp.text}}.
#'
#' @section S4 Class \code{kRp.txt.trans}:
#' This was used for objects returned by \code{\link[koRpus:textTransform]{textTransform}}, \code{\link[koRpus:clozeDelete]{clozeDelete}},
#' \code{\link[koRpus:cTest]{cTest}}, and \code{\link[koRpus:jumbleWords]{jumbleWords}}.
#' It was replaced by \code{\link[koRpus:kRp.text-class]{kRp.text}}.
#'
#' @section S4 Class \code{kRp.analysis}:
#' This was used for objects returned by \code{\link[koRpus:kRp.text.analysis]{kRp.text.analysis}}.
#' The function is also deprecated, functionality can be replicated by combining \code{treetag},\code{freq.analysis} and \code{lex.div}.
#'
#' @slot lang A character string, naming the language that is assumed for the tokenized text in this object.
#' @slot desc Descriptive statistics of the tagged text.
#' @slot TT.res Results of the called tokenizer and POS tagger. The data.frame usually has eleven columns:
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
#' @import methods
#' @references
#'    [1] Text Interchange Formats (\url{https://github.com/ropensci/tif})
#' @keywords classes
#' @export kRp_tagged
#' @exportClass kRp.tagged
#' @rdname koRpus-deprecated
#' @name koRpus-deprecated
#' @aliases kRp.tagged-class

kRp_tagged <- setClass("kRp.tagged",
    representation=representation(
      lang="character",
      desc="list",
      TT.res="data.frame"),
    prototype(
      lang=character(),
      desc=list(),
      TT.res=deprecated.init.kRp.tagged.df()
    )
)

setValidity("kRp.tagged", function(object){
  validate_df(
    df=slot(object, "TT.res"),
    valid_cols=valid.TT.res.kRp.tagged,
    strict=FALSE,
    warn_only=FALSE,
    name="TT.res"
  )

  if(!is.character(slot(object, "lang"))){
    stop(simpleError("Invalid object: Slot \"lang\" must be of class character!"))
  } else {}

  return(TRUE)
})

#' @rdname kRp.text_get-methods
#' @export
#' @docType methods
#' @aliases
#'    fixObject,-methods
#'    fixObject,kRp.tagged-method
#' @include 02_method_get_set_kRp.text.R
setMethod("fixObject",
  signature=signature(obj="kRp.tagged"),
  function (obj, doc_id=NA){
    obj <- fixObject(
      kRp_text(
        lang=language(obj),
        desc=describe(obj),
        tokens=taggedText(obj)
      ),
      doc_id=doc_id
    )
    return(obj)
  }
)

#' @slot freq.analysis A list with information on the word frequencies of the analyzed text.
#' @import methods
#' @keywords classes
#' @export kRp_txt_freq
#' @exportClass kRp.txt.freq
#' @rdname koRpus-deprecated
#' @name koRpus-deprecated
#' @aliases kRp.txt.freq-class
kRp_txt_freq <- setClass("kRp.txt.freq",
    representation=representation(
    freq.analysis="list"),
  prototype=prototype(
    lang=character(),
    TT.res=deprecated.init.kRp.tagged.df(),
    desc=list(),
    freq.analysis=list()),
  contains=c("kRp.tagged")
)

#' @rdname kRp.text_get-methods
#' @export
#' @docType methods
#' @aliases
#'    fixObject,-methods
#'    fixObject,kRp.txt.freq-method
#' @include 02_method_get_set_kRp.text.R
setMethod("fixObject",
  signature=signature(obj="kRp.txt.freq"),
  function (obj, doc_id=NA){
    new_obj <- fixObject(
      kRp_text(
        lang=language(obj),
        desc=describe(obj),
        tokens=taggedText(obj)
      ),
      doc_id=doc_id
    )
    corpusFreq(new_obj) <- slot(obj, "freq.analysis")
    return(obj)
  }
)

#' @slot diff A list with mostly atomic vectors, describing the amount of diffences between both text variants (percentage):
#'    \describe{
#'      \item{\code{all.tokens}:}{Percentage of all tokens, including punctuation, that were altered.}
#'      \item{\code{words}:}{Percentage of altered words only.}
#'      \item{\code{all.chars}:}{Percentage of all characters, including punctuation, that were altered.}
#'      \item{\code{letters}:}{Percentage of altered letters in words only.}
#'      \item{\code{transfmt}:}{Character vector documenting the transformation(s) done to the tokens.}
#'      \item{\code{transfmt.equal}:}{Data frame documenting which token was changed in which transformational step. Only available if more than one transformation was done.}
#'      \item{\code{transfmt.normalize}:}{A list documenting steps of normalization that were done to the object, one element per transformation.
#'        Each entry holds the name of the method, the query parameters, and the effective replacement value.}
#'    }
#' @import methods
#' @keywords classes
#' @export kRp_txt_trans
#' @exportClass kRp.txt.trans
#' @rdname koRpus-deprecated
#' @name koRpus-deprecated
#' @aliases kRp.txt.trans-class
kRp_txt_trans <- setClass("kRp.txt.trans",
    representation=representation(
    diff="list"),
  prototype=prototype(
    lang=character(),
    desc=list(),
    TT.res=cbind(deprecated.init.kRp.tagged.df(), data.frame(token.orig=NA, equal=NA)),
    diff=list()),
  contains=c("kRp.tagged")
)

setValidity("kRp.txt.trans", function(object){
  validate_df(
    df=slot(object, "TT.res"),
    valid_cols=c(valid.tokens.kRp.tagged, "token.orig", "equal"),
    strict=FALSE,
    warn_only=FALSE,
    name="tokens"
  )
})

#' @rdname kRp.text_get-methods
#' @export
#' @docType methods
#' @aliases
#'    fixObject,-methods
#'    fixObject,kRp.txt.trans-method
#' @include 02_method_get_set_kRp.text.R
setMethod("fixObject",
  signature=signature(obj="kRp.txt.trans"),
  function (obj, doc_id=NA){
    new_obj <- fixObject(
      kRp_text(
        lang=language(obj),
        desc=describe(obj),
        tokens=taggedText(obj)
      ),
      doc_id=doc_id
    )
    diffText(new_obj) <- slot(obj, "diff")
    return(obj)
  }
)

#' @slot lex.div Information on lexical diversity
#' @import methods
#' @keywords classes
#' @include 01_class_02_kRp.TTR.R
#' @export kRp_analysis
#' @exportClass kRp.analysis
#' @rdname koRpus-deprecated
#' @name koRpus-deprecated
#' @aliases kRp.analysis-class
kRp_analysis <- setClass("kRp.analysis",
    representation=representation(
    lex.div="kRp.TTR",
    freq.analysis="list"),
  prototype=prototype(
    lang=character(),
    TT.res=deprecated.init.kRp.tagged.df(),
    desc=list(),
    lex.div=kRp_TTR(),
    freq.analysis=list()),
  contains=c("kRp.tagged")
)

#' @rdname kRp.text_get-methods
#' @export
#' @docType methods
#' @aliases
#'    fixObject,-methods
#'    fixObject,kRp.analysis-method
#' @include 02_method_get_set_kRp.text.R
setMethod("fixObject",
  signature=signature(obj="kRp.analysis"),
  function (obj, doc_id=NA){
    new_obj <- fixObject(
      kRp_text(
        lang=language(obj),
        desc=describe(obj),
        tokens=taggedText(obj)
      ),
      doc_id=doc_id
    )
    corpusFreq(new_obj) <- slot(obj, "freq.analysis")
    corpusLexDiv(new_obj) <- slot(obj, "lex.div")
    return(obj)
  }
)
