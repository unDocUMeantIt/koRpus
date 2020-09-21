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

init.kRp.text.df <- function(rows=1){
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

valid.tokens.kRp.text <- colnames(init.kRp.text.df())

#' S4 Class kRp.text
#'
#' This class is used for objects that are returned by \code{\link[koRpus:treetag]{treetag}} or \code{\link[koRpus:tokenize]{tokenize}}.
#'
#' @section Contructor function:
#' Should you need to manually generate objects of this class (which should rarely be the case), the contructor function 
#' \code{kRp_text(...)} can be used instead of
#' \code{new("kRp.text", ...)}.
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
#' @slot features A named logical vector, indicating which features are available in this object's \code{feat_list} slot.
#'    Common features are listed in the description of the \code{feat_list} slot.
#' @slot feat_list A named list with optional analysis results or other content as used by the defined \code{features}:
#'    \itemize{
#'      \item{\code{hyphen} }{A named list of objects of class \code{\link[sylly:kRp.hyphen-class]{kRp.hyphen}}.}
#'      \item{\code{readability} }{A named list of objects of class \code{\link[koRpus:kRp.readability-class]{kRp.readability}}.}
#'      \item{\code{lex_div} }{A named list of objects of class \code{\link[koRpus:kRp.TTR-class]{kRp.TTR}}.}
#'      \item{\code{freq} }{A list with additional results of \code{\link[koRpus:freq.analysis]{freq.analysis}}.}
#'      \item{\code{corp_freq} }{An object of class \code{\link[koRpus:kRp.corp.freq-class]{kRp.corp.freq}}, e.g., results of a call to
#'        \code{\link[koRpus:read.corp.custom]{read.corp.custom}}.}
#'      \item{\code{diff} }{Additional results of calls to a method like \code{\link[koRpus:textTransform]{textTransform}}.}
#      \item{\code{summary} }{A summary data frame for the full corpus, including descriptive statistics on all texts, as well as
#        results of analyses like readability and lexical diversity, if available.}
#'      \item{\code{doc_term_matrix} }{A sparse document-term matrix, as produced by \code{\link[koRpus:docTermMatrix]{docTermMatrix}}.}
#      \item{\code{stopwords} }{A numeric vector with the total number of stopwords in each text, if stopwords were analyzed during tokenizing or POS tagging.}
#      \item{\code{} }{}
#'    }
#'    See the \code{\link[koRpus:kRp.text_get-methods]{getter and setter methods}} for easy access to these sub-slots.
#'    There can actually be any number of additional features, the above is just a list of those already defined by this package.
#' @note There is also \code{as()} methods to transform objects from other koRpus classes into kRp.text.
#' @name kRp.text,-class
#' @aliases kRp.text-class
#' @import methods
#' @references
#'    [1] Text Interchange Formats (\url{https://github.com/ropensci/tif})
#' @keywords classes
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @export kRp_text
#' @exportClass kRp.text
#' @rdname kRp.text-class

kRp_text <- setClass("kRp.text",
    representation=representation(
      lang="character",
      desc="list",
      tokens="data.frame",
      features="logical",
      feat_list="list"
    ),
    prototype(
      lang=character(),
      desc=list(),
      tokens=init.kRp.text.df(),
      features=logical(),
      feat_list=list()
    )
)

setValidity("kRp.text", function(object){
  features <- slot(object, "features")
  feat_list <- slot(object, "feat_list")
  hyphen <- feat_list[["hyphen"]]
  readability <- feat_list[["readability"]]
  lex_div <- feat_list[["lex_div"]]
  freq <- feat_list[["freq"]]
  corp_freq <- feat_list[["corp_freq"]]
  # obj_summary <- feat_list[["summary"]]

  announced_features <- names(features[features])
  missingFeatures <- sapply(feat_list[announced_features], is.null)
  if(any(missingFeatures)){
    warning(
      paste0(
        "Invalid object: There's no data for activated \"features\":\n  ",
        paste0(announced_features[missingFeatures], collapse=", ")
      ),
      call.=FALSE
    )
  } else {}

  is_valid <- validate_df(
    df=slot(object, "tokens"),
    valid_cols=valid.tokens.kRp.text,
    strict=FALSE,
    warn_only=TRUE,
    name="tokens"
  )

  if(!isTRUE(is_valid)){
    warning("Try fixObject() to fix the issues.", call.=FALSE)
  } else {}

  if(!is.character(slot(object, "lang"))){
    stop(simpleError("Invalid object: Slot \"lang\" must be of class character!"))
  } else {}

  classObj <- list(
    "kRp.hyphen"=list(name="hyphen", obj=hyphen),
    "kRp.readability"=list(name="readability", obj=readability),
    "kRp.TTR"=list(name="lex_div", obj=lex_div),
    "kRp.corp.freq"=list(name="corp_freq", obj=corp_freq)#,
    #"data.frame"=list(name="summary", obj=obj_summary)
  )
  for (thisClassObj in names(classObj)) {
    if(
      all(
        !identical(classObj[[thisClassObj]][["obj"]], list()),
        !is.null(classObj[[thisClassObj]][["obj"]]),
        !all(sapply(classObj[[thisClassObj]][["obj"]], function(x) inherits(x, thisClassObj)))
      )
    ){
      stop(simpleError(paste0("Invalid object: Slot \"", classObj[[thisClassObj]][["name"]], "\" must have entries inheriting from class ", thisClassObj, "!")))
    } else {}
  }

  return(TRUE)
})

## method validate_df()
# checks whether a given data frame provides all columns expected
#  - strict: if TRUE only allows columns defined by valid_cols
setGeneric(
  "validate_df",
  function(
    df,
    valid_cols=valid.tokens.kRp.text,
    strict=TRUE,
    warn_only=TRUE,
    name="tokens"
  ) standardGeneric("validate_df")
)
setMethod("validate_df",
  signature=signature(df="data.frame"),
  function(
    df,
    valid_cols=valid.tokens.kRp.text,
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
