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

#' Generate a document-term matrix
#'
#' Returns a sparse document-term matrix calculated from a given TIF[1] compliant token data frame
#' or object of class \code{\link[koRpus:kRp.tagged-class]{kRp.tagged}}. You can also
#' calculate the term frequency inverted document frequency value (tf-idf) for each term.
#' 
#' This is usually more interesting if done with more than one single text. If you're interested
#' in full corpus analysis, the \code{tm.plugin.koRpus} package should be worth checking out.
#' Alternatively, a data frame with multiple \code{doc_id} entries can be used.
#' 
#' See the examples to learn how to limit the analysis to desired word classes.
#' 
#' @param obj Either an object of class \code{\link[koRpus:kRp.tagged-class]{kRp.tagged}}, or a TIF[1] compliant token data frame.
#' @param terms A character string defining the \code{tokens} column to be used for calculating the matrix.
#' @param case.sens Logical, whether terms should be counted case sensitive.
#' @param tfidf Logical, if \code{TRUE} calculates term frequency--inverse document frequency (tf-idf)
#'    values instead of absolute frequency.
#' @param ... Additional arguments depending on the particular method.
#' @return A sparse matrix of class \code{\link[Matrix:dgCMatrix-class]{dgCMatrix}}.
#' @references
#'    [1] Text Interchange Formats (\url{https://github.com/ropensci/tif})
#' @importFrom Matrix Matrix
#' @export
#' @docType methods
#' @rdname docTermMatrix
#' @examples
#' \dontrun{
#' tokenized.obj <- tokenize(
#'   file.path(path.package("koRpus"), "tests", "testthat", "sample_text.txt")
#' )
#' 
#' # get the document-term frequencies in a sparse matrix
#' myDTMatrix <- docTermMatrix(tokenized.obj)
#' 
#' # combine with filterByClass() to, e.g.,  exclude all punctuation
#' myDTMatrix <- docTermMatrix(filterByClass(tokenized.obj))
#' 
#' # instead of absolute frequencies, get the tf-idf values
#' myDTMatrix <- docTermMatrix(
#'   filterByClass(tokenized.obj),
#'   tfidf=TRUE
#' )
#' }
setGeneric(
  "docTermMatrix",
  function(
    obj,
    terms="token",
    case.sens=FALSE,
    tfidf=FALSE,
    ...
  ) standardGeneric("docTermMatrix")
)

#' @rdname docTermMatrix
#' @docType methods
#' @export
#' @aliases
#'    docTermMatrix,-methods
#'    docTermMatrix,data.frame-method
setMethod("docTermMatrix",
  signature=signature(obj="data.frame"),
  function(
    obj,
    terms="token",
    case.sens=FALSE,
    tfidf=FALSE
  ){
    validate_df(
      df=obj,
      valid_cols=c("doc_id", terms),
      strict=FALSE,
      warn_only=FALSE,
      name="obj"
    )
    if(!is.character(obj[["doc_id"]])){
      warning("Converting \"doc_id\" into character, this might fail!")
      obj[["doc_id"]] <- as.character(obj[["doc_id"]])
    } else {}
    if(!isTRUE(case.sens)){
      obj[[terms]] <- tolower(obj[[terms]])
    } else {}
    uniqueTerms <- unique(obj[[terms]])
    doc_ids <- unique(as.character(obj[["doc_id"]]))

    dt_mtx <- matrix(
      0,
      nrow=length(doc_ids),
      ncol=length(uniqueTerms),
      dimnames=list(doc_ids, uniqueTerms)
    )
    if(isTRUE(tfidf)){
      tf_mtx <- dt_mtx
    } else {}

    for (thisDoc in doc_ids){
      relevantTerms <- obj[obj[["doc_id"]] %in% thisDoc, terms]
      termsInDoc <- table(relevantTerms)
      if(isTRUE(tfidf)){
        tf_mtx[rownames(tf_mtx) %in% thisDoc, colnames(tf_mtx) %in% names(termsInDoc)] <- termsInDoc/length(relevantTerms)
      } else {}
      dt_mtx[rownames(dt_mtx) %in% thisDoc, colnames(dt_mtx) %in% names(termsInDoc)] <- termsInDoc
    }

    if(isTRUE(tfidf)){
      idf <- log(nrow(dt_mtx)/colSums(dt_mtx > 0))
      result <- Matrix(t(t(tf_mtx) * idf), sparse=TRUE)
    } else {
      result <- Matrix(dt_mtx, sparse=TRUE)
    }

    return(result)
  }
)

#' @rdname docTermMatrix
#' @docType methods
#' @export
#' @aliases
#'    docTermMatrix,-methods
#'    docTermMatrix,kRp.taggedText-method
#' @include 01_class_80_kRp.taggedText_union.R
#' @include koRpus-internal.R
setMethod("docTermMatrix",
  signature=signature(obj="kRp.taggedText"),
  function(
    obj,
    terms="token",
    case.sens=FALSE,
    tfidf=FALSE
  ){
    docTermMatrix(
      obj=tif_as_tokens_df(obj),
      terms=terms,
      case.sens=case.sens,
      tfidf=tfidf
    )
  }
)
