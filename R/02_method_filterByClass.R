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


#' Remove word classes
#'
#' This method strips off defined word classes of tagged text objects.
#'
#' @param txt An object of class \code{\link[koRpus:kRp.text-class]{kRp.text}}.
#' @param corp.rm.class A character vector with word classes which should be removed. The default value
#'    \code{"nonpunct"} has special meaning and will cause the result of
#'    \code{kRp.POS.tags(lang, tags=c("punct","sentc"), list.classes=TRUE)} to be used.
#'    Another valid value is "stopword" to remove all detected stopwords.
#' @param corp.rm.tag A character vector with valid POS tags which should be removed.
#' @param as.vector Logical. If \code{TRUE}, results will be returned as a character vector containing only the text parts
#'    which survived the filtering.
#' @param update.desc Logical. If \code{TRUE}, the \code{desc} slot of the tagged object will be fully recalculated
#'    using the filtered text. If \code{FALSE}, the \code{desc} slot will be copied from the original object.
#'    Finally, if \code{NULL}, the \code{desc} slot remains empty.
#' @param ... Additional options, currently unused.
#' @return An object of the input class. If \code{as.vector=TRUE}, returns only a character vector.
#' @seealso \code{\link[koRpus:kRp.POS.tags]{kRp.POS.tags}}
#' @keywords misc
#' @import methods
#' @docType methods
#' @export
#' @rdname filterByClass-methods
#' @examples
#' \dontrun{
#'    filterByClass(tagged.text)
#' }
setGeneric("filterByClass", function(txt, ...){standardGeneric("filterByClass")})

#' @export
#' @docType methods
#' @rdname filterByClass-methods
#' @aliases filterByClass,kRp.text-method
#' @include 01_class_01_kRp.text.R
#' @include koRpus-internal.R
setMethod("filterByClass",
  signature(txt="kRp.text"),
  function(txt, corp.rm.class="nonpunct", corp.rm.tag=c(), as.vector=FALSE, update.desc=TRUE){
    txt.tokens <- taggedText(txt)
    txt.desc <- describe(txt)

    # set the language definition
    lang <- language.setting(txt, NULL)

    pre.results <- tagged.txt.rm.classes(txt.tokens, lemma=FALSE, lang=lang, corp.rm.class=corp.rm.class, corp.rm.tag=corp.rm.tag, as.vector=as.vector)

    if(isTRUE(as.vector)){
      results <- pre.results
    } else {
      rownames(pre.results) <- NULL
      results <- txt
      taggedText(results) <- pre.results
      if(!is.null(update.desc)){
        if(isTRUE(update.desc)){
          describe(results)[[doc_id(results)]] <- basic.tagged.descriptives(txt=results, txt.vector=pre.results[["token"]], doc_id=doc_id(results))
        } else {
          describe(results)[[doc_id(results)]] <- txt.desc
        }
      } else {}
    }

    return(results)
  }
)

#' @rdname koRpus-deprecated
#' @name koRpus-deprecated
#' @param ... Parameters to be passed to the replacement of the function
#' @export
kRp.filter.wclass <- function(...){
  .Deprecated(new="filterByClass")
  filterByClass(..., update.desc=NULL)
}
