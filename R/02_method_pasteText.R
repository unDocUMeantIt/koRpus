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


#' Paste koRpus objects
#'
#' Paste the text in koRpus objects.
#'
#' This function takes objects of either class \code{kRp.tagged}, \code{kRp.txt.freq} or \code{kRp.analysis} and pastes only
#' the actual text as is.
#'
#' @param txt An object of class \code{\link[koRpus:kRp.txt.trans-class]{kRp.txt.trans}}, \code{\link[koRpus:kRp.tagged-class]{kRp.tagged}},
#'    \code{\link[koRpus:kRp.txt.freq-class]{kRp.txt.freq}} or \code{\link[koRpus:kRp.analysis-class]{kRp.analysis}}.
#' @param replace A named character vector to define replacements for \code{koRpus}' internal headline and paragraph tags.
#' @param ... Additional options, currently unused.
#' @return An atomic character vector.
#' @keywords misc
#' @export
#' @import methods
#' @docType methods
#' @rdname pasteText-methods
#' @examples
#' \dontrun{
#'   tagged.text.obj <- freq.analysis("/some/text.txt", corp.freq=my.LCC.data)
#'   pasteText(tagged.text.obj)
#' }
setGeneric("pasteText", function(txt, ...){standardGeneric("pasteText")})


#' @export
#' @docType methods
#' @rdname pasteText-methods
#' @aliases pasteText,kRp.taggedText-method
#' @include 01_class_80_kRp.taggedText_union.R
#' @include koRpus-internal.R
setMethod("pasteText",
  # "kRp.taggedText" is a ClassUnion defined in koRpus-internal.R
  signature(txt="kRp.taggedText"),
  function(txt, replace=c(hon.kRp="", hoff.kRp="\n\n", p.kRp="\n\n")){
    TT.res <- taggedText(txt)

    # we probably need to replace tags
    if("hon.kRp" %in% names(replace)){
      TT.res[TT.res[["tag"]] == "hon.kRp", "token"] <- replace["hon.kRp"]
    } else {}
    if("hoff.kRp" %in% names(replace)){
      TT.res[TT.res[["tag"]] == "hoff.kRp", "token"] <- replace["hoff.kRp"]
    } else {}
    if("p.kRp" %in% names(replace)){
      TT.res[TT.res[["tag"]] == "p.kRp", "token"] <- replace["p.kRp"]
    } else {}

    # put all text together
    results <- paste.tokenized.text(txt=TT.res[["token"]])

    return(results)
  }
)


#' @rdname koRpus-deprecated
#' @name koRpus-deprecated
#' @export
kRp.text.paste <- function(...){
  .Deprecated(new="pasteText")
  pasteText(...)
}
