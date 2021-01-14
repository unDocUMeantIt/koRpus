# Copyright 2010-2021 Meik Michalke <meik.michalke@hhu.de>
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
#' This function takes objects of class \code{kRp.text} and pastes only the actual text as is.
#'
#' @param txt An object of class \code{\link[koRpus:kRp.text-class]{kRp.text}}.
#' @param replace A named character vector to define replacements for \code{koRpus}' internal headline and paragraph tags.
#' @param ... Additional options, currently unused.
#' @return An atomic character vector.
#' @keywords misc
#' @export
#' @import methods
#' @docType methods
#' @rdname pasteText-methods
#' @example inst/examples/if_lang_en_clause_start.R
#' @example inst/examples/define_sample_file.R
#' @examples
#'   tokenized.obj <- tokenize(
#'     txt=sample_file,
#'     lang="en"
#'   )
#'   tokenized.obj <- jumbleWords(tokenized.obj)
#'   pasteText(tokenized.obj)
#' @example inst/examples/if_lang_en_clause_end.R
setGeneric("pasteText", function(txt, ...){standardGeneric("pasteText")})


#' @export
#' @docType methods
#' @rdname pasteText-methods
#' @aliases pasteText,kRp.text-method
#' @include 01_class_01_kRp.text.R
#' @include koRpus-internal.R
setMethod("pasteText",
  signature(txt="kRp.text"),
  function(txt, replace=c(hon.kRp="", hoff.kRp="\n\n", p.kRp="\n\n")){
    tokens <- taggedText(txt)

    # we probably need to replace tags
    if("hon.kRp" %in% names(replace)){
      tokens[tokens[["tag"]] == "hon.kRp", "token"] <- replace["hon.kRp"]
    } else {}
    if("hoff.kRp" %in% names(replace)){
      tokens[tokens[["tag"]] == "hoff.kRp", "token"] <- replace["hoff.kRp"]
    } else {}
    if("p.kRp" %in% names(replace)){
      tokens[tokens[["tag"]] == "p.kRp", "token"] <- replace["p.kRp"]
    } else {}

    # put all text together
    results <- paste.tokenized.text(txt=tokens[["token"]])

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
