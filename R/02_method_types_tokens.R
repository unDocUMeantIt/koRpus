# Copyright 2016-2019 Meik Michalke <meik.michalke@hhu.de>
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


#' Get types and tokens of a given text
#' 
#' These methods return character vectors that return all types or tokens of a given text, where text can either be a character
#' vector itself, a previosly tokenized/tagged koRpus object, or an object of class \code{kRp.TTR}.
#' 
#' @note If the input is of class \code{kRp.TTR}, the result will only be useful if \code{lex.div} or
#' the respective wrapper function was called with \code{keep.tokens=TRUE}. Similarily, \code{lemmatize} can only work
#' properly if the input is a tagged text object with lemmata or you've properly set up the enviroment via \code{set.kRp.env}.
#' Calling these methods on \code{kRp.TTR} objects is just returning the respective part of its \code{tt} slot.
#'
#' @param txt An object of either class \code{\link[koRpus:kRp.text-class]{kRp.text}} or
#'    \code{\link[koRpus:kRp.TTR-class]{kRp.TTR}}, or a character vector.
#' @param case.sens Logical, whether types should be counted case sensitive.
#'    This option is available for tagged text and character input only.
#' @param lemmatize Logical, whether analysis should be carried out on the lemmatized tokens rather than all running word forms.
#'    This option is available for tagged text and character input only.
#' @param corp.rm.class A character vector with word classes which should be dropped. The default value
#'    \code{"nonpunct"} has special meaning and will cause the result of
#'    \code{kRp.POS.tags(lang, tags=c("punct","sentc"), list.classes=TRUE)} to be used.
#'    This option is available for tagged text and character input only.
#' @param corp.rm.tag A character vector with POS tags which should be dropped.
#'    This option is available for tagged text and character input only.
#' @param stats Logical, whether statistics on the length in characters and frequency of types in the text should also be returned.
#' @param lang Set the language of a text, see the \code{force.lang} option of \code{\link[koRpus:lex.div]{lex.div}}.
#'    This option is available for character input only.
#' @return A character vector. For\code{types} and \code{stats=TRUE} a data.frame containing all types, their length (characters)
#'    and frequency. The \code{types} result is always sorted by frequency, with more frequent types coming first.
#' @keywords LD
#' @seealso \code{\link[koRpus:kRp.POS.tags]{kRp.POS.tags}},
#'    \code{\link[koRpus:kRp.text-class]{kRp.text}},
#'    \code{\link[koRpus:kRp.TTR-class]{kRp.TTR}},
#'    \code{\link[koRpus:lex.div]{lex.div}}
#' @import methods
#' @rdname types.tokens-methods
#' @export
#' @examples
#' \dontrun{
#' types(tagged.text)
#' tokens(tagged.text)
#' }

#' @param ... Only used for the method generic.
setGeneric("types", function(txt, ...) standardGeneric("types"))
#' @export
#' @rdname types.tokens-methods
setGeneric("tokens", function(txt, ...) standardGeneric("tokens"))

#' @export
#' @include 01_class_02_kRp.TTR.R
#' @aliases types types,kRp.TTR-method
#' @rdname types.tokens-methods
setMethod("types", signature(txt="kRp.TTR"), function(txt, stats=FALSE){
    if(isTRUE(stats)){
      result <- slot(txt, "tt")[["types"]]
    } else {
      result <- slot(txt, "tt")[["types"]][["type"]]
    }
    return(result)
  }
)
#' @export
#' @include 01_class_02_kRp.TTR.R
#' @aliases tokens tokens,kRp.TTR-method
#' @rdname types.tokens-methods
setMethod("tokens", signature(txt="kRp.TTR"), function(txt){
    return(slot(txt, "tt")[["tokens"]])
  }
)


#' @export
#' @include 01_class_01_kRp.text.R
#' @include koRpus-internal.R
#' @aliases types,kRp.text-method
#' @rdname types.tokens-methods
setMethod("types", signature(txt="kRp.text"), function(txt,
    case.sens=FALSE, lemmatize=FALSE, corp.rm.class="nonpunct", corp.rm.tag=c(), stats=FALSE){
    basicTnT <- TnT(
      txt=txt,
      corp.rm.class=corp.rm.class,
      corp.rm.tag=corp.rm.tag,
      case.sens=case.sens,
      lemmatize=lemmatize,
      keep.tokens=TRUE,
      quiet=TRUE
    )
    if(isTRUE(lemmatize)){
      results <- basicTnT[["txt.lemma.freq"]]
    } else {
      results <- basicTnT[["txt.type.freq"]]
    }
    if(!isTRUE(stats)){
      results <- results[["type"]]
    } else {}
    return(results)
  }
)
#' @export
#' @include 01_class_01_kRp.text.R
#' @include koRpus-internal.R
#' @aliases tokens,kRp.text-method
#' @rdname types.tokens-methods
setMethod("tokens", signature(txt="kRp.text"), function(txt,
    case.sens=FALSE, lemmatize=FALSE, corp.rm.class="nonpunct", corp.rm.tag=c()){
    basicTnT <- TnT(
      txt=txt,
      corp.rm.class=corp.rm.class,
      corp.rm.tag=corp.rm.tag,
      case.sens=case.sens,
      lemmatize=lemmatize,
      keep.tokens=TRUE,
      quiet=TRUE
    )
    return(basicTnT[["txt.all.tokens"]])
  }
)


#' @export
#' @aliases types,character-method
#' @rdname types.tokens-methods
setMethod("types", signature(txt="character"), function(txt,
  case.sens=FALSE, lemmatize=FALSE, corp.rm.class="nonpunct", corp.rm.tag=c(), stats=FALSE, lang=NULL){
    basicTnT <- TnT(
      txt=txt,
      force.lang=lang,
      corp.rm.class=corp.rm.class,
      corp.rm.tag=corp.rm.tag,
      case.sens=case.sens,
      lemmatize=lemmatize,
      keep.tokens=TRUE,
      quiet=TRUE
    )
    if(isTRUE(lemmatize)){
      results <- basicTnT[["txt.lemma.freq"]]
    } else {
      results <- basicTnT[["txt.type.freq"]]
    }
    if(!isTRUE(stats)){
      results <- results[["type"]]
    } else {}
    return(results)
  }
)
#' @export
#' @aliases tokens,character-method
#' @rdname types.tokens-methods
setMethod("tokens", signature(txt="character"), function(txt,
  case.sens=FALSE, lemmatize=FALSE, corp.rm.class="nonpunct", corp.rm.tag=c(), lang=NULL){
    basicTnT <- TnT(
      txt=txt,
      force.lang=lang,
      corp.rm.class=corp.rm.class,
      corp.rm.tag=corp.rm.tag,
      case.sens=case.sens,
      lemmatize=lemmatize,
      keep.tokens=TRUE,
      quiet=TRUE
    )
    return(basicTnT[["txt.all.tokens"]])
  }
)
