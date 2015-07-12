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


#' Import custom corpus data
#'
#' Read data from a custom corpus into a valid object of class \code{\link[koRpus]{kRp.corp.freq-class}}.
#'
#' The methods should enable you to perform a basic text corpus frequency analysis. That is, not just to
#' import analysis results like LCC files, but to import the corpus material itself. The resulting object
#' is of class \code{\link[koRpus]{kRp.corp.freq-class}}, so it can be used for frequency analysis by
#' other functions and methods of this package.
#'
#' @param corpus Either the path to directory with txt files to read and analyze, or a vector object already holding the text corpus.
#'    Can also be an already tokenized and tagged text object which inherits class \code{kRp.tagged} (then the column \code{"token"} of
#'    the \code{"TT.res"} slot is used).
#' @param format Either "file" or "obj", depending on whether you want to scan files or analyze the given object.
#' @param quiet Logical. If \code{FALSE}, short status messages will be shown.
#' @param caseSens Logical. If \code{FALSE}, all tokens will be matched in their lower case form.
#' @param log.base A numeric value defining the base of the logarithm used for inverse document frequency (idf). See
#'    \code{\link[base:log]{log}} for details.
#' @param ... Additional options to be passed through to the \code{tokenize} function.
#' @return An object of class \code{\link[koRpus]{kRp.corp.freq-class}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords corpora
#' @seealso \code{\link[koRpus]{kRp.corp.freq-class}}
#' @import methods
#' @rdname read.corp.custom-methods
#' @export
#' @examples
#' \dontrun{
#' ru.corp <- read.corp.custom("~/mydata/corpora/russian_corpus/")
#' }

###########################################################################
## if this signature changes, check kRp.read.corp.custom.calc() as well! ##
###########################################################################

setGeneric("read.corp.custom", function(corpus, ...) standardGeneric("read.corp.custom"))

#' @export
#' @include 00_class_01_kRp.tagged.R
#' @include 00_class_03_kRp.txt.freq.R
#' @include 00_class_04_kRp.txt.trans.R
#' @include 00_class_05_kRp.analysis.R
#' @include koRpus-internal.R
#' @aliases read.corp.custom,kRp.taggedText-method
#' @rdname read.corp.custom-methods
setMethod("read.corp.custom", signature(corpus="kRp.taggedText"), function(corpus,
    quiet=TRUE, caseSens=TRUE, log.base=10, ...){

    results <- kRp.read.corp.custom.calc(corpus=corpus, format="obj",
      quiet=quiet, caseSens=caseSens, log.base=log.base, ...)

    return(results)
  }
)

#' @export
#' @aliases read.corp.custom,character-method
#' @param tagger A character string pointing to the tokenizer/tagger command you want to use for basic text analysis. Can be omitted if
#'    \code{txt.file} is already of class \code{kRp.tagged-class}. Defaults to \code{tagger="kRp.env"} to get the settings by
#'    \code{\link[koRpus:get.kRp.env]{get.kRp.env}}. Set to \code{"tokenize"} to use \code{\link[koRpus:tokenize]{tokenize}}.
#' @param force.lang A character string defining the language to be assumed for the text(s), by force.
#' @rdname read.corp.custom-methods
setMethod("read.corp.custom", signature(corpus="character"), function(corpus,
    format="file", quiet=TRUE, caseSens=TRUE, log.base=10, tagger="kRp.env",
    force.lang=NULL, ...){

    results <- kRp.read.corp.custom.calc(corpus=corpus, format=format, quiet=quiet, caseSens=caseSens,
      log.base=log.base, tagger=tagger, force.lang=force.lang, ...)

    return(results)
  }
)

#' @export
#' @aliases read.corp.custom,list-method
#' @rdname read.corp.custom-methods
setMethod("read.corp.custom", signature(corpus="list"), function(corpus,
    quiet=TRUE, caseSens=TRUE, log.base=10, ...){

    if(!all(sapply(corpus, is.taggedText))){
      stop(simpleError("If you provide a list to read.corp.custom(), its elements must all inherit from kRp.taggedText!"))
    } else {}

    results <- kRp.read.corp.custom.calc(corpus=corpus, format="obj",
      quiet=quiet, caseSens=caseSens, log.base=log.base, ...)

    return(results)
  }
)
