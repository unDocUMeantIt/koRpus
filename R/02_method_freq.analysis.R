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


#' Analyze word frequencies
#'
#' The function \code{freq.analysis} analyzes texts regarding frequencies of tokens, word classes etc.
#'
#' It adds new columns with frequency information to the \code{tokens} data frame of the input data,
#' describing how often the particular token is used in the additionally provided corpus frequency object.
#'
#' To get the results, you can use \code{taggedText} to get the \code{tokens} slot, \code{describe} to get
#' the raw descriptive statistics (only updated if \code{desc.stat=TRUE}), and \code{corpusFreq} to get
#' the data from the added \code{freq} feature.
#' 
#' If \code{corp.freq} provides appropriate idf values for the types in \code{txt.file}, the
#' term frequency--inverse document frequency statistic (tf-idf) will also be computed.
#' Missing idf values will result in \code{NA}.
#'
#' @param txt.file An object of class \code{\link[koRpus:kRp.text-class]{kRp.text}}.
#' @param corp.freq An object of class \code{\link[koRpus:kRp.corp.freq-class]{kRp.corp.freq}}.
#' @param desc.stat Logical, whether an updated descriptive statistical analysis should be conducted.
#' @param corp.rm.class A character vector with word classes which should be ignored for frequency analysis. The default value
#'    \code{"nonpunct"} has special meaning and will cause the result of
#'    \code{kRp.POS.tags(lang, tags=c("punct","sentc"), list.classes=TRUE)} to be used.
#' @param corp.rm.tag A character vector with POS tags which should be ignored for frequency analysis.
#' @param ... Additional options for the generic.
#' @return An updated object of class \code{\link[koRpus:kRp.text-class]{kRp.text}} with the added feature \code{freq},
#'    which is a list with information on the word frequencies of the analyzed text.
#'    Use \code{\link[koRpus:corpusFreq]{corpusFreq}} to get that slot.
#' @keywords misc
#' @seealso \code{\link[koRpus:get.kRp.env]{get.kRp.env}}, \code{\link[koRpus:kRp.text-class]{kRp.text}},
#'    \code{\link[koRpus:kRp.corp.freq-class]{kRp.corp.freq}}
#' @import methods
#' @export
#' @rdname freq.analysis-methods
#' @example inst/examples/if_lang_en_clause_start.R
#' @example inst/examples/define_sample_file.R
#' @examples
#'   # call freq.analysis() on a tokenized text
#'   tokenized.obj <- tokenize(
#'     txt=sample_file,
#'     lang="en"
#'   )
#'   # the token slot before frequency analysis
#'   head(taggedText(tokenized.obj))
#'
#'   # instead of data from a larger corpus, we'll
#'   # use the token frequencies of the text itself
#'   tokenized.obj <- freq.analysis(
#'     tokenized.obj,
#'     corp.freq=read.corp.custom(tokenized.obj)
#'   )
#'   # compare the columns after the anylsis
#'   head(taggedText(tokenized.obj))
#'
#'   # the object now has further statistics in a
#'   # new feature slot called freq
#'   hasFeature(tokenized.obj)
#'   corpusFreq(tokenized.obj)
#' @example inst/examples/if_lang_en_clause_end.R

########################################################################
## if this signature changes, check kRp.freq.analysis.calc() as well! ##
########################################################################

setGeneric("freq.analysis", function(txt.file, ...) standardGeneric("freq.analysis"))

#' @export
#' @include 01_class_01_kRp.text.R
#' @include koRpus-internal.R
#' @aliases freq.analysis,kRp.text-method
#' @rdname freq.analysis-methods
setMethod(
  "freq.analysis",
  signature(txt.file="kRp.text"),
  function(
    txt.file,
    corp.freq=NULL,
    desc.stat=TRUE,
    corp.rm.class="nonpunct",
    corp.rm.tag=c()
  ){
    results <- kRp.freq.analysis.calc(
      txt.file=txt.file,
      corp.freq=corp.freq,
      desc.stat=desc.stat,
      corp.rm.class=corp.rm.class,
      corp.rm.tag=corp.rm.tag
    )

    return(results)
  }
)
