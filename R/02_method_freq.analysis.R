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
#' The easiest way to see what kinds of analyses are done is probably to look at the slot description of \code{\link[koRpus:kRp.txt.freq-class]{kRp.txt.freq}}.
#'
#' @param txt.file An object of class \code{\link[koRpus:kRp.tagged-class]{kRp.tagged}}.
#' @param corp.freq An object of class \code{\link[koRpus:kRp.corp.freq-class]{kRp.corp.freq}}.
#' @param desc.stat Logical, whether a descriptive statistical analysis should be performed.
#' @param corp.rm.class A character vector with word classes which should be ignored for frequency analysis. The default value
#'    \code{"nonpunct"} has special meaning and will cause the result of
#'    \code{kRp.POS.tags(lang, tags=c("punct","sentc"), list.classes=TRUE)} to be used.
#' @param corp.rm.tag A character vector with POS tags which should be ignored for frequency analysis.
#' @param tfidf Logical, whether the term frequency--inverse document frequency statistic (tf-idf) should be computed. Requires
#'    \code{corp.freq} to provide appropriate idf values for the types in \code{txt.file}. Missing idf values will result in \code{NA}.
#' @param as.feature Logical, whether the output should be just the analysis results or the input object with
#'    the results added as a feature. Use \code{\link[koRpus:corpusFreq]{corpusFreq}}
#'    to get the results from such an aggregated object.
#' @param ... Additional options to be passed through to the function defined with \code{tagger}.
#' @return Depending on \code{as.feature}, either an object of class \code{\link[koRpus:kRp.txt.freq-class]{kRp.txt.freq}},
#'    or an object of class \code{\link[koRpus:kRp.tagged-class]{kRp.tagged}} with the added feature \code{freq} containing it.
#' @keywords misc
#' @seealso \code{\link[koRpus:get.kRp.env]{get.kRp.env}}, \code{\link[koRpus:kRp.tagged-class]{kRp.tagged}},
#'    \code{\link[koRpus:kRp.corp.freq-class]{kRp.corp.freq}}
#' @import methods
#' @export
#' @rdname freq.analysis-methods
#' @examples
#' \dontrun{
#' freq.analysis(tagged.text, corp.freq=my.LCC.data)
#' }

########################################################################
## if this signature changes, check kRp.freq.analysis.calc() as well! ##
########################################################################

setGeneric("freq.analysis", function(txt.file, ...) standardGeneric("freq.analysis"))

#' @export
#' @include 01_class_01_kRp.tagged.R
#' @include 01_class_03_kRp.txt.freq.R
#' @include 01_class_05_kRp.analysis.R
#' @include 01_class_80_kRp.taggedText_union.R
#' @include koRpus-internal.R
#' @aliases freq.analysis,kRp.taggedText-method
#' @rdname freq.analysis-methods
setMethod(
  "freq.analysis",
  signature(txt.file="kRp.taggedText"),
  function(
    txt.file,
    corp.freq=NULL,
    desc.stat=TRUE,
    corp.rm.class="nonpunct",
    corp.rm.tag=c(),
    tfidf=TRUE,
    as.feature=FALSE
  ){
    results <- kRp.freq.analysis.calc(
      txt.file=txt.file,
      corp.freq=corp.freq,
      desc.stat=desc.stat,
      corp.rm.class=corp.rm.class,
      corp.rm.tag=corp.rm.tag,
      tfidf=tfidf
    )

    if(isTRUE(as.feature)){
      taggedText(txt.file) <- taggedText(results)
      corpusFreq(txt.file) <- slot(results, "freq.analysis")
      describe(txt.file) <- describe(results)
      return(txt.file)
    } else {
      return(results)
    }
  }
)
