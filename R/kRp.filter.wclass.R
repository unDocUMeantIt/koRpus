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


#' Remove word classes
#'
#' This function strips off defined word classes of tagged text objects.
#'
#' @param txt An object of class \code{\link[koRpus]{kRp.tagged-class}}.
#' @param corp.rm.class A character vector with word classes which should be removed. The default value
#'    \code{"nonpunct"} has special meaning and will cause the result of
#'    \code{kRp.POS.tags(lang, c("punct","sentc"), list.classes=TRUE)} to be used.
#'    Another valid value is "stopword" to remove all detected stopwords.
#' @param corp.rm.tag A character vector with valid POS tags which should be removed.
#' @param as.vector Logical. If \code{TRUE}, results will be returned as a character vector containing only the text parts
#'    which survived the filtering.
#' @return An object of class \code{\link[koRpus]{kRp.tagged-class}}. If \code{as.vector=TRUE}, returns only a character vector.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @seealso \code{\link[koRpus:kRp.POS.tags]{kRp.POS.tags}}
#' @keywords misc
#' @export
#' @examples
#' \dontrun{
#'    kRp.filter.wclass(tagged.text)
#' }

kRp.filter.wclass <- function(txt, corp.rm.class="nonpunct", corp.rm.tag=c(), as.vector=FALSE){

  # the internal function tag.kRp.txt() will return the object unchanged if it
  # is already tagged, so it's safe to call it with the lang set here
  tagged.text <- tag.kRp.txt(txt, tagger=NULL, objects.only=TRUE)
  txt.TT.res <- tagged.text@TT.res
  # set the language definition
  lang <- language.setting(tagged.text, NULL)

  pre.results <- tagged.txt.rm.classes(txt.TT.res, lemma=FALSE, lang=lang, corp.rm.class=corp.rm.class, corp.rm.tag=corp.rm.tag, as.vector=as.vector)

  if(isTRUE(as.vector)){
    results <- pre.results
  } else {
    dimnames(pre.results)[[1]] <- c(1:nrow(pre.results))
    results <- new("kRp.tagged", lang=lang, TT.res=pre.results)
  }

  return(results)
}
