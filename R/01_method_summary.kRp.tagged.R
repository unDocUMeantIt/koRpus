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


# internal function to produce the word class distribution table
# wclass: object@TT.res[["wclass"]]
# lang:   object@lang
# abs: if not NULL, percentages will also be calculated relative to this number
wClassNoPunct <- function(wclass, lang, abs=NULL){
  word.tags <- kRp.POS.tags(lang, list.classes=TRUE, tags="words")
  wclass.num <- summary(as.factor(wclass))
  wclass.nopunct <- names(wclass.num)[names(wclass.num) %in% word.tags]
  wclass.punct <- names(wclass.num)[!names(wclass.num) %in% word.tags]
  wclass.nopunct.num <- wclass.num[wclass.nopunct]
  wclass.punct.num <- wclass.num[wclass.punct]

  wclass.nopunct.num <- wclass.nopunct.num[order(wclass.nopunct.num, decreasing=TRUE)]
  if(is.null(abs)){
    wclass.nopunct.num <- rbind(wclass.nopunct.num, 100 * wclass.nopunct.num / sum(wclass.nopunct.num))
    rownames(wclass.nopunct.num) <- c("num", "pct")
  } else {
    wclass.nopunct.num <- rbind(wclass.nopunct.num, 100 * wclass.nopunct.num / sum(wclass.nopunct.num), 100 * wclass.nopunct.num / abs)
    rownames(wclass.nopunct.num) <- c("num", "pct", "pct.abs")
  }
  if(length(wclass.punct) != 0){
    wclass.nopunct.num <- t(cbind(wclass.nopunct.num, rbind(wclass.punct.num, NA)))
  } else {
    wclass.nopunct.num <- t(wclass.nopunct.num)
  }
  return(wclass.nopunct.num)
}

#' @export
#' @docType methods
#' @rdname summary-methods
#' @aliases summary,kRp.tagged-method
#' @examples
#' \dontrun{
#' tagged.results <- treetag("~/my.data/sample_text.txt", treetagger="manual", lang="en",
#'    TT.options=list(path="~/bin/treetagger", preset="en"))
#' summary(tagged.results)
#' }
#' @include 00_class_01_kRp.tagged.R
#' @include 01_method_summary.kRp.lang.R
setMethod("summary", signature(object="kRp.tagged"), function(object){
  # to prevent hiccups from R CMD check
  Row.names <- NULL
  desc <- object@desc
  wclass.nopunct.num <- wClassNoPunct(wclass=object@TT.res[["wclass"]], lang=object@lang)
  if(!is.null(object@desc[["cloze"]][["origText"]][["wclass"]])){
    wclass.orig.order <- order(order(rownames(wclass.nopunct.num)))
    wclass.nopunct.num.cloze <- wClassNoPunct(wclass=object@desc[["cloze"]][["origText"]][["wclass"]], lang=object@lang, abs=desc[["words"]])
    colnames(wclass.nopunct.num.cloze) <- c("num.cloze", "pct.cloze", "pct.cloze.abs")
    wclass.nopunct.num <- merge(wclass.nopunct.num, wclass.nopunct.num.cloze, all=TRUE, by='row.names', sort=FALSE, suffixes=c("", ".cloze"))
    # merge adds a column for row numbers, reverse that
    rownames(wclass.nopunct.num) <- wclass.nopunct.num[["Row.names"]]
    wclass.nopunct.num <- subset(wclass.nopunct.num, select=-Row.names)
    # regain original order
    wclass.nopunct.num <- wclass.nopunct.num[order(rownames(wclass.nopunct.num))[wclass.orig.order],]
    # add another column for the percentage of words of each class which were removed
    wclass.nopunct.num[["pct.cloze.wclass"]] <- wclass.nopunct.num[["num.cloze"]] * 100 / wclass.nopunct.num[["num"]]
  } else {}

  cat(
  "\n  Sentences: ", desc[["sentences"]], "\n",
  "  Words:     ", desc[["words"]], " (", round(desc[["avg.sentc.length"]], digits=2), " per sentence)\n",
  "  Letters:   ", desc[["letters"]][["all"]], " (", round(desc[["avg.word.length"]], digits=2), " per word)\n\n  Word class distribution:\n\n",
  sep="")

  return(wclass.nopunct.num)
})

