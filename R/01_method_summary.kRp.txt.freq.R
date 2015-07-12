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


#' @export
#' @docType methods
#' @rdname summary-methods
#' @aliases summary,kRp.txt.freq-method
#' @examples
#' \dontrun{
#' summary(freq.analysis(tagged.txt))
#' }
#' @include 00_class_01_kRp.tagged.R
#' @include 00_class_03_kRp.txt.freq.R
#' @include 01_method_summary.kRp.lang.R
setMethod("summary", signature(object="kRp.txt.freq"), function(object){

  summary.table <- t(data.frame(
    sentences=object@desc[["sentences"]],
    avg.sentence.length=object@desc[["avg.sentc.length"]],
    words=object@desc[["words"]],
    avg.word.length=object@desc[["avg.word.length"]],
    all.characters=object@desc[["all.chars"]],
    letters=object@desc[["letters"]][["all"]],
    lemmata=object@desc[["lemmata"]],
    questions=object@desc[["questions"]],
    exclamations=object@desc[["exclam"]],
    semicolon=object@desc[["semicolon"]],
    colon=object@desc[["colon"]],
    stringsAsFactors=FALSE))

  dimnames(summary.table)[[2]] <- "freq"

  return(summary.table)
})
