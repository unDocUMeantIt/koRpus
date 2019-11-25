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


#' @export
#' @docType methods
#' @rdname summary-methods
#' @aliases summary,kRp.txt.freq-method
#' @examples
#' \dontrun{
#' summary(freq.analysis(tagged.txt))
#' }
#' @include 01_class_01_kRp.text.R
#' @include 01_class_03_kRp.txt.freq.R
#' @include 02_method_summary.kRp.lang.R
setMethod("summary", signature(object="kRp.txt.freq"), function(object){

  summary.table <- t(data.frame(
    sentences=describe(object)[["sentences"]],
    avg.sentence.length=describe(object)[["avg.sentc.length"]],
    words=describe(object)[["words"]],
    avg.word.length=describe(object)[["avg.word.length"]],
    all.characters=describe(object)[["all.chars"]],
    letters=describe(object)[["letters"]][["all"]],
    lemmata=describe(object)[["lemmata"]],
    questions=describe(object)[["questions"]],
    exclamations=describe(object)[["exclam"]],
    semicolon=describe(object)[["semicolon"]],
    colon=describe(object)[["colon"]],
    stringsAsFactors=FALSE))

  dimnames(summary.table)[[2]] <- "freq"

  return(summary.table)
})
