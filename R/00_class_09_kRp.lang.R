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


#' S4 Class kRp.lang
#'
#' This class is used for objects that are returned by \code{\link[koRpus:guess.lang]{guess.lang}}.
#'
#' @slot lang A character string, naming the language (by its ISO 639-3 identifier) that was estimated for the analized text in this object.
#' @slot lang.name A character string, full name of the estimated language.
#' @slot txt A character string containing the analized part of the text.
#' @slot txt.full A character string containing the full text.
#' @slot udhr A data.frame with full analysis results for each language tried.
#' @name kRp.lang,-class
#' @aliases kRp.lang,-class kRp.lang-class
#' @import methods
#' @keywords classes
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @export
#' @rdname kRp.lang-class

setClass("kRp.lang",
    representation=representation(
    lang="character",
    lang.name="character",
    txt="character",
    txt.full="character",
    udhr="data.frame"),
  prototype(
    lang=character(),
    lang.name=character(),
    txt=character(),
    txt.full=character(),
    udhr=data.frame())
)

# setValidity("kRp.analysis", function(object){
#     TT.res <- object@TT.res
#     TT.res.names <- dimnames(TT.res)[[2]]
#     if(identical(TT.res.names, c("word","tag","lemma"))){
#       return(TRUE)
#     } else {
#       stop(simpleError("Invalid object: Wrong column names."))
#     }
# })
