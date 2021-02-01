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


#' S4 Class kRp.lang
#'
#' This class is used for objects that are returned by \code{\link[koRpus:guess.lang]{guess.lang}}.
#'
#' @section Contructor function:
#' Should you need to manually generate objects of this class (which should rarely be the case), the contructor function 
#' \code{kRp_lang(...)} can be used instead of
#' \code{new("kRp.lang", ...)}.
#'
#' @slot lang A character string, naming the language (by its ISO 639-3 identifier) that was estimated for the analized text in this object.
#' @slot lang.name A character string, full name of the estimated language.
#' @slot txt A character string containing the analized part of the text.
#' @slot txt.full A character string containing the full text.
#' @slot udhr A data.frame with full analysis results for each language tried.
#' @name kRp.lang,-class
#' @aliases kRp.lang-class
#' @import methods
#' @keywords classes
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @export kRp_lang
#' @exportClass kRp.lang
#' @rdname kRp.lang-class

kRp_lang <- setClass("kRp.lang",
    representation=representation(
    lang="character",
    lang.name="character",
    txt="character",
    txt.full="character",
    udhr="data.frame")
)

setMethod("initialize", "kRp.lang",
  function(
    .Object,
    lang=character(),
    lang.name=character(),
    txt=character(),
    txt.full=character(),
    udhr=data.frame()
  ){
    slot(.Object, "lang") <- lang
    slot(.Object, "lang.name") <- lang.name
    slot(.Object, "txt") <- txt
    slot(.Object, "txt.full") <- txt.full
    slot(.Object, "udhr") <- udhr
    validObject(.Object)
    return(.Object)
  }
)
