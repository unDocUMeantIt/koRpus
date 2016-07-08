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

#' S4 Class kRp.hyphen
#'
#' This class is used for objects that are returned by \code{\link[koRpus:hyphen]{hyphen}}.
#'
#' @slot lang A character string, naming the language that is assumed for the analized text in this object
#' @slot desc Descriptive statistics of the analyzed text.
#' @slot hyphen A data.frame with two columns:
#' \describe{
#'   \item{\code{syll}:}{Number of recognized syllables}
#'   \item{\code{word}:}{The hyphenated word}
#' }
#' @name kRp.hyphen,-class
#' @aliases kRp.hyphen,-class kRp.hyphen-class
#' @import methods
#' @keywords classes
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @export
#' @rdname kRp.hyphen-class

setClass("kRp.hyphen",
    representation=representation(
    lang="character",
    desc="list",
    hyphen="data.frame"),
  prototype(
    lang=character(),
    desc=list(
      num.syll=NA,
      syll.distrib=NA,
      syll.uniq.distrib=NA,
      avg.syll.word=NA,
      syll.per100=NA
    ),
    hyphen=data.frame(syll=numeric(), word=character()))
)

setValidity("kRp.hyphen", function(object){
    hyphen <- object@hyphen
    hyphen.names <- dimnames(hyphen)[[2]]

    if(!is.character(object@lang)){
      stop(simpleError("Invalid object: Slot \"lang\" must be of class character!"))
    } else {}

    if(!identical(hyphen.names, c("syll", "word"))){
      stop(simpleError("Invalid object: Wrong column names in slot \"hyphen\"!"))
    } else {}

  return(TRUE)
})
