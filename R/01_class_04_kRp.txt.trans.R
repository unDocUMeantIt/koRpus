# Copyright 2010-2018 Meik Michalke <meik.michalke@hhu.de>
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


#' S4 Class kRp.txt.trans
#'
#' This class is used for objects that are returned by \code{\link[koRpus:textTransform]{textTransform}}.
#'
#' @section Contructor function:
#' Should you need to manually generate objects of this class (which should rarely be the case), the contructor function 
#' \code{kRp_txt_trans(...)} can be used instead of
#' \code{new("kRp.txt.trans", ...)}.
#'
#' @slot lang A character string, naming the language that is assumed for the analized text in this object.
#' @slot desc Descriptive statistics of the tagged text.
#' @slot TT.res A data.frame with the fully tagged and transformed text (like \code{TT.res} in class \code{koRpus.tagged}, plus
#'    the new columns \code{token.old} and \code{equal}).
#' @slot diff A list with atomic vectors, describing the amount of diffences between both text variants (percentage):
#'    \describe{
#'      \item{\code{all.tokens}:}{Percentage of all tokens, including punctuation, that were altered.}
#'      \item{\code{words}:}{Percentage of altered words only.}
#'      \item{\code{all.chars}:}{Percentage of all characters, including punctuation, that were altered.}
#'      \item{\code{letters}:}{Percentage of altered letters in words only.}
#'    }
#' @name kRp.txt.trans,-class
#' @aliases kRp.txt.trans-class
#' @import methods
#' @keywords classes
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @export kRp_txt_trans
#' @exportClass kRp.txt.trans
#' @rdname kRp.txt.trans-class
#' @include 01_class_01_kRp.tagged.R
kRp_txt_trans <- setClass("kRp.txt.trans",
    representation=representation(
    diff="list"),
  prototype=prototype(
    lang=character(),
    desc=list(),
    TT.res=data.frame(token=NA, tag=NA, lemma=NA, lttr=NA, wclass=NA, desc=NA, token.old=NA, equal=NA),
    diff=list()),
  contains=c("kRp.tagged")
)

#' @include 01_class_01_kRp.tagged.R
setAs(from="kRp.txt.trans", to="kRp.tagged", function(from){
    tagged.df <- as.data.frame(taggedText(from)[, valid.TT.res.kRp.tagged])
    retagged.object <- kRp_tagged(
      lang=language(from),
      TT.res=tagged.df
    )
    return(retagged.object)
  }
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
