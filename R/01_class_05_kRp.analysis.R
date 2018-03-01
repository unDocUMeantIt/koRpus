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


#' S4 Class kRp.analysis
#'
#' This class is used for objects that are returned by \code{\link[koRpus:kRp.text.analysis]{kRp.text.analysis}}.
#'
#' @section Contructor function:
#' Should you need to manually generate objects of this class (which should rarely be the case), the contructor function 
#' \code{kRp_analysis(...)} can be used instead of
#' \code{new("kRp.analysis", ...)}.
#'
#' @slot lang A character string, naming the language that is assumed for the analized text in this object
#' @slot TT.res A commented verion of the fully tagged text. Depending on input data, this is
#'    identical to the slot \code{TT.res} of function \code{treetag} or \code{freq.analysis}.
#' @slot desc Descriptive statistics
#' @slot lex.div Information on lexical diversity
#' @slot freq.analysis Information on the word frequencies of the analyzed text.
#' @name kRp.analysis,-class
#' @aliases kRp.analysis,-class kRp.analysis-class
#' @import methods
#' @keywords classes
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @include 01_class_02_kRp.TTR.R
#' @include 01_class_03_kRp.txt.freq.R
#' @export kRp_analysis
#' @exportClass kRp.analysis
#' @rdname kRp.analysis-class
kRp_analysis <- setClass("kRp.analysis",
    representation=representation(
    lex.div="kRp.TTR"),
  prototype=prototype(
    lang=character(),
    TT.res=data.frame(),
    desc=list(),
    lex.div=kRp_TTR(),
    freq.analysis=list()),
  contains=c("kRp.txt.freq")
)


#' @include 01_class_01_kRp.tagged.R
setAs(from="kRp.analysis", to="kRp.tagged", function(from){
    tagged.df <- as.data.frame(taggedText(from)[, valid.TT.res.kRp.tagged])
    retagged.object <- kRp_tagged(
      lang=language(from),
      TT.res=tagged.df
    )
    return(retagged.object)
  }
)


#' @include 01_class_01_kRp.tagged.R
#' @include 01_class_03_kRp.txt.freq.R
setAs(from="kRp.analysis", to="kRp.txt.freq", function(from){
    retagged.object <- kRp_txt_freq(
      lang=language(from),
      TT.res=taggedText(from),
      desc=describe(from),
      freq.analysis=slot(from, "freq.analysis")
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
