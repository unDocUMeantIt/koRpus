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


## temporarily turned off most of the roxygen comments
## class docs will remain static until roxygen2 supports "@slot"

#' S4 Class kRp.analysis
#'
#' This class is used for objects that are returned by \code{\link[koRpus:kRp.text.analysis]{kRp.text.analysis}}.
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
#' @export
#' @rdname kRp.analysis-class

#' @include 00_class_02_kRp.TTR.R
#' @include 00_class_03_kRp.txt.freq.R
setClass("kRp.analysis",
    representation=representation(
    lex.div="kRp.TTR"),
  prototype=prototype(
    lang=character(),
    TT.res=data.frame(),
    desc=list(),
    lex.div=new("kRp.TTR"),
    freq.analysis=list()),
  contains=c("kRp.txt.freq")
)


#' @include 00_class_01_kRp.tagged.R
setAs(from="kRp.analysis", to="kRp.tagged", function(from){
    lang <- from@lang
    tagged.df <- as.data.frame(from@TT.res[, valid.TT.res.kRp.tagged])
    retagged.object <- new("kRp.tagged", lang=lang, TT.res=tagged.df)
    return(retagged.object)
    }
)


#' @include 00_class_01_kRp.tagged.R
#' @include 00_class_03_kRp.txt.freq.R
setAs(from="kRp.analysis", to="kRp.txt.freq", function(from){
    lang <- from@lang
    desc <- from@desc
    freq.analysis <- from@freq.analysis
    tagged.df <- from@TT.res
    retagged.object <- new("kRp.txt.freq", lang=lang, TT.res=tagged.df, desc=desc, freq.analysis=freq.analysis)
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
