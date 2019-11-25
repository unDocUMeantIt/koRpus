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


# S4 Class kRp.analysis
#
# This class is used for objects that are returned by \code{\link[koRpus:kRp.text.analysis]{kRp.text.analysis}}.
#
# @section Contructor function:
# Should you need to manually generate objects of this class (which should rarely be the case), the contructor function 
# \code{kRp_analysis(...)} can be used instead of
# \code{new("kRp.analysis", ...)}.
#
# @slot lang A character string, naming the language that is assumed for the analized text in this object
# @slot TT.res A commented verion of the fully tagged text. Depending on input data, this is
#    identical to the slot \code{TT.res} of function \code{treetag} or \code{freq.analysis}.
# @slot desc Descriptive statistics
# @slot lex.div Information on lexical diversity
# @slot freq.analysis Information on the word frequencies of the analyzed text.
# @name kRp.analysis,-class
# @aliases kRp.analysis-class
# @import methods
# @keywords classes
# @include 01_class_02_kRp.TTR.R
# @export kRp_analysis
# @exportClass kRp.analysis
# @rdname kRp.analysis-class
# kRp_analysis <- setClass("kRp.analysis",
#     representation=representation(
#     lex.div="kRp.TTR",
#     freq.analysis="list"),
#   prototype=prototype(
#     lang=character(),
#     TT.res=init.kRp.text.df(),
#     desc=list(),
#     lex.div=kRp_TTR(),
#     freq.analysis=list()),
#   contains=c("kRp.text")
# )
# 
# 
# # @include 01_class_01_kRp.text.R
# setAs(from="kRp.analysis", to="kRp.text", function(from){
#     tagged.df <- as.data.frame(taggedText(from)[, valid.TT.res.kRp.text])
#     retagged.object <- kRp_text(
#       lang=language(from),
#       TT.res=tagged.df
#     )
#     return(retagged.object)
#   }
# )
