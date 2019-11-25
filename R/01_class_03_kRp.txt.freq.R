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


# S4 Class kRp.txt.freq
#
# This class is used for objects that are returned by \code{\link[koRpus:freq.analysis]{freq.analysis}}.
#
# @section Contructor function:
# Should you need to manually generate objects of this class (which should rarely be the case), the contructor function 
# \code{kRp_txt_freq(...)} can be used instead of
# \code{new("kRp.txt.freq", ...)}.
#
# @slot lang A character string, naming the language that is assumed for the analized text in this object.
# @slot tokens A data.frame with a version of the fully tagged text (like \code{tokens} in class \code{kRp.text}, plus frequency data).
# @slot desc A list with detailed descriptive statistics on the analyzed text.
# @slot freq.analysis A list with information on the word frequencies of the analyzed text.
# @name kRp.txt.freq,-class
# @aliases kRp.txt.freq-class
# @import methods
# @keywords classes
# @export kRp_txt_freq
# @exportClass kRp.txt.freq
# @rdname kRp.txt.freq-class
# @include 01_class_01_kRp.text.R
# kRp_txt_freq <- setClass("kRp.txt.freq",
#     representation=representation(
#     freq.analysis="list"),
#   prototype=prototype(
#     lang=character(),
#     tokens=init.kRp.text.df(),
#     desc=list(),
#     freq.analysis=list()),
#   contains=c("kRp.text")
# )
# 
# 
# # @include 01_class_01_kRp.text.R
# setAs(from="kRp.txt.freq", to="kRp.text", function(from){
#     tagged.df <- as.data.frame(taggedText(from)[, valid.tokens.kRp.text])
#     retagged.object <- kRp_text(
#       lang=language(from),
#       desc=describe(from),
#       tokens=tagged.df
#     )
#     return(retagged.object)
#   }
# )
