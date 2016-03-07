# Copyright 2010-2015 Meik Michalke <meik.michalke@hhu.de>
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


#' Plot method for objects of class kRp.tagged
#'
#' Plot method for S4 objects of class \code{\link[koRpus]{kRp.tagged-class}},
#' plots the frequencies of tagged word classes.
#'
#' @param x An object of class \code{kRp.tagged}
#' @param y From the generic \code{plot} function, ignored for koRpus class objects.
#' @param what Character string, valid options are:
#'    \describe{
#'      \item{\code{"wclass"}:}{Barplot of distribution of word classes}
#'      \item{\code{"letters"}:}{Line plot of distribution of word length in letters}
#'    }
#' @param ... Any other argument suitable for plot()
#' @seealso \code{\link[koRpus]{kRp.tagged-class}}
#' @examples
#' \dontrun{
#' tagged.results <- treetag("~/my.data/sample_text.txt", treetagger="manual", lang="en",
#'    TT.options=list(path="~/bin/treetagger", preset="en"))
#' plot(tagged.results)
#' }
#' @keywords methods plot
#' @export
#' @docType methods
#' @rdname plot-methods
setGeneric("plot", function(x, y, ...) standardGeneric("plot"))

#' @export
#' @docType methods
#' @rdname plot-methods
#' @aliases plot,kRp.tagged,missing-method
#' @include 00_class_01_kRp.tagged.R
#' @import graphics
setMethod("plot", signature(x="kRp.tagged", y="missing"), function(x, what="wclass", ...){
  if(identical(what, "wclass")){
    wclass.distrib <- summary(x)
    # Increase bottom margin to make room for rotated labels
    par(mar = c(7, 4, 4, 2) + 0.1)
    barplot.ticks <- barplot(wclass.distrib[,1], xlab="", ylab="frequency", main="Distribution of word classes", xaxt="n", ...)
    axis(1, at=barplot.ticks, labels=FALSE)
    labels <- rownames(wclass.distrib)
    text(barplot.ticks, par("usr")[3]-6, srt=45, adj=1, labels=labels, xpd=TRUE)
    mtext(1, text="word class", line=6)
  } else if(identical(what, "letters")){
    lttr.distrib <- x@desc[["lttr.distrib"]]
    plot(lttr.distrib["pct",], type="l", xlab="letters", ylab="percent", main="Distribution of word lengths (letters)", xaxt="n", ...)
    axis(1, at=c(1:ncol(lttr.distrib)), labels=colnames(lttr.distrib))
  } else {}
})
