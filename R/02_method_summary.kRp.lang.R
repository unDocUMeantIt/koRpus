# Copyright 2010-2017 Meik Michalke <meik.michalke@hhu.de>
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


#' Summary methods for koRpus objects
#'
#' Summary method for S4 objects of classes
#' \code{\link[koRpus]{kRp.lang-class}},
#' \code{\link[koRpus]{kRp.readability-class}},
#' \code{\link[koRpus]{kRp.tagged-class}},
#' \code{\link[koRpus]{kRp.TTR-class}} or
#' \code{\link[koRpus]{kRp.txt.freq-class}}.
#'
#' @param object An object of class, \code{kRp.lang}, \code{kRp.readability}, 
#'    \code{kRp.tagged}, \code{kRp.TTR} or \code{kRp.txt.freq}.
#' @seealso
#'    \code{\link[koRpus]{kRp.lang-class}},
#'    \code{\link[koRpus]{kRp.readability-class}},
#'    \code{\link[koRpus]{kRp.tagged-class}},
#'    \code{\link[koRpus]{kRp.TTR-class}},
#'    \code{\link[koRpus]{kRp.txt.freq-class}}
#' @keywords methods
#' @rdname summary-methods
#' @export
#' @docType methods
#' @examples
#' \dontrun{
#' summary(guess.lang("/home/user/data/some.txt", udhr.path="/home/user/data/udhr_txt/"))
#' }
#' @include 01_class_09_kRp.lang.R
#' @aliases summary,kRp.lang-method
setMethod("summary", signature(object="kRp.lang"), function(object){
  # show the main results
  show(object)

  udhr <- slot(object, "udhr")
  # then some statistics
  cat("Distribution of compression differences:\n")
  print(summary(udhr[["diff"]]))
  cat("\n  SD:", round(sd(udhr[["diff"]]), digits=2), "\n\n")

  langs.available <- nrow(udhr)
  selectCols <- c("name","uli","iso639-3","bcp47","country","region","diff","diff.std")
  # the 'country' data was only present in the index.xml until ~2014
  # let's omit all columns that aren't available
  selectCols <- selectCols[selectCols %in% colnames(udhr)]
  top5 <- top5.shown <- subset(head(udhr, n=5), select=selectCols)
  last5 <- last5.shown <- subset(tail(udhr, n=5), select=selectCols)
  # nicen up the visible output
  lang.name.len <- max(nchar(c(top5.shown[["name"]], last5.shown[["name"]])))
  lang.region.len <- max(nchar(c(top5.shown[["region"]], last5.shown[["region"]])))
  top5.shown[["name"]] <- sprintf(paste0("%",lang.name.len + nchar(langs.available) - 1,"s"), top5.shown[["name"]])
  last5.shown[["name"]] <- sprintf(paste0("%",lang.name.len,"s"), last5.shown[["name"]])
  top5.shown[["region"]] <- sprintf(paste0("%",lang.region.len,"s"), top5.shown[["region"]])
  last5.shown[["region"]] <- sprintf(paste0("%",lang.region.len,"s"), last5.shown[["region"]])
  cat("Top 5 guesses:\n")
  print(top5.shown)
  cat("\nLast 5 guesses:\n")
  print(last5.shown)

  summary.list <- list(top5=top5, last5=last5)
  return(invisible(summary.list))
})
