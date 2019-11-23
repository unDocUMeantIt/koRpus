# Copyright 2016-2019 Meik Michalke <meik.michalke@hhu.de>
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


#' @export
#' @docType methods
#' @aliases show,kRp.taggedText-method
#' @rdname show-methods
#' @include 02_method_show.kRp.lang.R
#' @include 01_class_80_kRp.taggedText_union.R
#' @include koRpus-internal.R
setMethod("show", signature(object="kRp.taggedText"), function(object){
  txt <- taggedText(object)

  # only print head an tail of long texts
  headLength <- formals(head.matrix)[["n"]]
  if(isTRUE(nrow(txt) > (headLength * 2 + 1))){
    # we need to extend the levels of the factors so they don't show ugly <NA>s
    factor_cols <- sapply(txt, is.factor)
    for (thisFactor in names(factor_cols)[factor_cols]){
      # the dots are actually just needed for "wclass", but we'll keep it simple here...
      levels(txt[[thisFactor]]) <- c(levels(txt[[thisFactor]]), "", "[...]")
    }
    # the tokens slot can change its columns, this must be dealt with dynamically
    middle <- txt[1,]
    for (thisCol in seq_along(middle)){
      # haha, checking for middle class :-D
      middle.class <- class(middle[thisCol])
      middle[1,thisCol] <- switch(middle.class,
        numeric=0,
        logical=NA,
        ""
      )
    }
    middle[[round(length(factor_cols) / 2)]] <- "[...]"
    rownames(middle) <- ""
    show.mtx <- rbind(head(txt), middle, tail(txt))
    show(show.mtx)
  } else {
    show(txt)
  }

  features <- slot(object, "features")
  features <- features[features]
  if(length(features) > 0){
    message(paste0("\nAdditional features:\n  \"", paste0(names(features), collapse="\", \""), "\""))
  } else {}

# add some stats if text was transformed
  if(hasFeature(object, "diff")){
    diff <- diffText(object)
    message(paste0(
      "\nDifference between original and transformed text (punctuation ignored)\n",
      "            Words: ", round(diff[["words"]], digits=2),"%\n",
      "          Letters: ", round(diff[["letters"]], digits=2),"%\n",
      "  Transformations: \"", paste0(diff[["transfmt"]], collapse="\", \""), "\""      
    ))
  } else {}
  

})
