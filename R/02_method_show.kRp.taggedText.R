# Copyright 2016 Meik Michalke <meik.michalke@hhu.de>
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
#' @include koRpus-internal.R
setMethod("show", signature(object="kRp.taggedText"), function(object){
  txt <- taggedText(object)
  # the TT.res slot can change its columns, this must be dealt with dynamically
  middle <- txt[1,]
  for (thisCol in seq_along(middle)){
    # haha, checking for middle class :-D
    middle.class <- class(middle[thisCol])
    middle[1,thisCol] <- switch(middle.class,
      character="",
      numeric=0,
      logical=NA,
      ""
    )
  }
  middle[["wclass"]] <- "[...]"
  rownames(middle) <- ""
  show.mtx <- rbind(head(txt), middle, tail(txt))
  show(show.mtx)
})
