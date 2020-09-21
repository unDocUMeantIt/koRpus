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


#' Analyze texts using TreeTagger and word frequencies
#'
#' This was basically a wrapper for \code{treetag()},\code{freq.analysis()} and \code{lex.div()}.
#' Please use those directly for equivalent results and more control over your workflow.
#'
#' @param ... Any option to be ignored.
#' @rdname koRpus-deprecated
#' @name koRpus-deprecated
#' @export

kRp.text.analysis <- function(...){
  .Deprecated(
    new="freq.analysis, lex.div"
  )
  return(invisible(NULL))
}
