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


#' Show methods for koRpus objects
#'
#' Show methods for S4 objects of classes \code{\link[koRpus]{kRp.lang-class}},
#' \code{\link[koRpus]{kRp.readability-class}}, \code{\link[koRpus]{kRp.corp.freq-class}} or
#' \code{\link[koRpus]{kRp.TTR-class}}.
#'
#' @param object An object of class \code{kRp.lang}, \code{kRp.readability}, \code{kRp.corp.freq}
#'    or \code{kRp.TTR}.
#' @aliases show,-methods show,kRp.lang-method
#' @seealso
#'    \code{\link[koRpus]{kRp.lang-class}},
#'    \code{\link[koRpus]{kRp.readability-class}},
#'    \code{\link[koRpus]{kRp.corp.freq-class}},
#'    \code{\link[koRpus]{kRp.TTR-class}}
#' @keywords methods
#' @examples
#' \dontrun{
#'   guess.lang("/home/user/data/some.txt", udhr.path="/home/user/data/udhr_txt/")
#' }
#' @export
#' @docType methods
#' @rdname show-methods
setGeneric("show")

#' @export
#' @docType methods
#' @rdname show-methods
#' @include 00_class_09_kRp.lang.R
setMethod("show", signature(object="kRp.lang"), function(object){
  estim.lang <- object@lang.name
  estim.lang.uli <- object@lang
  estim.lang.country <- object@udhr[1,"country"]
  estim.lang.region <- object@udhr[1,"region"]
  langs.available <- dim(object@udhr)[1]

  cat("\n  Estimated language: ", estim.lang,
     "\n          Identifier: ", estim.lang.uli,
     "\n             Country: ", estim.lang.country, " (", estim.lang.region,")\n",
     "\n", langs.available, " different languages were checked.\n\n",
     sep="")

})
