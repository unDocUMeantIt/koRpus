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


#' Paste koRpus objects
#'
#' Paste the text in koRpus objects.
#'
#' This function takes objects of either class \code{kRp.tagged}, \code{kRp.txt.freq} or \code{kRp.analysis} and pastes only
#' the actual text as is.
#'
#' @param txt An object of class \code{\link[koRpus]{kRp.txt.trans-class}}, \code{\link[koRpus]{kRp.tagged-class}},
#'    \code{\link[koRpus]{kRp.txt.freq-class}} or \code{\link[koRpus]{kRp.analysis-class}}.
#' @param replace A named character vector to define replacements for \code{koRpus}' internal headline and paragraph tags.
#' @return An atomic character vector.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords misc
#' @export
#' @examples
#' \dontrun{
#' tagged.text.obj <- freq.analysis("/some/text.txt", corp.freq=my.LCC.data)
#' kRp.text.paste(tagged.text.obj)
#' }

kRp.text.paste <- function(txt, replace=c(hon.kRp="", hoff.kRp="\n\n", p.kRp="\n\n")){
  # deal with the txt object
  if(inherits(txt, "kRp.tagged")){
    # get class kRp.tagged from txt object
    # the internal function tag.kRp.txt() will return the object unchanged if it
    # is already tagged
    TT.res <- tag.kRp.txt(txt, objects.only=TRUE)@TT.res
  } else {
    stop(simpleError("Wrong object class (txt)!"))
  }

  # we probably need to replace tags
  if("hon.kRp" %in% names(replace)){
    TT.res[TT.res[["tag"]] == "hon.kRp", "token"] <- replace["hon.kRp"]
  } else {}
  if("hoff.kRp" %in% names(replace)){
    TT.res[TT.res[["tag"]] == "hoff.kRp", "token"] <- replace["hoff.kRp"]
  } else {}
  if("p.kRp" %in% names(replace)){
    TT.res[TT.res[["tag"]] == "p.kRp", "token"] <- replace["p.kRp"]
  } else {}

  txt <- TT.res$token

  # put all text together
  results <- paste.tokenized.text(txt=txt)

  return(results)
}
