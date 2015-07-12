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


#' Extract text features for authorship analysis
#' 
#' This function combines several of \code{koRpus}' methods to extract the 9-Feature Set for
#' authorship detection (Brannon, Afroz & Greenstadt, 2011; Brannon & Greenstadt, 2009).
#'
#' @param text An object of class \code{\link[koRpus]{kRp.tagged-class}},
#'    \code{\link[koRpus]{kRp.txt.freq-class}} or \code{\link[koRpus]{kRp.analysis-class}}. Can
#'    also be a list of these objects, if you want to analyze more than one text at once.
#' @param hyphen An object of class \code{\link[koRpus]{kRp.hyphen-class}}, if \code{text} has
#'    already been hyphenated. If \code{text} is a list and \code{hyphen} is not \code{NULL}, it must
#'    also be a list with one object for each text, in the same order.
#' @return A data.frame:
#'    \describe{
#'      \item{uniqWd}{Number of unique words (tokens)}
#'      \item{cmplx}{Complexity (TTR)}
#'      \item{sntCt}{Sentence count}
#'      \item{sntLen}{Average sentence length}
#'      \item{syllCt}{Average syllable count}
#'      \item{charCt}{Character count (all characters, including spaces)}
#'      \item{lttrCt}{Letter count (without spaces, punctuation and digits)}
#'      \item{FOG}{Gunning FOG index}
#'      \item{flesch}{Flesch Reading Ease index}
#'    }
#' @export
#' @references
#'    Brennan, M., Afroz, S., & Greenstadt, R. (2011). Deceiving authorship detection. Presentation
#'      at \emph{28th Chaos Communication Congress (28C3)}, Berlin, Germany.
#'    Brennan, M. & Greenstadt,R. (2009). Practical Attacks Against Authorship Recognition Techniques. In
#'      \emph{Proceedings of the Twenty-First Conference on Innovative Applications of Artificial Intelligence (IAAI)}, Pasadena, CA.
#'    Tweedie, F.J., Singh, S., & Holmes, D.I. (1996). Neural Network Applications in Stylometry: The Federalist Papers.
#'      \emph{Computers and the Humanities}, 30, 1--10.
#' @examples
#' \dontrun{
#' set.kRp.env(TT.cmd="manual", lang="en", TT.options=list(path="~/bin/treetagger", preset="en"))
#' tagged.txt <- treetag("example_text.txt")
#' tagged.txt.features <- textFeatures(tagged.txt)
#' }

textFeatures <- function(text, hyphen=NULL){

## TODO: signature approach (chisq test letters/sentences/punctuation)
# http://www.philocomp.net/humanities/signature

  if(inherits(text, "kRp.tagged")){
    # get class kRp.tagged from words object
    # the internal function tag.kRp.txt() will return the object unchanged if it
    # is already tagged, so it's safe to call it with the lang set here
    tagged.text <- tag.kRp.txt(text, objects.only=TRUE)
    tagged.text.nopunct <- kRp.filter.wclass(text, corp.rm.class="nonpunct")
  } else if(is.list(text)){
    num.texts <- length(text)
    if(!is.null(hyphen)){
      if(!identical(length(hyphen), num.texts)){
        stop(simpleError("Number of hyphenated objects differs from number of text objects!"))
      } else {}
    } else {
      # create a dummy list of NULLs
      hyphen <- rep(NULL, num.texts)
    }
    # go through texts recursively
    results <- data.frame(t(sapply(1:num.texts, function(this.text){
        textFeatures(text[[this.text]], hyphen=hyphen[[this.text]])
      })))
    rownames(results) <- as.character(substitute(text))[-1]
    return(results)
  } else {
    stop(simpleError("Please tokenize text first!"))
  }

  text.types <- length(tolower(unique(taggedText(tagged.text.nopunct)[["token"]])))
  # get syllable count
  if(is.null(hyphen)){
    text.hyph <- hyphen(tagged.text.nopunct, quiet=TRUE)
  } else {
    stopifnot(inherits(hyphen, "kRp.hyphen"))
    text.hyph <- hyphen
  }
  # calculate readability measures
  text.rdb <- readability(tagged.text, hyphen=text.hyph, index=c("Flesch", "FOG"))

  text.desc <- describe(tagged.text)
  rdb.desc <- describe(text.rdb)
  results <- data.frame(
    uniqWd=text.types,
    complx=rdb.desc[["TTR"]],
    sntCt=text.desc[["sentences"]],
    sntLen=text.desc[["avg.sentc.length"]],
    syllCt=text.hyph@desc[["avg.syll.word"]],
    charCt=text.desc[["all.chars"]],
    lttrCt=text.desc[["letters"]][["all"]],
    FOG=text.rdb@FOG[["FOG"]],
    flesch=text.rdb@Flesch[["RE"]]
  )
  return(results)
}
