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


#' Produce jumbled words
#' 
#' This method either takes a character vector or objects inheriting class \code{kRp.tagged}
#' (i.e., text tokenized by \code{koRpus}), and jumbles the words. This usually means that the
#' first and last letter of each word is left intact, while all characters inbetween are being
#' randomized.
#' 
#' @param words Either a character vector or an object inheriting from class \code{kRp.tagged}.
#' @param min.length An integer value, defining the minimum word length. Words with less characters
#'    will not be changed. Grapheme clusters are counted as one.
#' @param intact A named vector with the two integer values named \code{start} and \code{stop}.
#'    These define how many characters of each relevant words will be left unchanged at its start
#'    and its end, respectively.
#' @param ... Additional options, currently unused.
#' @return Depending on the class of \code{words}, either a character vector or an object of class
#'    \code{\link[koRpus:kRp.txt.trans-class]{kRp.txt.trans}}.
#' @import methods
#' @docType methods
#' @export
#' @rdname jumbleWords-methods
#' @examples
#' \dontrun{
#'    jumbled.text <- jumbleWords(tagged.text)
#' }
setGeneric("jumbleWords", function(words, ...){standardGeneric("jumbleWords")})

#' @export
#' @docType methods
#' @rdname jumbleWords-methods
#' @aliases jumbleWords,kRp.taggedText-method
#' @include 01_class_80_kRp.taggedText_union.R
#' @include koRpus-internal.R
setMethod("jumbleWords",
  # "kRp.taggedText" is a ClassUnion defined in koRpus-internal.R
  signature(words="kRp.taggedText"),
  function(words, min.length=3, intact=c(start=1, end=1)){
    words.TT.res <- taggedText(words)
    words.TT.res[["token"]] <- kRp_jumbleWords(words=words.TT.res[["token"]], min.length=min.length, intact=intact)
    results <- txt_trans_diff(obj=words, TT.res.new=words.TT.res, transfmt="jumbleWords")
    return(results)
  }
)

#' @export
#' @docType methods
#' @rdname jumbleWords-methods
#' @aliases jumbleWords,character-method
setMethod("jumbleWords",
  # "kRp.taggedText" is a ClassUnion defined in koRpus-internal.R
  signature(words="character"),
  function(words, min.length=3, intact=c(start=1, end=1)){
    words <- kRp_jumbleWords(words=words, min.length=min.length, intact=intact)
    return(words)
  }
)

# this is the actual internal workhorse for the method
# - words: must be a character vector
# also returns a character vector
kRp_jumbleWords <- function(words, min.length=3, intact=c(start=1, end=1)){

  # check start and end values to leave intact
  start <- ifelse("start" %in% names(intact), intact[["start"]], 1)
  end <- ifelse("end" %in% names(intact), intact[["end"]], 1)
  stopifnot(start >= 0 & end >= 0)

  if(min.length <= sum(c(start,end))){
    stop(simpleError("'min.length' must be greater than the sum of 'intact'!"))
  }

  stopifnot(is.character(words))

  num.chars <- nchar(words, type="width")
  toJumble <- num.chars >= min.length
  # get only the relevant words
  jmbWords <- words[toJumble]

  jumbledPart <- sapply(
    strsplit(substr(jmbWords, start=start+1, stop=num.chars[toJumble]-end), split=""), 
    function(this.word){
      paste(sample(this.word), collapse="")
    }
  )

  substr(jmbWords, start=start+1, stop=num.chars[toJumble]-end) <- jumbledPart

  # write back jumbled words
  words[toJumble] <- jmbWords
  return(words)
}
