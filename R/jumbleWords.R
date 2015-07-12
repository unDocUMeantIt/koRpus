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


#' Produce jumbled words
#' 
#' This function takes either a character vector or objects inheriting class \code{kRp.tagged}
#' (i.e., text tokenized by \code{koRpus}), and jumbles the words. This usually means that the
#' first and last letter of each word is left intact, while all characters inbetween are being
#' randomized.
#' 
#' @param words Either a character vector or an object inheriting from class \code{kRp.tagged}.
#' @param min.length An integer value, defining the minimum word length. Words with less characters
#'    will not be changed.
#' @param intact A named vector with the two integer values named \code{start} and \code{stop}.
#'    These define how many characters of each relevant words will be left unchanged at its start
#'    and its end, respectively.
#' @return Depending on the class of \code{words}, either a character vector or tagged text object.
#' @export

jumbleWords <- function(words, min.length=3, intact=c(start=1, end=1)){

  # check start and end values to leave intact
  start <- ifelse("start" %in% names(intact), intact[["start"]], 1)
  end <- ifelse("end" %in% names(intact), intact[["end"]], 1)
  stopifnot(start >= 0 && end >= 0)

  if(min.length <= sum(c(start,end))){
    stop(simpleError("'min.length' must be greater than the sum of 'intact'!"))
  }

  # is this a tokenized text object?
  if(inherits(words, "kRp.tagged")){
    words.orig <- words
    words.TT.res <- slot(words, "TT.res")
    words <- words.TT.res[["token"]]
  } else {
    stopifnot(is.character(words))
    words.orig <- NULL
  }

  num.chars <- nchar(words)
  toJumble <- num.chars >= min.length
  # get only the relevant words
  jmbWords <- words[toJumble]

  jumbledPart <- sapply(strsplit(substr(jmbWords, start=start+1, stop=num.chars[toJumble]-end), split=""), 
    function(this.word){paste(sample(this.word), collapse="")})

  substr(jmbWords, start=start+1, stop=num.chars[toJumble]-end) <- jumbledPart

  # write back jumbled words
  words[toJumble] <- jmbWords
  if(!is.null(words.orig)){
    words.TT.res[["token"]] <- words
    slot(words.orig, "TT.res") <- words.TT.res
    return(words.orig)
  } else {
    return(words)
  }
}
