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


#' Transform text into C-Test-like format
#' 
#' If you feed a tagged text object to this function, its text will be transformed into
#' a format used for C-Tests:
#' \itemize{
#'    \item{the first and last sentence will be left untouched (except if the \code{start}
#'      and \code{stop} values of the \code{intact} parameter are changed}
#'    \item{of all other sentences, the second half of every 2nd word (or as specified by
#'      \code{every}) will be replaced by a line}
#'    \item{words must have at least \code{min.length} characters, otherwise they are
#'      skipped}
#'    \item{words an uneven number of characters will be replaced after the next character,
#'      i.e., a word with five characters will keep the first three and have the last two
#'      replaced}
#' }
#'
#' @export
#' @param ... Additional arguments to the method (as described in this document).
#' @docType methods
#' @return And object of class kRp.tagged, with an additional list \code{cTest} in its
#'    \code{desc} slot, listing the words which were changed.
#' @rdname cTest-methods
setGeneric("cTest", function(obj, ...){standardGeneric("cTest")})

#### internal function 
## function cTestify()
# replaces the second half of a word with undercores
cTestify <- function(words, replace.by="_"){
  num.chars <- nchar(words)
  half.words <- ifelse(num.chars %% 2 == 0, num.chars / 2, (num.chars+1) / 2)
  word.rest <- sapply(1:length(num.chars), function(idx){
      return(paste(rep(replace.by, num.chars[idx]-half.words[idx]), collapse=""))
    })
  substr(words, start=half.words+1, stop=num.chars) <- word.rest
  return(words)
} ## end function cTestify()

#' @export
#' @docType methods
#' @rdname cTest-methods
#' @aliases cTest,kRp.tagged-method
#' @param obj An object of class "kRp.tagged"
#' @param every Integer numeric, setting the frequency of words to be manipulated. By default,
#'    every other word is being transformed.
#' @param min.length Integer numeric, sets the minimum length of words to be considered (in letters).
#' @param intact Named vector with the elements \code{start} and \code{end}. both must be integer values
#'    and define, which sentences are to be left untouched, counted in sentences from beginning and end of the text.
#'    The default is to ignore the first and last sentence.
#' @param replace.by Character, will be used as the replacement for the removed word halves.
#' @include 00_class_01_kRp.tagged.R
setMethod("cTest",
    signature(obj="kRp.tagged"),
    function (obj, every=2, min.length=3, intact=c(start=1, end=1), replace.by="_"){
    # check start and end values to leave intact
    start <- ifelse("start" %in% names(intact), intact[["start"]], 1)
    end <- ifelse("end" %in% names(intact), intact[["end"]], 1)
    stopifnot(start >= 0 && end >= 0)

    lang <- slot(obj, "lang")
    tagged.text <- slot(obj, "TT.res")
    # find out where the sentences end
    sntc.tags <- kRp.POS.tags(lang=lang, list.tags=TRUE, tags="sentc")
    txt.sntc.ends <- which(tagged.text[["tag"]] %in% sntc.tags)
    # make a list of the sentences, i.e., each element is a data.frame of one sentence
    txt.sntc.list <- lapply(1:length(txt.sntc.ends), function(idx){
        txt.from <- ifelse(idx == 1, 1, txt.sntc.ends[idx-1]+1)
        txt.to <- txt.sntc.ends[idx]
        return(tagged.text[txt.from:txt.to,])
      })
    num.sntc <- length(txt.sntc.list)

    # now do the actual text alterations
    word.tags <- kRp.POS.tags(lang=lang, list.tags=TRUE, tags="words")
    txt.cTestified <- list()
    textThatWasChanged <- list()
    for(idx in 1:num.sntc){
        this.sntc <- txt.sntc.list[[idx]]
        # check if this sentence needs manipulation at all
        if(idx <= start || idx > (num.sntc - end)){
        } else {
          # we'll only care for actual words with min.length
          txtIsWord <- this.sntc[["tag"]] %in% word.tags
          txtMinChars <- this.sntc[["lttr"]] >= min.length
          txtToChange <- txtIsWord & txtMinChars

          # now only take every other word (like indicated by "every")
          txtToChangeTRUE <- which(txtToChange)
          txtToChange[txtToChangeTRUE[!1:length(txtToChangeTRUE) %% every == 0]] <- FALSE
          relevant.text <- this.sntc[txtToChange, "token"]
          textThatWasChanged[[idx]] <- this.sntc[txtToChange, ]
          relevant.text <- cTestify(relevant.text, replace.by=replace.by)
          this.sntc[txtToChange, "token"] <- relevant.text
        }
        txt.cTestified[[idx]] <- this.sntc
      }

    # hm, this is a list now, make a data.frame again
    result.DF <- txt.cTestified[[1]]
    for(this.sntc in txt.cTestified[-1]){
      result.DF <- rbind(result.DF, this.sntc)
    }
    changed.DF <- textThatWasChanged[[1]]
    for(this.sntc in textThatWasChanged[-1]){
      changed.DF <- rbind(changed.DF, this.sntc)
    }
    cTestChanged <- list(origText=changed.DF)

    # put the altered text back into the tagged object
     slot(obj, "TT.res") <- result.DF
    slot(obj, "desc")[["cTest"]] <- cTestChanged
     return(obj)
  }
)
