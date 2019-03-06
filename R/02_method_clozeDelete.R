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


#' Transform text into cloze test format
#' 
#' If you feed a tagged text object to this function, its text will be transformed into
#' a format used for cloze deletion tests. That is, by default every fifth word (or as specified by
#' \code{every}) will be replaced by a line. You can also set an offset value to specify where
#' to begin.
#' 
#' The option \code{offset="all"} will not return one single object, but print the results after iterating
#' through all possible offset values.
#'
#' @export
#' @docType methods
#' @param ... Additional arguments to the method (as described in this document).
#' @return An object of class  \code{\link[koRpus:kRp.txt.trans-class]{kRp.txt.trans}}.
#' @rdname clozeDelete-methods
#' @examples
#' \dontrun{
#'   clozed.text <- clozeDelete(tagged.text)
#' }
setGeneric("clozeDelete", function(obj, ...){standardGeneric("clozeDelete")})

#### internal function 
## function clozify()
# replaces a word with undercores
clozify <- function(words, replace.by="_"){
  num.chars <- nchar(words, type="width")
  word.rest <- sapply(seq_along(num.chars), function(idx){
      return(paste(rep(replace.by, num.chars[idx]), collapse=""))
    })
  return(word.rest)
} ## end function clozify()

#' @export
#' @docType methods
#' @rdname clozeDelete-methods
#' @aliases clozeDelete,kRp.taggedText-method
#' @param obj An object of class "kRp.taggedText"
#' @param every Integer numeric, setting the frequency of words to be manipulated. By default,
#'    every fifth word is being transformed.
#' @param offset Either an integer numeric, sets the number of words to offset the transformations. Or the
#'    special keyword \code{"all"}, which will cause the method to iterate through all possible offset values
#'    and not return an object, but print the results (including the list with changed words).
#' @param replace.by Character, will be used as the replacement for the removed words.
#' @param fixed Integer numberic, defines the length of the replacement (\code{replace.by} will
#'    be repeated this much times). If set to 0, the replacement wil be as long as the replaced word.
#' @include 01_class_01_kRp.tagged.R
#' @include 01_class_02_kRp.TTR.R
#' @include 01_class_03_kRp.txt.freq.R
#' @include 01_class_04_kRp.txt.trans.R
#' @include 01_class_05_kRp.analysis.R
#' @include 01_class_80_kRp.taggedText_union.R
#' @include koRpus-internal.R
setMethod("clozeDelete",
  # "kRp.taggedText" is a ClassUnion defined in koRpus-internal.R
  signature(obj="kRp.taggedText"),
  function (obj, every=5, offset=0, replace.by="_", fixed=10){

    if(identical(offset, "all")){
      for(idx in seq_along(every)-1){
        clozeTxt <- clozeDelete(obj=obj, every=every, offset=idx, replace.by=replace.by, fixed=fixed)
        # changedTxt <- describe(clozeTxt)[["cloze"]][["origText"]]
        changedTxt <- originalText(clozeTxt)
        rmLetters <- sum(changedTxt[["lttr"]])
        allLetters <- describe(obj)[["letters.only"]]
        cat(headLine(paste0("Cloze variant ", idx+1, " (offset ", idx, ")")), "\n\n",
          pasteText(clozeTxt), "\n\n\n", headLine(paste0("Changed text (offset ", idx, "):"), level=2), "\n\n",
          sep="")
        print(changedTxt)
        cat("\n\n", headLine(paste0("Statistics (offset ", idx, "):"), level=2), "\n", sep="")
        print(summary(as(clozeTxt, "kRp.tagged")))
        cat("\nCloze deletion took ", rmLetters, " letters (", round(rmLetters * 100 / allLetters, digits=2),"%)\n\n\n", sep="")
      }
      return(invisible(NULL))
    } else {
      stopifnot(is.numeric(offset))
      if(offset > every){
        stop(simpleError("'offset' can't be greater than 'every'!"))
      } else {}

      lang <- language(obj)
      tagged.text <- taggedText(obj)

      # now do the actual text alterations
      word.tags <- kRp.POS.tags(lang=lang, list.tags=TRUE, tags="words")
      # we'll only care for actual words
      txtToChange <- tagged.text[["tag"]] %in% word.tags
      txtToChangeTRUE <- which(txtToChange)
      # implement the offset by removing the first words
      txtToChangeTRUE <- txtToChangeTRUE[offset+seq_along(txtToChangeTRUE)]
      changeIndex <- txtToChangeTRUE[!seq_along(txtToChangeTRUE) %% every == 0]
      txtToChange[changeIndex] <- FALSE
      txtToChange[0:max(0,offset-1)] <- FALSE

      relevant.text <- tagged.text[txtToChange, "token"]
      # check if the deleted text should be replaced by a line with fixed length
      if(identical(fixed, 0)){
        relevant.text <- clozify(relevant.text, replace.by=replace.by)
      } else {
        relevant.text <- rep(paste(rep("_", fixed), collapse=""), length(relevant.text))
      }
      tagged.text[txtToChange, "token"] <- relevant.text

      results <- txt_trans_diff(obj=obj, TT.res.new=tagged.text, transfmt="clozeDelete")
      return(results)
    }
  }
)
