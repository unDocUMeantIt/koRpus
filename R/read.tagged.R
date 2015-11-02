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


#' Import already tagged texts
#'
#' This function can be used on text files or matrices containing already tagged text material, e.g. the results of
#' TreeTagger[1].
#'
#' Note that the value of \code{lang} must match a valid language supported by \code{\link[koRpus:kRp.POS.tags]{kRp.POS.tags}}.
#' It will also get stored in the resulting object and might be used by other functions at a later point.
#'
#' @param file Either a matrix, a connection or a character vector. If the latter, that must be a valid path to a file,
#'    containing the previously analyzed text. If it is a matrix, it must contain three columns named "token", "tag", and "lemma",
#'    and only these three columns are used.
#' @param lang A character string naming the language of the analyzed corpus. See \code{\link[koRpus:kRp.POS.tags]{kRp.POS.tags}}
#'    for all supported languages.
#'    If set to \code{"kRp.env"} this is got from \code{\link[koRpus:get.kRp.env]{get.kRp.env}}.
#' @param encoding A character string defining the character encoding of the input file, like  \code{"Latin1"} or \code{"UTF-8"}.
#'    If \code{NULL}, the encoding will either be taken from a preset (if defined in \code{TT.options}), or fall back to \code{""}.
#'    Hence you can overwrite the preset encoding with this parameter.
#' @param tagger The software which was used to tokenize and tag the text. Currently, TreeTagger is the only
#'    supported tagger.
#' @param apply.sentc.end Logical, whethter the tokens defined in \code{sentc.end} should be searched and set to a sentence ending tag.
#'    You could call this a compatibility mode to make sure you get the results you would get if you called
#'    \code{\link[koRpus:treetag]{treetag}} on the original file.
#'    If set to \code{FALSE}, the tags will be imported as they are.
#' @param sentc.end A character vector with tokens indicating a sentence ending. This adds to given results, it doesn't replace them.
#' @param stopwords A character vector to be used for stopword detection. Comparison is done in lower case. You can also simply set 
#'    \code{stopwords=tm::stopwords("en")} to use the english stopwords provided by the \code{tm} package.
#' @param stemmer A function or method to perform stemming. For instance, you can set \code{stemmer=Snowball::SnowballStemmer} if you
#'    have the \code{Snowball} package installed (or \code{SnowballC::wordStem}). As of now, you cannot provide further arguments to
#'    this function.
#' @param rm.sgml Logical, whether SGML tags should be ignored and removed from output
#' @return An object of class \code{\link[koRpus]{kRp.tagged-class}}. If \code{debug=TRUE}, prints internal variable settings and
#'    attempts to return the original output if the TreeTagger system call in a matrix.
#' @keywords misc
#' @seealso \code{\link[koRpus:treetag]{treetag}},
#'    \code{\link[koRpus:freq.analysis]{freq.analysis}},
#'    \code{\link[koRpus:get.kRp.env]{get.kRp.env}},
#'    \code{\link[koRpus]{kRp.tagged-class}}
#' @references
#' Schmid, H. (1994). Probabilistic part-of-speec tagging using decision trees. In
#'    \emph{International Conference on New Methods in Language Processing}, Manchester, UK, 44--49.
#'
#' [1] \url{http://www.ims.uni-stuttgart.de/projekte/corplex/TreeTagger/DecisionTreeTagger.html}
#' @export
#' @examples
#' \dontrun{
#' tagged.results <- read.tagged("~/my.data/tagged_speech.txt", lang="en")
#' }

read.tagged <- function(file, lang="kRp.env", encoding=NULL, tagger="TreeTagger",
  apply.sentc.end=TRUE, sentc.end=c(".","!","?",";",":"),
  stopwords=NULL, stemmer=NULL, rm.sgml=TRUE){

  if(identical(lang, "kRp.env")){
    lang <- get.kRp.env(lang=TRUE)
  } else {}

  # file can be a matrix-like object with previously tagged text
  if(is.matrix(file) | is.data.frame(file)){
    if(!ncol(file) == 3 | any(!c("token","tag","lemma") %in% colnames(file))){
      stop(simpleError("If 'file' is a matrix, it must have three columns named \"token\", \"tag\", and \"lemma\"!"))
    } else {
      tagged.mtrx <- as.matrix(file[,c("token","tag","lemma")])
    }
    haveMatrix <- TRUE
  } else {
    haveMatrix <- FALSE
    if(inherits(file, "connection")){
      takeAsFile <- file
    } else {
      # does the text file exist?
      takeAsFile <- normalizePath(file)
      check.file(takeAsFile, mode="exist")
    }

    ## read the file
    if(is.null(encoding)){
      encoding <- ""
    } else {}
    tagged.text <- readLines(takeAsFile, encoding=encoding)
    tagged.text <- enc2utf8(tagged.text)

    ## workaround
    # in seldom cases TreeTagger seems to return duplicate tab stops
    # we'll try to correct for that here
    tagged.text <- gsub("\t\t", "\t", tagged.text)
  }

  if(identical(tagger, "TreeTagger")){
    if(!isTRUE(haveMatrix)){
      if(isTRUE(rm.sgml)){
        tagged.text <- tagged.text[grep("^[^<]", tagged.text)]
      } else {}
      tagged.mtrx <- matrix(unlist(strsplit(tagged.text, "\t")), ncol=3, byrow=TRUE, dimnames=list(c(),c("token","tag","lemma")))
    } else {}

    # add sentence endings as defined
    if(isTRUE(apply.sentc.end)){
      sntc.end.tag <- kRp.POS.tags(lang, tags="sentc", list.tags=TRUE)[[1]]
      matched.sentc.tokens <- tagged.mtrx[, "token"] %in% sentc.end
      tagged.mtrx[matched.sentc.tokens, "tag"] <- sntc.end.tag
    } else {}

    # add word classes, comments and numer of letters ("wclass", "desc", "lttr")
    tagged.mtrx <- treetag.com(tagged.mtrx, lang=lang)
  } else {
    ## additional taggers go here...
    stop(simpleError(paste0("Sorry, but tagger \"", tagger, "\" is not supported.")))
  }

  # probably apply stopword detection and stemming
  tagged.mtrx <- stopAndStem(tagged.mtrx, stopwords=stopwords, stemmer=stemmer, lowercase=TRUE)

  results <- new("kRp.tagged", lang=lang, TT.res=tagged.mtrx)
  ## descriptive statistics
  results@desc <- basic.tagged.descriptives(results, lang=lang,
    txt.vector=paste.tokenized.text(tagged.mtrx[["token"]]))

  return(results)
}
