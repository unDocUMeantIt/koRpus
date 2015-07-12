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


#' A simple tokenizer
#'
#' This tokenizer can be used to try replace TreeTagger. Its results are not as detailed when it comes to word classes, and no
#' lemmatization is done. However, for most cases this should suffice.
#'
#' \code{tokenize} can try to guess what's a headline and where a paragraph was inserted (via the \code{detect} parameter).
#' A headline is assumed if a line of text without sentence ending punctuation is found, a paragraph if two blocks of text
#' are separated by space. This will add extra tags into the text: "<kRp.h>" (headline starts), "</kRp.h>" (headline ends)
#' and "<kRp.p/>" (paragraph), respectively. This can be useful in two cases: "</kRp.h>" will be treated like a sentence ending,
#' which gives you more control for automatic analyses. And adding to that, \code{\link[koRpus:kRp.text.paste]{kRp.text.paste}}
#' can replace these tags, which probably preserves more of the original layout.
#'
#' @param txt Either an open connection, the path to directory with txt files to read and tokenize, or a vector object
#'    already holding the text corpus.
#' @param format Either "file" or "obj", depending on whether you want to scan files or analyze the given object.
#' @param fileEncoding A character string naming the encoding of all files.
#' @param split A regular expression to define the basic split method. Should only need refinement
#'    for languages that don't separate words by space.
#' @param ign.comp A character vector defining punctuation which might be used in composita that should 
#'    not be split.
#' @param heuristics A vector to indicate if the tokenizer should use some heuristics. Can be none, one or several of the following:
#'    \itemize{
#'      \item{\code{"abbr"}}{Assume that "letter-dot-letter-dot" combinations are abbreviations and leave them intact.}
#'      \item{\code{"suf"}}{Try to detect possesive suffixes like "'s", or shorting suffixes like "'ll" and treat them as one token}
#'      \item{\code{"pre"}}{Try to detect prefixes like "s'" or "l'" and treat them as one token}
#'    }
#'    Earlier releases used the names \code{"en"} and \code{"fr"} instead of \code{"suf"} and \code{"pre"}. They are still working, that is
#'    \code{"en"} is equivalent to \code{"suf"}, whereas \code{"fr"} is now equivalent to both \code{"suf"} and \code{"pre"} (and not only
#'    \code{"pre"} as in the past, which was missing the use of suffixes in French).
#' @param heur.fix A list with the named vectors \code{pre} and \code{suf}. These will be used if \code{heuristics} were
#'    set to use one of the presets that try to detect pre- and/or suffixes. Change them if you document uses other
#'    characters than the ones defined by default.
#' @param abbrev Path to a text file with abbreviations to take care of, one per line. Note that
#'    this file must have the same encoding as defined by \code{fileEncoding}.
#' @param tag Logical. If \code{TRUE}, the text will be rudimentarily tagged and returned as an object
#'    of class \code{kRp.tagged}.
#' @param lang A character string naming the language of the analyzed text. If set to \code{"kRp.env"} this is got from \code{\link[koRpus:get.kRp.env]{get.kRp.env}}. Only needed if \code{tag=TRUE}.
#' @param sentc.end A character vector with tokens indicating a sentence ending. Only needed if \code{tag=TRUE}.
#' @param detect A named logical vector, indicating by the setting of \code{parag} and \code{hline} whether \code{tokenize} should try
#'    to detect paragraphs and headlines.
#' @param clean.raw A named list of character values, indicating replacements that should globally be made to the text prior to tokenizing it.
#'    This is applied after the text was converted into UTF-8 internally. In the list, the name of each element represents a pattern which
#'    is replaced by its value if met in the text. Since this is done by calling \code{\link[base:gsub]{gsub}}, regular expressions are basically
#'    supported. See the \code{perl} attribute, too.
#' @param perl Logical, only relevant if \code{clean.raw} is not \code{NULL}. If \code{perl=TRUE}, this is forwarded to \code{\link[base:gsub]{gsub}}
#'    to allow for perl-like regular expressions in \code{clean.raw}.
#' @param stopwords A character vector to be used for stopword detection. Comparison is done in lower case. You can also simply set 
#'    \code{stopwords=tm::stopwords("en")} to use the english stopwords provided by the \code{tm} package.
#' @param stemmer A function or method to perform stemming. For instance, you can set \code{SnowballC::wordStem} if you have
#'    the \code{SnowballC} package installed. As of now, you cannot provide further arguments to this function.
#' @return If \code{tag=FALSE}, a character vector with the tokenized text. If \code{tag=TRUE}, returns an object of class \code{\link[koRpus]{kRp.tagged-class}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords misc
#' @export
#' @examples
#' \dontrun{
#' tokenized.obj <- tokenize("~/mydata/corpora/russian_corpus/")
#' 
#' ## character manipulation
#' # this is useful if you know of problematic characters in your
#' # raw text files, but don't want to touch them directly. you
#' # don't have to, as you can substitute them, even using regular
#' # expressions. a simple example: replace all single quotes by
#' # double quotes througout the text:
#' tokenized.obj <- tokenize("~/my.data/speech.txt",
#'    clean.raw=list("'"="\""))
#' # now replace all occurrances of the letter A followed
#' # by two digits with the letter B, followed by the same
#' # two digits:
#' tokenized.obj <- tokenize("~/my.data/speech.txt",
#'    clean.raw=list("(A)([[:digit:]]{2})"="B\\2"),
#'    perl=TRUE)
#'
#' ## enabling stopword detection and stemming
#' # if you also installed the packages tm and Snowball,
#' # you can use some of their features with koRpus:
#' tokenized.obj <- tokenize("~/my.data/speech.txt",
#'    stopwords=tm::stopwords("en"),
#'    stemmer=SnowballC::wordStem)
#'
#' # removing all stopwords now is simple:
#' tokenized.noStopWords <- kRp.filter.wclass(tokenized.obj, "stopword")
#' }

tokenize <- function(txt, format="file", fileEncoding=NULL, split="[[:space:]]",
          ign.comp="-", heuristics="abbr", heur.fix=list(pre=c("\u2019","'"), suf=c("\u2019","'")),
          abbrev=NULL, tag=TRUE, lang="kRp.env", sentc.end=c(".","!","?",";",":"),
          detect=c(parag=FALSE, hline=FALSE), clean.raw=NULL, perl=FALSE, stopwords=NULL, stemmer=NULL){

  if(is.null(fileEncoding)){
    fileEncoding <- ""
  } else {}

  # basic checks before we even proceed...
  if(inherits(txt, "connection")){
    takeAsTxt <- readLines(txt, encoding=fileEncoding)
    read.txt.files <- FALSE
  } else if(identical(format, "file")){
    # valid path? file or directory?
    if(check.file(txt, mode="exist", stopOnFail=FALSE)){
      txt.file <- txt
      read.txt.files <- TRUE
    } else if(check.file(txt, mode="dir", stopOnFail=FALSE)){
      txt.file <- file.path(txt, dir(txt))
      read.txt.files <- TRUE
    } else {
      stop(simpleError(paste0("Unable to locate\n ",txt)))
    }
  } else if(identical(format, "obj")){
    takeAsTxt <- txt
    read.txt.files <- FALSE
  } else {
    stop(simpleError(paste0("Invalid value for format: ",format)))
  }

  ## read file or text vector?
  if(isTRUE(read.txt.files)){
    # read in files
    # make sure we end up with UTF-8 to avoid nasty character problems
    txt.vector <- unlist(lapply(txt.file, function(txt){
        readLines(txt, encoding=fileEncoding)
      }))
    # force text into UTF-8 format
    txt.vector <- enc2utf8(txt.vector)
  } else {
    # process object
    txt.vector <- enc2utf8(as.vector(takeAsTxt))
  }
  
  ## see if the text should be cleaned up further
  if(!is.null(clean.raw)){
    txt.vector <- clean.text(txt.vector, from.to=clean.raw, perl=perl)
  } else {}

  ## run the tokenizer
  # tokenz() is an internal function
  tokens <- tokenz(txt.vector, split=split, ign.comp=ign.comp, encoding=fileEncoding,
          heuristics=heuristics, heur.fix=heur.fix, abbrev=abbrev, tag=tag, sntc=sentc.end, detect=detect)

  if(isTRUE(tag)){
    if(identical(lang, "kRp.env")){
      lang <- get.kRp.env(lang=TRUE)
    } else {}
    # prepare commenting by adding empty lemma column
    tagged.mtrx <- cbind(tokens, lemma="")
    # add word classes, comments and numer of letters ("wclass", "desc", "lttr")
    tagged.mtrx <- treetag.com(tagged.mtrx, lang=lang)
    # probably apply stopword detection and stemming
    tagged.mtrx <- stopAndStem(tagged.mtrx, stopwords=stopwords, stemmer=stemmer, lowercase=TRUE)
    # create object, combine descriptives afterwards
    tokens <- new("kRp.tagged", lang=lang, TT.res=tagged.mtrx)
    ## descriptive statistics
    tokens@desc <- basic.tagged.descriptives(tokens, lang=lang, txt.vector=txt.vector)
  } else {}

  return(tokens)
}
