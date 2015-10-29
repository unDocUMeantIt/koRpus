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


#' Automatic hyphenation
#'
#' These methods implement word hyphenation, based on Liang's algorithm.
#'
#' For this to work the function must be told which pattern set it should use to
#' find the right hyphenation spots. If \code{words} is already a tagged object,
#' its language definition might be used. Otherwise, in addition to the words to
#' be processed you must specify \code{hyph.pattern}. You have two options: If you
#' want to use one of the built-in language patterns, just set it to the according
#' language abbrevation. As of this version valid choices are:
#' \itemize{
#'  \item {\code{"de"}} {--- German (new spelling, since 1996)}
#'  \item {\code{"de.old"}} {--- German (old spelling, 1901--1996)}
#'  \item {\code{"en"}} {--- English (UK)}
#'  \item {\code{"en.us"}} {--- English (US)}
#'  \item {\code{"es"}} {--- Spanish}
#'  \item {\code{"fr"}} {--- French}
#'  \item {\code{"it"}} {--- Italian}
#'  \item {\code{"ru"}} {--- Russian}
#' }
#' In case you'd rather use your own pattern set, \code{hyph.pattern} can be an
#' object of class \code{kRp.hyph.pat}, alternatively.
#'
#' The built-in hyphenation patterns were derived from the patterns available on CTAN[1]
#' under the terms of the LaTeX Project Public License[2], see \code{\link[koRpus:hyph.XX]{hyph.XX}}
#' for detailed information.
#'
#' @param words Either an object of class \code{\link[koRpus]{kRp.tagged-class}}, \code{\link[koRpus]{kRp.txt.freq-class}} or
#'    \code{\link[koRpus]{kRp.analysis-class}}, or a character vector with words to be hyphenated.
#' @param hyph.pattern Either an object of class \code{\link[koRpus]{kRp.hyph.pat-class}}, or
#'    a valid character string naming the language of the patterns to be used. See details.
#' @param min.length Integer, number of letters a word must have for considering a hyphenation. \code{hyphen} will
#'    not split words after the first or before the last letter, so values smaller than 4 are not useful.
#' @param rm.hyph Logical, whether appearing hyphens in words should be removed before pattern matching.
#' @param corp.rm.class A character vector with word classes which should be ignored. The default value
#'    \code{"nonpunct"} has special meaning and will cause the result of
#'    \code{kRp.POS.tags(lang, c("punct","sentc"), list.classes=TRUE)} to be used. Relevant only if \code{words}
#'    is a valid koRpus object.
#' @param corp.rm.tag A character vector with POS tags which should be ignored. Relevant only if \code{words}
#'    is a valid koRpus object.
#' @param quiet Logical. If \code{FALSE}, short status messages will be shown.
#' @param cache Logical. \code{hyphen()} can cache results to speed up the process. If this option is set to \code{TRUE}, the
#'    current cache will be queried and new tokens also be added. Caches are language-specific and reside in an environment,
#'    i.e., they are cleaned at the end of a session. If you want to save these for later use, see the option \code{hyphen.cache.file}
#'    in \code{\link[koRpus:set.kRp.env]{set.kRp.env}}.
#' @return An object of class \code{\link[koRpus]{kRp.hyphen-class}}
#' @keywords hyphenation
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @seealso
#'    \code{\link[koRpus:read.hyph.pat]{read.hyph.pat}},
#'    \code{\link[koRpus:manage.hyph.pat]{manage.hyph.pat}}
#' @references
#'  Liang, F.M. (1983). \emph{Word Hy-phen-a-tion by Com-put-er}.
#'      Dissertation, Stanford University, Dept. of Computer Science.
#'
#' [1] \url{http://tug.ctan.org/tex-archive/language/hyph-utf8/tex/generic/hyph-utf8/patterns/}
#'
#' [2] \url{http://www.ctan.org/tex-archive/macros/latex/base/lppl.txt}
#' @export
#' @import methods
#' @rdname hyphen-methods
#' @examples
#' \dontrun{
#' hyphen(tagged.text)
#' }

#################################################################
## if this signature changes, check kRp.hyphen.calc() as well! ##
#################################################################
#' @param ... Only used for the method generic.
setGeneric("hyphen", function(words, ...) standardGeneric("hyphen"))

#' @export
#' @include 00_class_01_kRp.tagged.R
#' @include 00_class_03_kRp.txt.freq.R
#' @include 00_class_04_kRp.txt.trans.R
#' @include 00_class_05_kRp.analysis.R
#' @include koRpus-internal.R
#' @aliases hyphen,kRp.taggedText-method
#' @rdname hyphen-methods
setMethod("hyphen", signature(words="kRp.taggedText"), function(words,
    hyph.pattern=NULL, min.length=4, rm.hyph=TRUE,
    corp.rm.class="nonpunct",
    corp.rm.tag=c(), quiet=FALSE, cache=TRUE){

    # get class kRp.tagged from words object
    # the internal function tag.kRp.txt() will return the object unchanged if it
    # is already tagged, so it's safe to call it with the lang set here
    tagged.text <- tag.kRp.txt(words, objects.only=TRUE)
    lang <- language(tagged.text)
    words <- tagged.txt.rm.classes(taggedText(tagged.text), lemma=FALSE,
      lang=lang, corp.rm.class=corp.rm.class, corp.rm.tag=corp.rm.tag)
 
    results <- kRp.hyphen.calc(words=words, hyph.pattern=hyph.pattern, min.length=min.length,
      rm.hyph=rm.hyph, corp.rm.class=corp.rm.class, corp.rm.tag=corp.rm.tag, quiet=quiet, cache=cache, lang=lang)

    return(results)
  }
)

#' @export
#' @aliases hyphen,character-method
#' @rdname hyphen-methods
setMethod("hyphen", signature(words="character"), function(words,
    hyph.pattern=NULL, min.length=4, rm.hyph=TRUE,
    corp.rm.class="nonpunct",
    corp.rm.tag=c(), quiet=FALSE, cache=TRUE){

    results <- kRp.hyphen.calc(words=words, hyph.pattern=hyph.pattern, min.length=min.length,
      rm.hyph=rm.hyph, corp.rm.class=corp.rm.class, corp.rm.tag=corp.rm.tag, quiet=quiet, cache=cache)

    return(results)
  }
)
