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


#' Handling hyphenation pattern objects
#' 
#' This function can be used to examine and change hyphenation pattern objects be used with \code{\link[koRpus:hyphen]{hyphen}}.
#'
#' You can only run one of the possible actions at a time. If any of these arguments is not \code{NULL},
#' the corresponding action is done in the following order, and every additional discarded:
#' \itemize{
#'   \item{\code{get}}{Searches the pattern set for a given word part}
#'   \item{\code{set}}{Adds or replaces a pattern in the set (duplicates are removed)}
#'   \item{\code{rm}}{Removes a word part and its pattern from the set}
#'   \item{\code{word}}{Hyphenates a word and returns all parts examined as well as all matching patterns}
#' }
#' 
#' If all action arguments are \code{NULL}, \code{manage.hyph.pat} returns the full pattern object. 
#'
#' @param hyph.pattern Either an object of class \code{kRp.hyph.pat}, or a valid language abbreviation for patterns included in this package.
#' @param get A character string, part of a word to look up in the pattern set, i.e., without the numbers indicating split probability.
#' @param set A character string, a full pattern to be added to the pattern set, i.e., including the numbers indicating split probability.
#' @param rm A character string, part of a word to remove from the pattern set, i.e., without the numbers indicating split probability.
#' @param word A character string, a full word to hyphenate using the given pattern set.
#' @param min.length Integer, number of letters a word must have for considering a hyphenation.
#' @param rm.hyph Logical, whether appearing hyphens in words should be removed before pattern matching.
#' @return If all action arguments are \code{NULL}, returns an object of class \code{\link[koRpus:kRp.hyph.pat-class]{kRp.hyph.pat-class}}.
#'    The same is true if \code{set} or \code{rm} are set and \code{hyph.pattern} is itself an object of that class; if you refer to a language
#'    instead, pattern changes will be done internally for the running session and take effect immediately.
#'    The \code{get} argument will return a caracter vector, and \code{word} a data frame.
#' @keywords hyphenation
#' @seealso
#'    \code{\link[koRpus:kRp.hyph.pat-class]{kRp.hyph.pat-class}},
#'    \code{\link[koRpus:hyphen]{hyphen}}
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @references
#' [1] \url{http://tug.ctan.org/tex-archive/language/hyph-utf8/tex/generic/hyph-utf8/patterns/txt/}
#' @export
#' @examples
#' \dontrun{
#' manage.hyph.pat("en", set="r3ticl")
#' manage.hyph.pat("en", get="rticl")
#' manage.hyph.pat("en", word="article")
#' manage.hyph.pat("en", rm="rticl")
#' }

## function manage.hyph.pat()
manage.hyph.pat <- function(hyph.pattern, get=NULL, set=NULL, rm=NULL,
  word=NULL, min.length=3, rm.hyph=TRUE){

  if(inherits(hyph.pattern, "kRp.hyph.pat")) {
    hyph.pat <- hyph.pattern
    koRpus.env=FALSE
  } else {
    hyph.pat <- load.hyph.pattern(hyph.pattern)
#    hyph.pat <- get(hyph.pattern, envir=.koRpus.env)
    koRpus.env=TRUE
  }

  if(!is.null(get)){
    result <- hyph.pat@pattern[hyph.pat@pattern[,"char"] == get, ]
  } else if(!is.null(set)){
    # .se5ra -> .sera
    hyphen.char <- gsub("[[:digit:]]", "", set)
    # z2weig -> "0z2weig"
    hyphen.nums <- gsub("(^[^.[:digit:]]+)", "0\\1", set, perl=TRUE)
    # .se5ra -> "00500"
    hyphen.nums <- gsub("[^[:digit:]]", 0, gsub("([^[:digit:]^[:punct:]])([[:digit:]])", "\\2", hyphen.nums, perl=TRUE))

    hyphen.pars <- cbind(orig=set, char=hyphen.char, nums=hyphen.nums)
    # check if we have duplicate entries now, then replace the old one
    old.pattern <- hyph.pat@pattern[!hyph.pat@pattern[, "char"] %in% hyphen.char,]
    new.pattern <- rbind(old.pattern, hyphen.pars)
    # order results
    hyph.pat@pattern <- new.pattern[order(new.pattern[,1]),]
    result <- hyph.pat
  } else if(!is.null(rm)){
    hyph.pat@pattern <- hyph.pat@pattern[!hyph.pat@pattern[, "char"] %in% rm,]
    result <- hyph.pat
  } else if(!is.null(word)){
    # min-lenth and max-length of patterns
    min.pat <- min(nchar(hyph.pat@pattern[,"char"]))
    max.pat <- max(nchar(hyph.pat@pattern[,"char"]))

    ## remove hyphens in word
    if(isTRUE(rm.hyph)){
      word <- gsub("-", "", word)
    } else {}
    # non-letters like leading dots confuse the algorithm. we'll remove any non-alphabetic character
    word <- gsub("[^\\p{L}]+", "", word, perl=TRUE)
    ## convert to lowercase
    word <- tolower(word)
    ## transform "word" to ".word."
    word.dotted <- paste0(".", word, ".")
    word.length <- nchar(word.dotted)

    ## create word fragments ".w", ".wo", ".wor"... "rd."
    # first, define all possible start values. obviously it starts with the first letter
    # since minimal patten length is knwon, the last start value is (last character - min-length + 1)
    iter.start.points <- c(1:(word.length - min.pat))

    word.fragments <- data.frame(sapply(iter.start.points, function(start){
        # if there's less of the word left than there's patterns to match,
        # don't care about too long patterns
        rest.of.word <- word.length - start
        iter.counter <- min.pat
        iter.counter.max <- min(c(max.pat, max(rest.of.word, min.pat))) + 1
        sub.fragments <- sapply(iter.counter:iter.counter.max, function(frag.stop){
            frag.stop <- (start + frag.stop - 1)
            word.part <- substr(word.dotted, start, frag.stop)
            # return a vector with the fragment and its start/end points in the word
            return(c(frag=word.part, on=start, off=frag.stop))
          })
      }), stringsAsFactors=FALSE)
    # find all matching patterns of the word fragments
    frag.matches <- hyph.pat@pattern[match(word.fragments["frag",], hyph.pat@pattern[,"char"]),"orig"]
    frag.matches[is.na(frag.matches)] <- "--"
    matched.patterns <- rbind(word.fragments[-1,], match=frag.matches)
    colnames(matched.patterns) <- word.fragments["frag",]
    result <- matched.patterns
  } else {
    result <- hyph.pat
  }

  if(isTRUE(koRpus.env) & is.null(word) & (!is.null(set) | !is.null(rm))){
    assign(hyph.pattern, result, envir=.koRpus.env)
    return(invisible(NULL))
  } else {
    return(result)
  }
}
