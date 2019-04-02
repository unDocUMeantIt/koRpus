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


#' Get elaborated word tag definitions
#'
#' This function can be used to get a set of part-of-speech (POS) tags for a given language. These tag sets should conform
#' with the ones used by TreeTagger.
#'
#' Use \code{\link[koRpus:available.koRpus.lang]{available.koRpus.lang}} to get a list of all supported languages. Language
#' support packages must be installed an loaded to be usable with \code{kRp.POS.tags}.
#' For the internal tokenizer a small subset of tags is also defined, available through \code{lang="kRp"}.
#' Finally, the Universal POS Tags[1] are automatically appended if no matching tag was already defined.
#' If you don't know the language your text was written in, the function \code{\link[koRpus:guess.lang]{guess.lang}}
#' should be able to detect it.
#'
#' With the element \code{tags} you can specify if you want all tag definitions, or a subset, e.g. tags only for punctuation and
#' sentence endings (that is, you need to call for both "punct" and "sentc" to get all punctuation tags).
#'
#' The function is not so much intended to be used directly, but it is called by several other functions internally. However,
#' it can still be useful to directly examine available POS tags.
#'
#' @param lang A character string defining a language (see details for valid choices).
#' @param list.classes Logical, if \code{TRUE} only the known word classes for the chosen language will me returned.
#' @param list.tags Logical, if \code{TRUE} only the POS tags for the chosen language will me returned.
#' @param tags A character vector with at least one of "words", "punct" or "sentc".
#' @return If \code{list.classes=FALSE} and \code{list.tags=FALSE} returns a matrix with word tag definitions of the given language.
#' The matrix has three columns:
#'    \describe{
#'      \item{\code{tag}:}{Word tag}
#'      \item{\code{class}:}{Respective word class}
#'      \item{\code{desc}:}{"Human readable" description of what the tag stands for}
#'    }
#' Otherwise a vector with the known word classes or POS tags for the chosen language (and probably tag subset) will be returned.
#' If both \code{list.classes} and \code{list.tags} are \code{TRUE}, still only the POS tags will be returned.
#' @keywords misc
#' @references
#' [1] \url{https://universaldependencies.org/u/pos/index.html}
#' @seealso
#'    \code{\link[koRpus:get.kRp.env]{get.kRp.env}},
#'    \code{\link[koRpus:available.koRpus.lang]{available.koRpus.lang}},
#'    \code{\link[koRpus:install.koRpus.lang]{install.koRpus.lang}}
#' @export
#' @rdname kRp.POS.tags
#' @examples
#' tags.internal <- kRp.POS.tags("kRp")
#' \dontrun{
#' library(koRpus.lang.de)
#' tags.de <- kRp.POS.tags("de")
#' }

kRp.POS.tags <- function(lang=get.kRp.env(lang=TRUE), list.classes=FALSE, list.tags=FALSE, tags=c("words", "punct", "sentc")){

  if(is.na(sum(match(tags, c("words", "punct", "sentc"))))){
    stop(simpleError("Invalid tags declared (must be at least one of \"words\", \"punct\" or \"sentc\")!"))
  } else {}

  if(!identical(lang, "kRp")){
    lang <- is.supported.lang(lang, support="treetag")
  } else {}

  # get the tag set from the internal environment
  all.POS.tags <- as.list(as.environment(.koRpus.env))[["langSup"]][["kRp.POS.tags"]][["tags"]][[lang]]
  if(is.null(all.POS.tags) & !identical(lang, "kRp")){
    stop(simpleError("No tags found for this language!"))
  } else {
    tag.class.def.words <- all.POS.tags[["tag.class.def.words"]]
    tag.class.def.punct <- all.POS.tags[["tag.class.def.punct"]]
    tag.class.def.sentc <- all.POS.tags[["tag.class.def.sentc"]]
  }

  # kRp -- dummy definitions for internal tokenizer
  tag.class.def.words.kRp <- matrix(c(
    "word.kRp", "word", "Word (kRp internal)",
    "no.kRp", "number", "Number (kRp internal)",
    "abbr.kRp", "abbreviation", "Abbreviation (kRp internal)",
    "unk.kRp", "unknown", "Unknown (kRp internal)"
    ), ncol=3, byrow=TRUE, dimnames=list(c(),c("tag","wclass","desc")))
  tag.class.def.punct.kRp <- matrix(c(
    ",kRp", "comma", "Comma (kRp internal)",
    "(kRp", "punctuation", "Opening bracket (kRp internal)",
    ")kRp", "punctuation", "Closing bracket (kRp internal)",
    "''kRp", "punctuation", "Quote (kRp internal)",
    "-kRp", "punctuation", "Punctuation (kRp internal)",
    "hon.kRp", "punctuation", "Headline begins (kRp internal)",
    "p.kRp", "punctuation", "Paragraph (kRp internal)"
    ), ncol=3, byrow=TRUE, dimnames=list(c(),c("tag","wclass","desc")))
  tag.class.def.sentc.kRp <- matrix(c(
    ".kRp", "fullstop", "Sentence ending punctuation (kRp internal)",
    "hoff.kRp", "fullstop", "Headline ends (kRp internal)"
    ), ncol=3, byrow=TRUE, dimnames=list(c(),c("tag","wclass","desc")))

  # uni -- universal POS tags, added if otherwise undefined as a fallback
  # doesn't know a special tag for sentence endings, though. see
  # https://universaldependencies.org/u/pos/index.html
  tag.class.def.words.uni <- matrix(c(
    "ADJ", "adjective", "Adjective (universal POS tags)",
    "ADP", "adposition", "Adposition (universal POS tags)",
    "ADV", "adverb", "Adverb (universal POS tags)",
    "AUX", "auxiliary", "Auxiliary (universal POS tags)",
    "CCONJ", "conjunction", "Coordinating conjunction (universal POS tags)",
    "DET", "determiner", "Determiner (universal POS tags)",
    "INTJ", "interjection", "Interjection (universal POS tags)",
    "NOUN", "noun", "Noun (universal POS tags)",
    "NUM", "numeral", "Numeral (universal POS tags)",
    "PART", "particle", "Particle (universal POS tags)",
    "PRON", "pronoun", "Pronoun (universal POS tags)",
    "PROPN", "name", "Proper noun (universal POS tags)",
    "SCONJ", "conjunction", "Subordinating conjunction (universal POS tags)",
    "SYM", "symbol", "Symbol (universal POS tags)",
    "VERB", "verb", "Verb (universal POS tags)",
    "X", "other", "Not assigned a real POS category (universal POS tags)"
    ), ncol=3, byrow=TRUE, dimnames=list(c(),c("tag","wclass","desc")))
  tag.class.def.punct.uni <- matrix(c(
    "PUNCT", "punctuation", "Punctuation (universal POS tags)"
    ), ncol=3, byrow=TRUE, dimnames=list(c(),c("tag","wclass","desc")))

  ## get the needed tag definition
#  tag.wanted <- paste("tag.class.def", tags, lang, sep=".")
  tag.wanted <- paste("tag.class.def", tags, sep=".")
  if(identical(lang, "kRp")){
    tag.wanted.kRp <- paste(tag.wanted, "kRp", sep=".")
  } else {
    # append koRpus internal definitions
    tag.wanted.kRp <- c()
    for(x in tag.wanted){
      tag.wanted.kRp <- c(tag.wanted.kRp, x, paste(x, "kRp", sep="."))
    }
  }
  tag.definition <- c()
  for(x in tag.wanted.kRp){
    stopifnot(exists(x, inherits=FALSE))
    tag.definition <- rbind(tag.definition, get(x, inherits=FALSE))
    # append missing tags
    if("words" %in% tags & "tag.class.def.words.kRp" %in% x){
      tag.definition <- rbind(
        tag.definition,
        tag.class.def.words.uni[!tag.class.def.words.uni[,"tag"] %in% tag.definition[,"tag"],]
      )
    } else {}
    if("punct" %in% tags & "tag.class.def.punct.kRp" %in% x){
      tag.definition <- rbind(
        tag.definition,
        tag.class.def.punct.uni[!tag.class.def.punct.uni[,"tag"] %in% tag.definition[,"tag"],]
      )
    } else {}
  }
  
  if(isTRUE(list.classes) & !isTRUE(list.tags)){
    tag.definition <- unique(tag.definition[,"wclass"])
  } else {}
  if(isTRUE(list.tags)){
    tag.definition <- unique(tag.definition[,"tag"])
  } else {}

  return(tag.definition)
}
