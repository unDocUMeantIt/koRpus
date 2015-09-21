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


#' Get elaborated word tag definitions
#'
#' This function can be used to get a set of part-of-speech (POS) tags for a given language. These tag sets should conform
#' with the ones used by TreeTagger.
#'
#' Currently supported languages are:
#' \itemize{
#'  \item \code{"de"} --- German, according to the STTS guidelines (Schiller, Teufel, Stockert, & Thielen, 1995)
#'  \item \code{"en"} --- English, according to the Penn Treebank guidelines (Santorini, 1991)
#'  \item \code{"es"} --- Spanish, according to \url{http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/data/spanish-tagset.txt}
#'  \item \code{"fr"} --- French, according to \url{http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/data/french-tagset.html}
#'  \item \code{"it"} --- Italian, according to \url{http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/data/italian-tagset.txt}
#'    and \url{http://sslmit.unibo.it/~baroni/collocazioni/itwac.tagset.txt}, respectively
#'  \item \code{"ru"} --- Russian, according to the MSD tagset by Sharoff, Kopotev, Erjavec, Feldman & Divjak (2008)
#' }
#' For the internal tokenizer a small subset of tags is also defined, available through \code{lang="kRp"}. If you don't know the language your text was written in, the function \code{\link[koRpus:guess.lang]{guess.lang}}
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
#' @author m.eik michalke \email{meik.michalke@@hhu.de}, support for Spanish contributed by Earl Brown \email{ekbrown@@ksu.edu}, support for Italian contributed by Alberto Mirisola.
#' @keywords misc
#' @seealso \code{\link[koRpus:get.kRp.env]{get.kRp.env}}
#' @references
#' Santorini, B. (1991). \emph{Part-of-Speech Tagging Guidelines for the Penn Treebank Project}.
#'    URL: \url{http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/data/Penn-Treebank-Tagset.pdf}
#'
#'  Schiller, A., Teufel, S., Stockert, C. & Thielen, C. (1995). \emph{Vorl\"aufge Guidelines f\"ur das Tagging deutscher Textcorpora mit STTS}.
#'    URL: \url{http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/data/stts_guide.pdf}
#'
#' Sharoff, S., Kopotev, M., Erjavec, T., Feldman, A. & Divjak, D. (2008). \emph{Designing and evaluating Russian tagsets}. In: Proc. LREC 2008, Marrakech.
#'    URL: \url{http://corpus.leeds.ac.uk/mocky/}
#'
#' @export
#' @rdname kRp.POS.tags
#' @examples
#' tags.de <- kRp.POS.tags("de")

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

# skeleton for further tag sets
#   # XX -- <language>
#   # see <reference/URL>
#   tag.class.def.words.XX <- matrix(c(
#     ), ncol=3, byrow=TRUE, dimnames=list(c(),c("tag","wclass","desc")))
#   tag.class.def.punct.XX <- matrix(c(
#     "$,", "comma", "Komma", # not in guidelines
#     "$(", "punctuation", "satzinterne Interpunktion" # not in guidelines
#     ), ncol=3, byrow=TRUE, dimnames=list(c(),c("tag","wclass","desc")))
#   tag.class.def.sentc.XX <- matrix(c(
#     "SENT", "fullstop", "satzbeendende Interpunktion" # not in guidelines
#     ), ncol=3, byrow=TRUE, dimnames=list(c(),c("tag","wclass","desc")))

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
  }
  
  if(isTRUE(list.classes) & !isTRUE(list.tags)){
    tag.definition <- unique(tag.definition[,"wclass"])
  } else {}
  if(isTRUE(list.tags)){
    tag.definition <- unique(tag.definition[,"tag"])
  } else {}

  return(tag.definition)
}
