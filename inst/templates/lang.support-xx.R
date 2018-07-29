## TEMPLATE FILE -- ADJUST TO YOUR LANGUAGE
##
## this template file should help you add new language support to the koRpus
## package. first of all, please read ?set.lang.support for an overview of
## what's needed. after that, you should carefully go through this template
## and adjust it to your needs.
## 
## throughout the template, there are some values you need to replace globally:
##   Xyzedish: capitalized template name for the language (replace with "English", "Dutch" etc.)
##   xyzedish: template name for the language (replace with "english", "dutch" etc.)
##   xx:       template name for the language abbreviation (replace with "en", "nl" etc.)
## 
## when you're done, remove this block ;-)

# Copyright 2010-2018 Meik Michalke <meik.michalke@hhu.de>
#
# This file is part of the R package koRpus.lang.xx.
#
# koRpus.lang.xx is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# koRpus.lang.xx is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with koRpus.lang.xx.  If not, see <http://www.gnu.org/licenses/>.


# this script is providing additional support for language "xx".
# please refer to ?set.lang.support for details

## a note on the use in packages:
# if you use this template as basis for a language package, please update the
# roxygen2 documentation notes:
#' Language support for Xyzedish
#' 
#' This function adds support for Xyzedish to the koRpus package. You should not
#' need to call it manually, as that is done automatically when this package is
#' being loaded.
#' 
#' In particular, this function adds the following:
#' \itemize{
#'  \item \code{lang}: The additional language "xx" to be used with koRpus
#'  \item \code{treetag}: The additional preset "xx", implemented according to the respective TreeTagger[1] script
#'  \item \code{POS tags}: An additional set of tags, implemented using the documentation for the corresponding
#'    TreeTagger parameter set[2]
#' }
#' Hyphenation patterns are provided by means of the \code{\link[sylly.xx:hyph.support.xx]{sylly.xx}} package.
#'
#' @param ... Optional arguments for \code{\link[koRpus:set.lang.support]{set.lang.support}}.
#' @references
#' [1] \url{http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/}
#'
#' [2] \url{http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/data/xyzedish-tagset.txt}
#' @export
#' @importFrom koRpus set.lang.support
#' @examples
#' \dontrun{
#' lang.support.xx()
#' }
# call this function to add further language support
## function lang.support.xx()
lang.support.xx <- function(...) {
  # here you have to adjust the parameters according to the contents of the TreeTagger
  # scripts for your language (see ?set.lang.support for details)
  #  - UTF-8 scripts are the default in TreeTagger now, add them as "xx"
  #  - add both the unix and windows equivalents
  #  - if some setting is missing, just set it to an empty vector (c())
  koRpus::set.lang.support(target="treetag",
    value=list(
      "xx"=list(
        ## preset: "xx"
        # tags UTF-8 encoded text files
        lang      = "xx",
        encoding  = "UTF-8",
        preset    = function(TT.cmd, TT.bin, TT.lib, unix.OS){
          # note: these objects are set here for convenience, the
          # actual important part is the return value below
          TT.abbrev   <- file.path(TT.lib, "xyzedish-abbreviations")
          TT.lexicon  <- file.path(TT.lib, "xyzedish-lexicon.txt")
          TT.lookup   <- file.path(TT.cmd, "lookup.perl")
          TT.filter   <- "perl -pe 's/\\tV[BDHV]/\\tVB/;s/IN\\/that/\\tIN/;'"
          # generally, the parts below are combined in this order by treetag():
          # TT.splitter TT.splitter.opts TT.tokenizer TT.tknz.opts "|" TT.lookup.command TT.pre.tagger TT.tagger TT.opts TT.params TT.filter.command
          if(isTRUE(unix.OS)){
            # preset for unix systems
            return(
              list(
                # you should change these according to the TreeTagger script
                TT.splitter         = file.path(TT.cmd, "xyzedish-splitter.perl"),
                TT.splitter.opts    = paste("| sed \"s/\\([\\)\\\"\\'\\?\\!]\\)\\([\\.\\,\\;\\:]\\)/ \\1 \\2/g\" |"),
                TT.tokenizer        = file.path(TT.cmd, "utf8-tokenize.perl"),
                TT.tagger           = file.path(TT.bin, "tree-tagger"),
                TT.abbrev           = TT.abbrev,
                TT.params           = file.path(TT.lib, "xyzedish-utf8.par"),
                TT.lexicon          = TT.lexicon,
                TT.lookup           = TT.lookup,
                TT.filter           = TT.filter,

                TT.tknz.opts        = paste("-a", TT.abbrev),
                TT.lookup.command   = paste("perl", TT.lookup, TT.lexicon, "|"),
                TT.filter.command   = paste("|", TT.filter),
                TT.pre.tagger       = "grep -v '^$' |"
              )
            )
          } else {
            # preset for windows systems
            return(
              list(
                TT.splitter         = file.path(TT.cmd, "xyzedish-splitter.perl"),
                TT.splitter.opts    = paste("| sed \"s/\\([\\)\\\"\\'\\?\\!]\\)\\([\\.\\,\\;\\:]\\)/ \\1 \\2/g\" |"),
                TT.tokenizer        = file.path(TT.cmd, "utf8-tokenize.perl"),
                TT.tagger           = file.path(TT.bin, "tree-tagger.exe"),
                TT.abbrev           = TT.abbrev,
                TT.params           = file.path(TT.lib, "xyzedish-utf8.par"),
                TT.lexicon          = c(), # example for undefined values
                TT.lookup           = c(), # example for undefined values
                TT.filter           = TT.filter,

                TT.tknz.opts        = paste("-a", TT.abbrev),
                TT.lookup.command   = c(),
                TT.filter.command   = c(),
                TT.pre.tagger       = "grep -v '^$' |"
              )
            )
          }
        }
      )
    ),
    ...
  )


  # finally, add the POS tagset information (see ?set.lang.support for details)
  # the list is split into three parts, to be able to distinct between
  # words (including numbers etc.), normal punctuation and sentence ending punctuation.
  # this is mainly used for filtering purposes and statistics.
  # 
  # note that each tag must be defined by three values:
  #   - the original TreeTagger abbreviation
  #   - a global "word class" definition like "noun", "verb" etc.
  #   - a human readable explaination of the abbreviation
  koRpus::set.lang.support(target="kRp.POS.tags",
    ## tag and class definitions
    # xx -- xyzedish
    # see <ADD LINK TO TAGSET INFORMATION>
    value=list(
      "xx"=list(
        tag.class.def.words=matrix(c(
        # tag           class           meaning
          "ABR",        "abbreviation", "Abbreviation",
          "ADJ",        "adjective",    "Adjective",
          "ADV",        "adverb",       "Adverb",
          "CON",        "conjunction",  "Conjunction",
          "NOUN",       "noun",         "Noun",
          "NPR",        "name" ,        "Proper noun",
          "NUM",        "number",       "Number",
          "ORD",        "number",       "Ordinal number",
          "PRE",        "preposition",  "Preposition",
          "VER:cond",   "verb",         "Verb conditional",
          "VER:fin",    "verb",         "Verb finite"
          ), ncol = 3, byrow = TRUE, dimnames = list(c(), c("tag", "wclass", "desc"))),
        tag.class.def.punct=matrix(c(
        # tag           class           meaning
          "$,",         "comma",        "Comma",
          "PON",        "punctuation",  "Punctuation",
          "PUN",        "punctuation",  "Punctuation"
          ), ncol = 3, byrow = TRUE, dimnames = list(c(), c("tag", "wclass", "desc"))),
        tag.class.def.sentc=matrix(c(
        # tag           class           meaning
          "SENT",       "fullstop",     "Sentence ending punctuation"
          ), ncol = 3, byrow = TRUE, dimnames = list(c(), c("tag", "wclass", "desc")))
      )
    ),
    ...
  )
} ## end function lang.support.xx()


## a note on the use in packages:
# you can use the template to create language support packages, i.e.,
# load that package to add a new language to koRpus. in order for that to work
# the functions calls of the template must be executed when the package is
# loaded, after the koRpus environment was prepared. this can be achieved by
# putting the lang.support.xx() function directly inside an internal
# .onAttach() function:
#
# #' @importFrom sylly.xx hyph.support.xx
# .onAttach <- function(...) {
#   lang.support.xx()
#   sylly.xx::hyph.support.xx()
# }
