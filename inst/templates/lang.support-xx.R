## TEMPLATE FILE -- ADJUST TO YOUR LANGUAGE
##
## this template file should help you add new language support to the koRpus
## package. first of all, please read ?set.lang.support for an overview of
## what's needed. after that, you should carefully go through this template
## and adjust it to your needs.
## 
## throughout the template, there are some values you need to replace globally:
##   xyzedish: template name for the language (replace with "english", "dutch" etc.)
##   xx:       template name for the language abbreviation (replace with "en", "nl" etc.)
## 
## when you're done, remove this block ;-)

# Copyright 2010-2015 Meik Michalke <meik.michalke@hhu.de>
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


# this script is providing additional support for language "xx".
# please refer to ?set.lang.support for details

## a note on the use in packages:
# if you use this template as basis for a language package, please update the
# roxygen2 documentation notes:
#' Language support for xyzedish
#' 
#' This function adds support for xyzedish to the koRpus package. You should not
#' need to call it manually, as that is done automatically when this package is
#' loaded.
#' 
#' In particular, this function adds the following:
#' \itemize{
#'  \item \code{lang}: The additional language "xx" to be used with koRpus
#'  \item \code{hyphen}: An additional set of hyphenation patterns (see \code{\link[hyph.xx]{hyph.xx}})
#'  \item \code{treetag}: The additional presets "xx" and "xx-utf8", implemented according to the respective
#'    TreeTagger[1] script
#'  \item \code{POS tags}: An additional set of tags, implemented using the documentation for the corresponding
#'    TreeTagger parameter set[2]
#' }
#'
#' @references
#' [1] \url{http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/}
#'
#' [2] \url{http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/data/xyzedish-tagset.txt}
#' @export
# call this function to add further language support
## function lang.support.xx()
lang.support.xx <- function() {
  # first tell koRpus where to find hyphenation patterns (see ?set.lang.support for details)
  koRpus::set.lang.support(target="hyphen",
    value=list(
      "xx"=c("xx", package="koRpus.lang.xx")
      # if you want to use a custom hyphenation object called hyph.xx in the running session:
      #   "xx"=hyph.xx
      #
      # should your language need more than one pattern, just extend the list accordingly,
      # giving additional entries different names, e.g.:
      #   "xx.old"=c("xx.old", package="koRpus.lang.xx")
    )
  )


  # here you have to adjust the parameters according to the contents of the TreeTagger
  # scripts for your language (see ?set.lang.support for details)
  #  - if there's both UTF-8 and Latin1 scripts, add them both (as "xx-utf8" and "xx")
  #  - add both the unix and windows equivalents
  #  - if some setting is missing, just set it to an empty vector (c())
  koRpus::set.lang.support(target="treetag",
    value=list(
      "xx-utf8"=list(
        ## preset: "xx-utf8"
        # tags "utf-8" encoded text files
        lang      = "xx",
        encoding  = "UTF-8",
        preset    = function(TT.cmd, TT.bin, TT.lib, unix.OS){
          if(isTRUE(unix.OS)){
            # preset for unix systems

            # note: these objects are set here for convenience, the
            # actual important part is the return value below
            TT.abbrev   <- file.path(TT.lib, "xyzedish-abbreviations")
            TT.lexicon  <- file.path(TT.lib, "xyzedish-lexicon.txt")
            TT.lookup   <- file.path(TT.cmd, "lookup.perl")
            TT.filter   <- "perl -pe 's/\\tV[BDHV]/\\tVB/;s/IN\\/that/\\tIN/;'"
            return(
              list(
                # you should change these according to the TreeTagger script
                TT.tokenizer        = file.path(TT.cmd, "utf8-tokenize.perl"),
                TT.tagger           = file.path(TT.bin, "tree-tagger"),
                TT.abbrev           = TT.abbrev,
                TT.params           = file.path(TT.lib, "xyzedish-utf8.par"),
                TT.lexicon          = TT.lexicon,
                TT.lookup           = TT.lookup,
                TT.filter           = TT.filter,

                TT.tknz.opts.def    = c(),
                TT.tknz.opts        = paste("-a", TT.abbrev),
                TT.lookup.command   = paste("perl", TT.lookup, TT.lexicon, "|"),
                TT.filter.command   = paste("|", TT.filter)
              )
            )
          } else {
            # preset for windows systems
            TT.abbrev   <- file.path(TT.lib, "xyzedish-abbreviations")
            TT.filter   <- "perl -pe 's/\\tV[BDHV]/\\tVB/;s/IN\\/that/\\tIN/;'"
            return(
              list(
                TT.tokenizer        = file.path(TT.cmd, "utf8-tokenize.perl"),
                TT.tagger           = file.path(TT.bin, "tree-tagger.exe"),
                TT.abbrev           = TT.abbrev,
                TT.params           = file.path(TT.lib, "xyzedish-utf8.par"),
                TT.lexicon          = c(), # example for undefined values
                TT.lookup           = c(), # example for undefined values
                TT.filter           = TT.filter,

                TT.tknz.opts.def    = c(),
                TT.tknz.opts        = paste("-a", TT.abbrev),
                TT.lookup.command   = c(),
                TT.filter.command   = c()
              )
            )
          }
        }),
      "xx"=list(
        # tags "latin1" encoded text files
        lang      = "xx",
        encoding  = "Latin1",
        preset    = function(TT.cmd, TT.bin, TT.lib, unix.OS){
          if(isTRUE(unix.OS)){
            # preset for unix systems
            TT.abbrev   <- file.path(TT.lib, "xyzedish-abbreviations")
            TT.lexicon  <- file.path(TT.lib, "xyzedish-lexicon.txt")
            TT.lookup   <- file.path(TT.cmd, "lookup.perl")
            TT.filter   <- "perl -pe 's/\\tV[BDHV]/\\tVB/;s/IN\\/that/\\tIN/;'"
            return(
              list(
                TT.tokenizer        = file.path(TT.cmd, "tokenize.pl"),
                TT.tagger           = file.path(TT.bin, "tree-tagger"),
                TT.abbrev           = TT.abbrev,
                TT.params           = file.path(TT.lib, "xyzedish.par"),
                TT.lexicon          = TT.lexicon,
                TT.lookup           = TT.lookup,
                TT.filter           = TT.filter,

                TT.tknz.opts.def    = c(),
                TT.tknz.opts        = paste("-a", TT.abbrev),
                TT.lookup.command   = paste("perl", TT.lookup, TT.lexicon, "|"),
                TT.filter.command   = paste("|", TT.filter)
              )
            )
          } else {
            # preset for windows systems
            TT.abbrev   <- file.path(TT.lib, "xyzedish-abbreviations")
            TT.filter   <- "perl -pe 's/\\tV[BDHV]/\\tVB/;s/IN\\/that/\\tIN/;'"
            return(
              list(
                TT.tokenizer        = file.path(TT.cmd, "tokenize.pl"),
                TT.tagger           = file.path(TT.bin, "tree-tagger.exe"),
                TT.abbrev           = TT.abbrev,
                TT.params           = file.path(TT.lib, "xyzedish.par"),
                TT.lexicon          = c(),
                TT.lookup           = c(),
                TT.filter           = TT.filter,

                TT.tknz.opts.def    = c(),
                TT.tknz.opts        = paste("-a", TT.abbrev),
                TT.lookup.command   = c(),
                TT.filter.command   = c()
              )
            )
          }
        }
      )
    )
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
    )
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
# .onAttach <- function(...) {
#   lang.support.xx()
# }
