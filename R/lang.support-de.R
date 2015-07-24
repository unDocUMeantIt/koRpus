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


# this is an internal file providing language support.
# please refer to inst/README.languages for details

#' @include set.lang.support.R

set.lang.support("hyphen",
  list(
    "de"="de",
    "de-1996"="de",
    "de.old"="de.old",
    "de-1901"="de.old"
  )
)

set.lang.support("treetag",
  list("de-utf8"=list(
    ## preset: "de-utf8"
    lang="de",
    encoding="UTF-8",
    preset=function(TT.cmd, TT.bin, TT.lib, unix.OS){
      if(isTRUE(unix.OS)){
        # preset for unix systems
        TT.abbrev <- file.path(TT.lib, "german-abbreviations-utf8")
        TT.lexicon <- file.path(TT.lib, "german-lexicon-utf8.txt")
        TT.lookup <- file.path(TT.cmd, "lookup.perl")
        TT.filter <- file.path(TT.cmd, "filter-german-tags")
        return(
          list(
            TT.tokenizer    = file.path(TT.cmd, "utf8-tokenize.perl"),
            TT.tagger      = file.path(TT.bin, "tree-tagger"),
            TT.abbrev      = TT.abbrev,
            TT.params      = file.path(TT.lib, "german-utf8.par"),
            TT.lexicon      = TT.lexicon,
            TT.lookup      = TT.lookup,
            TT.filter      = TT.filter,

            TT.tknz.opts.def  = c(),
            TT.tknz.opts    = paste("-a", TT.abbrev),
            TT.lookup.command  = paste("perl", TT.lookup, TT.lexicon, "|"),
            TT.filter.command  = paste("|", TT.filter)
          )
        )
      } else {
        # preset for windows systems
        TT.abbrev <- file.path(TT.lib, "german-abbreviations-utf8")
        return(
          list(
            TT.tokenizer    = file.path(TT.cmd, "utf8-tokenize.perl"),
            TT.tagger      = file.path(TT.bin, "tree-tagger.exe"),
            TT.abbrev      = TT.abbrev,
            TT.params      = file.path(TT.lib, "german-utf8.par"),
            TT.lexicon      = c(),
            TT.lookup      = c(),
            TT.filter      = c(),

            TT.tknz.opts.def  = c(),
            TT.tknz.opts    = paste("-a", TT.abbrev),
            TT.lookup.command  = c(),
            TT.filter.command  = c()
          )
        )
      }
    }),
  "de"=list(
    ## preset: "de"
    lang="de",
    encoding="Latin1",
    preset=function(TT.cmd, TT.bin, TT.lib, unix.OS){
      if(isTRUE(unix.OS)){
        # preset for unix systems
        TT.abbrev  <- file.path(TT.lib, "german-abbreviations")
        TT.lexicon  <- file.path(TT.lib, "german-lexicon.txt")
        TT.lookup  <- file.path(TT.cmd, "lookup.perl")
        TT.filter  <- file.path(TT.cmd, "filter-german-tags")
        return(
          list(
            TT.tokenizer    = file.path(TT.cmd, "tokenize.pl"),
            TT.tagger      = file.path(TT.bin, "tree-tagger"),
            TT.abbrev      = TT.abbrev,
            TT.params      = file.path(TT.lib, "german.par"),
            TT.lexicon      = TT.lexicon,
            TT.lookup      = TT.lookup,
            TT.filter      = TT.filter,

            TT.tknz.opts.def  = c(),
            TT.tknz.opts    = paste("-a", TT.abbrev),
            TT.lookup.command  = paste("perl", TT.lookup, TT.lexicon, "|"),
            TT.filter.command  = paste("|", TT.filter)
          )
        )
      } else {
        # preset for windows systems
        TT.abbrev  <- file.path(TT.lib, "german-abbreviations")
        return(
          list(
            TT.tokenizer    = file.path(TT.cmd, "tokenize.pl"),
            TT.tagger      = file.path(TT.bin, "tree-tagger.exe"),
            TT.abbrev      = TT.abbrev,
            TT.params      = file.path(TT.lib, "german.par"),
            TT.lexicon      = c(),
            TT.lookup      = c(),
            TT.filter      = c(),

            TT.tknz.opts.def  = c(),
            TT.tknz.opts    = paste("-a", TT.abbrev),
            TT.lookup.command  = c(),
            TT.filter.command  = c()
          )
        )
      }
    })
  )
)

set.lang.support("kRp.POS.tags",
  ## tag and class definitions
  # de -- german
  # see http://www.ims.uni-stuttgart.de/ftp/pub/corpora/stts_guide.pdf
  list("de"=list(
    tag.class.def.words=matrix(c(
      "ADJ", "adjective", "Adjektiv",
      "ADJA", "adjective", "attributives Adjektiv",
      "ADJD", "adjective", "adverbiales oder pr\u00e4dikatives Adjektiv",
      "ADV", "adverb", "Adverb",
      "AP", "adposition", "Adposition",
      "APPR", "preposition", "Pr\u00e4position; Zirkumposition links",
      "APPRART", "preposition", "Pr\u00e4position mit Artikel",
      "APPO", "postposition", "Postposition",
      "APZR", "circumposition", "Zirkumposition rechts",
      "ART", "article", "Artikel",
      "CARD", "number", "Kardinalzahl",
      "FM", "foreign", "fremdsprachliches Material",
      "ITJ", "interjection", "Interjektion",
      "KO", "conjunction", "Konjunktion",
      "KOUI", "conjunction", "unterordnende Konjunktion mit zu und Infinitiv",
      "KOUS", "conjunction", "unterordnende Konjunktion mit Satz",
      "KON", "conjunction", "nebenordnende Konjunktion",
      "KOKOM", "conjunction", "Vergleichspartikel ohne Satz",
      "NE", "name", "Eigenname",
      "NN", "noun", "Nomen",
      "P", "pronoun", "Pronomen",
      "PDS", "pronoun", "substituierendes Demonstrativpronomen",
      "PDAT", "pronoun", "attribuierendes Demonstrativpronomen",
      "PIS", "pronoun", "substituierendes Indefinitpronomen",
      "PIAT", "pronoun", "attribuierendes Indefinitpronomen ohne Determiner",
      "PIDAT", "pronoun", "attribuierendes Indefinitpronomen mit Determiner",
      "PPER", "pronoun", "irreflexibles Personalpronomen",
      "PPOSS", "pronoun", "substituierendes Possesivpronomen",
      "PPOSAT", "pronoun", "attribuierendes Possesivpronomen",
      "PRELS", "pronoun", "substituierendes Relativpronomen",
      "PRELAT", "pronoun", "attribuierendes Relativpronomen",
      "PRF", "pronoun", "reflexibles Personalpronomen",
      "PWS", "pronoun", "substituierendes Interrogativpronomen",
      "PWAT", "pronoun", "attribuierendes Interrogativpronomen",
      "PWAV", "pronoun", "adverbiales Interrogativ- oder Relativpronomen",
      "PAV", "pronoun", "Pronominaladverb",
      "PROAV", "pronoun", "Pronominaladverb", # this is actually a bug in earlier TreeTagger versions!
      "PTK", "particle", "Partikel",
      "PTKZU", "particle", "zu vor Infinitiv",
      "PTKNEG", "particle", "Negationspartikel",
      "PTKVZ", "particle", "abgetrennter Verbzusatz",
      "PTKANT", "particle", "Antwortpartikel",
      "PTKA", "particle", "Partikel bei Adjektiv oder Adverb",
      "TRUNC", "composition", "Kompositions-Erstglied",
      "V", "verb", "Verb",
      "VVFIN", "verb", "finites Verb, voll",
      "VVIMP", "verb", "Verb, Imperativ, voll",
      "VVINF", "verb", "Verb, Infinitiv, voll",
      "VVIZU", "verb", "Verb, Infinitiv mit zu, voll",
      "VVPP", "verb", "Verb, Partizip Perfekt, voll",
      "VAFIN", "verb", "finites Verb, aux",
      "VAIMP", "verb", "Verb, Imperativ, aux",
      "VAINF", "verb", "Verb, Infinitiv, aux",
      "VAPP", "verb", "Verb, Partizip Perfekt, aux",
      "VMFIN", "verb", "finites Verb, modal",
      "VMINF", "verb", "Verb, Infinitiv, modal",
      "VMPP", "verb", "Verb, Partizip Perfekt, modal",
      "XY", "nonword", "Nichtwort"
      ), ncol=3, byrow=TRUE, dimnames=list(c(),c("tag","wclass","desc"))),
    tag.class.def.punct=matrix(c(
      "$,", "comma", "Komma",
      "$(", "punctuation", "satzinterne Interpunktion"
      ), ncol=3, byrow=TRUE, dimnames=list(c(),c("tag","wclass","desc"))),
    tag.class.def.sentc=matrix(c(
      "$.", "fullstop", "satzbeendende Interpunktion"
      ), ncol=3, byrow=TRUE, dimnames=list(c(),c("tag","wclass","desc")))
    )
  )
)
