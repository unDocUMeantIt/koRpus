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
    "it"="it"
  )
)

set.lang.support("treetag",
  list("it-utf8"=list(
    ## preset: "it-utf8"
    # tags "utf-8" encoded text files
    # Alberto Mirisola added this Italian section
    lang="it",
    encoding="UTF-8",
    preset=function(TT.cmd, TT.bin, TT.lib, unix.OS){
      if(isTRUE(unix.OS)){
        # preset for unix systems
        TT.abbrev    <- file.path(TT.lib, "italian-abbreviations")
        return(
          list(
            TT.tokenizer       = file.path(TT.cmd, "utf8-tokenize.perl"),
            TT.tagger         = file.path(TT.bin, "tree-tagger"),
            TT.abbrev         = TT.abbrev,
            TT.params         = file.path(TT.lib, "italian-utf8.par"),

            TT.tknz.opts.def    = c(),
            TT.tknz.opts       = paste("-a", TT.abbrev),
            TT.lookup.command    = c(),
            TT.filter.command    = c()
          )
        )
      } else {
        # preset for windows systems
        TT.abbrev    <- file.path(TT.lib, "italian-abbreviations")
        return(
          list(
            TT.tokenizer       = file.path(TT.cmd, "utf8-tokenize.perl"),
            TT.tagger         = file.path(TT.bin, "tree-tagger.exe"),
            TT.abbrev        = TT.abbrev,
            TT.params        = file.path(TT.lib, "italian-utf8.par"),

            TT.tknz.opts.def    = c(),
            TT.tknz.opts      = paste("-a", TT.abbrev),
            TT.lookup.command    = c(),
            TT.filter.command    = c()
          )
        )
      }
    }),
    "it"=list(
      # tags "latin1" encoded text files
      lang="it",
      encoding="Latin1",
      preset=function(TT.cmd, TT.bin, TT.lib, unix.OS){
        if(isTRUE(unix.OS)){
          # preset for unix systems
          TT.abbrev <- file.path(TT.lib, "italian-abbreviations")
          return(
            list(
              TT.tokenizer      = file.path(TT.cmd, "tokenize.pl"),
              TT.tagger        = file.path(TT.bin, "tree-tagger"),
              TT.abbrev        = TT.abbrev,
              TT.params        = file.path(TT.lib, "italian.par"),

              TT.tknz.opts.def    = c(),
              TT.tknz.opts      = paste("-a", TT.abbrev),
              TT.lookup.command    = c(),
              TT.filter.command    = c()
            )
          )
        } else {
          # preset for windows systems
          TT.abbrev <- file.path(TT.lib, "italian-abbreviations")
          return(
            list(
              TT.tokenizer      = file.path(TT.cmd, "tokenize.pl"),
              TT.tagger        = file.path(TT.bin, "tree-tagger.exe"),
              TT.abbrev        = TT.abbrev,
              TT.params        = file.path(TT.lib, "italian.par"),

              TT.tknz.opts.def    = c(),
              TT.tknz.opts      = paste("-a", TT.abbrev),
              TT.lookup.command    = c(),
              TT.filter.command    = c()
            )
          )
        }
      }
    )
  )
)

set.lang.support("kRp.POS.tags",
  ## tag and class definitions
  # it -- italian
  # see http://www.ims.uni-stuttgart.de/ftp/pub/corpora/italian-tagset.txt
  # and http://sslmit.unibo.it/~baroni/collocazioni/itwac.tagset.txt (alt. tagset)
  # Alberto Mirisola added the initial Italian tags
  list("it"=list(
    tag.class.def.words=matrix(c(
      "ABR", "abbreviation", "Abbreviation",
      "ADJ", "adjective", "Adjective",
      "ADV", "adverb", "Adverb",
      "ADV:mente", "adverb", "Adveb ending in -mente", # alt. tag set
      "ART", "article", "Article", # alt. tag set
      "ARTPRE", "preposition", "Preposition + article", # alt. tag set
      "AUX:fin", "auxiliary", "Auxiliary finite", # alt. tag set
      "AUX:fin:cli", "auxiliary", "Auxiliary finite with clitic", # alt. tag set
      "AUX:geru", "auxiliary", "Auxiliary gerundive", # alt. tag set
      "AUX:geru:cli", "auxiliary", "Auxiliary gerundive with clitic", # alt. tag set
      "AUX:infi", "auxiliary", "Auxiliary infinitive", # alt. tag set
      "AUX:infi:cli", "auxiliary", "Auxiliary infinitive with clitic", # alt. tag set
      "AUX:ppast", "auxiliary", "Auxiliary past participle", # alt. tag set
      "AUX:ppre", "auxiliary", "Auxiliary present participle", # alt. tag set
      "CHE", "che", "Che", # alt. tag set
      "CLI", "clitic", "Clitic", # alt. tag set
      "CON", "conjunction", "Conjunction",
      # "DET:def" and "DET:indef" appear in *both* tag sets, as
      # "article" in the default tag set and as "determiner" in the
      # alternative. set to the broader term "determiner":
      "DET:def", "determiner", "Definite determiner/article",
      "DET:indef", "determiner", "Indefinite determiner/article", # *both* tag sets, "article" in default tag set
      "DET:demo", "determiner", "Demonstrative determiner", # alt. tag set
      "DET:num", "determiner", "Numeral determiner", # alt. tag set
      "DET:poss", "determiner", "Possessive determiner", # alt. tag set
      "DET:wh", "determiner", "Wh determiner", # alt. tag set
      "INT", "interjection", "Interjection",
      "FW", "foreign", "Foreign word",
      "LS", "listmarker", "List item marker",
      "NEG", "negation", "Negation", # alt. tag set
      "NOCAT", "unknown", "Non-linguistic element", # alt. tag set
      "NOM", "noun", "Noun",
      "NOUN", "noun", "Noun", # alt. tag set
      "NPR", "name" ,"Proper noun",
      "NUM", "number", "Number",
      "ORD", "number", "Ordinal number",
      "PRE", "preposition", "Preposition",
      "PRE:det", "preposition", "Preposition + determiner/article",
      "PRO", "pronoun", "Pronoun",
      "PRO:demo", "pronoun", "Demonstrative pronoun",
      "PRO:indef", "pronoun", "Indefinite pronoun",
      "PRO:inter", "pronoun", "Interrogative pronoun",
      "PRO:num", "pronoun", "Numeral pronoun", # alt. tag set
      "PRO:pers", "pronoun", "Personal pronoun",
      "PRO:poss", "pronoun", "Possessive pronoun",
      "PRO:refl", "pronoun", "Reflexive pronoun",
      "PRO:rela", "pronoun", "Relative pronoun",
      "SYM", "symbol", "Symbol",
      "VER:cimp", "verb", "Verb conjunctive imperfect",
      "VER:cond", "verb", "Verb conditional",
      "VER:cpre", "verb", "Verb conjunctive present",
      "VER:fin", "verb", "Verb finite", # alt. tag set
      "VER:fin:cli", "verb", "Verb finite with clitic", # alt. tag set
      "VER:futu", "verb", "Verb future tense",
      "VER:geru", "verb", "Verb gerund",
      "VER:geru:cli", "verb", "Verb gerundive with clitic", # alt. tag set
      "VER:impe", "verb", "Verb imperative",
      "VER:impf", "verb", "Verb imperfect",
      "VER:infi", "verb", "Verb infinitive",
      "VER:infi:cli", "verb", "Verb infinitive with clitic", # alt. tag set
      "VER:ppast", "verb", "Verb past participle", # alt. tag set
      "VER:ppast:cli", "verb", "Verb past participle with clitic", # alt. tag set
      "VER:pper", "verb", "Verb participle perfect",
      "VER:ppre", "verb", "Verb participle present",
      "VER:pres", "verb", "Verb present",
      "VER:refl:infi", "verb", "Verb reflexive infinitive",
      "VER:remo", "verb", "Verb simple past",
      "VER2:fin", "verb", "Verb finite modal/causal", # alt. tag set
      "VER2:fin:cli", "verb", "Verb finite modal/causal with clitic", # alt. tag set
      "VER2:geru", "verb", "Verb gerundive modal/causal", # alt. tag set
      "VER2:geru:cli", "verb", "Verb gerundive modal/causal with clitic", # alt. tag set
      "VER2:infi", "verb", "Verb infinitive modal/causal", # alt. tag set
      "VER2:infi:cli", "verb", "Verb infinitive modal/causal with clitic", # alt. tag set
      "VER2:ppast", "verb", "Verb past participle modal/causal", # alt. tag set
      "VER2:ppre", "verb", "Verb present participle modal/causal", # alt. tag set
      "WH", "wh", "Wh word" # alt. tag set
      ), ncol = 3, byrow = TRUE, dimnames = list(c(), c("tag", "wclass", "desc"))),
    tag.class.def.punct=matrix(c(
      "PON", "punctuation", "Punctuation",
      "PUN", "punctuation", "Punctuation" # alt. tag set
      ), ncol = 3, byrow = TRUE, dimnames = list(c(), c("tag", "wclass", "desc"))),
    tag.class.def.sentc=matrix(c(
      "SENT", "fullstop", "Sentence ending punctuation"
      ), ncol = 3, byrow = TRUE, dimnames = list(c(), c("tag", "wclass", "desc")))
    )
  )
)

