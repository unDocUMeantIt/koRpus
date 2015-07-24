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
    "es"="es"
  )
)

set.lang.support("treetag",
  list("es-utf8"=list(
    ## preset: "es-utf8"
    # tags "utf-8" encoded text files
    # Earl Brown added this Spanish section
    lang="es",
    encoding="UTF-8",
    preset=function(TT.cmd, TT.bin, TT.lib, unix.OS){
      if(isTRUE(unix.OS)){
        # preset for unix systems
        TT.abbrev    <- file.path(TT.lib, "spanish-abbreviations")
        TT.lexicon    <- file.path(TT.lib, "spanish-mwls-utf8")
        TT.lookup    <- file.path(TT.cmd, "mwl-lookup.perl")
        return(
          list(
            TT.tokenizer       = file.path(TT.cmd, "utf8-tokenize.perl"),
            TT.tagger         = file.path(TT.bin, "tree-tagger"),
            TT.abbrev         = TT.abbrev,
            TT.params         = file.path(TT.lib, "spanish-utf8.par"),
            TT.lexicon         = TT.lexicon,
            TT.lookup         = TT.lookup,
            TT.filter         = c(),

            TT.tknz.opts.def    = c(),
            TT.tknz.opts       = paste("-a", TT.abbrev),
            TT.lookup.command    = paste(TT.lookup, "-f", TT.lexicon, "|"),
            TT.filter.command    = c()
          )
        )
      } else {
        # preset for windows systems
        TT.abbrev    <- file.path(TT.lib, "spanish-abbreviations")
        return(
          list(
            TT.tokenizer       = file.path(TT.cmd, "utf8-tokenize.perl"),
            TT.tagger         = file.path(TT.bin, "tree-tagger.exe"),
            TT.abbrev        = TT.abbrev,
            TT.params        = file.path(TT.lib, "spanish-utf8.par"),
            TT.lexicon        = c(),
            TT.lookup        = c(),
            TT.filter        = c(),
            TT.tknz.opts.def    = c(),
            TT.tknz.opts      = paste("-a", TT.abbrev),
            TT.lookup.command    = c(),
            TT.filter.command    = c()
          )
        )
      }
    }),
    "es"=list(
      # tags "latin1" encoded text files
      lang="es",
      encoding="Latin1",
      preset=function(TT.cmd, TT.bin, TT.lib, unix.OS){
        if(isTRUE(unix.OS)){
          # preset for unix systems
          TT.abbrev <- file.path(TT.lib, "spanish-abbreviations")
          TT.lexicon <- file.path(TT.lib, "spanish-mwls")
          TT.lookup <- file.path(TT.cmd, "mwl-lookup.perl")
          return(
            list(
              TT.tokenizer      = file.path(TT.cmd, "tokenize.pl"),
              TT.tagger        = file.path(TT.bin, "tree-tagger"),
              TT.abbrev        = TT.abbrev,
              TT.params        = file.path(TT.lib, "spanish.par"),
              TT.lexicon        = TT.lexicon,
              TT.lookup        = TT.lookup,
              TT.filter        = c(),
              TT.tknz.opts.def    = c(),
              TT.tknz.opts      = paste("-a", TT.abbrev),
              TT.lookup.command    = paste(TT.lookup, "-f", TT.lexicon, "|"),
              TT.filter.command    = c()
            )
          )
        } else {
          # preset for windows systems
          TT.abbrev <- file.path(TT.lib, "spanish-abbreviations")
          return(
            list(
              TT.tokenizer      = file.path(TT.cmd, "tokenize.pl"),
              TT.tagger        = file.path(TT.bin, "tree-tagger.exe"),
              TT.abbrev        = TT.abbrev,
              TT.params        = file.path(TT.lib, "spanish.par"),
              TT.lexicon        = c(),
              TT.filter        = c(),
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
  # es -- spanish
  # see http://www.ims.uni-stuttgart.de/ftp/pub/corpora/spanish-tagset.txt
  # Earl Brown added these Spanish tags
  list("es"=list(
    tag.class.def.words=matrix(c(
      "ACRNM", "acronym", "acronym (ISO, CEI)",
      "ADJ", "adjective", "Adjectives (mayores, mayor)",
      "ADV", "adverb", "Adverbs (muy, demasiado, c\u00f3mo)",
      "ALFP", "letter", "Plural letter of the alphabet (As/Aes, bes)",
      "ALFS", "letter", "Singular letter of the alphabet (A, b)",
      "ART", "determiner", "Articles (un, las, la, unas)",
      "CARD", "number", "Cardinals",
      "CC", "conjunction", "Coordinating conjunction (y, o)",
      "CCAD", "conjunction", "Adversative coordinating conjunction (pero)",
      "CCNEG", "conjunction", "Negative coordinating conjunction (ni)",
      "CODE", "code", "Alphanumeric code",
      "CQUE", "conjunction", "que (as conjunction)",
      "CSUBF", "conjunction", "Subordinating conjunction that introduces finite clauses (apenas)",
      "CSUBI", "conjunction", "Subordinating conjunction that introduces infinite clauses (al)",
      "CSUBX", "conjunction", "Subordinating conjunction underspecified for subord-type (aunque)",
      "DM", "determiner", "Demonstrative pronouns (\u00e9sas, \u00e9se, esta)",
      "FO", "formula", "Formula",
      "INT", "pronoun", "Interrogative pronouns (qui\u00e9nes, cu\u00e1ntas, cu\u00e1nto)",
      "ITJN", "interjection", "Interjection (oh, ja)",
      "NC", "noun", "Common nouns (mesas, mesa, libro, ordenador)",
      "NEG", "negation", "Negation",
      "NMEA", "noun", "measure noun (metros, litros)",
      "NMON", "noun", "month name",
      "NP", "noun", "Proper nouns",
      "ORD", "ordinal", "Ordinals (primer, primeras, primera)",
      "PAL", "al", "Portmanteau word formed by a and el",
      "PDEL", "del", "Portmanteau word formed by de and el",
      "PE", "foreign", "Foreign word",
      "PNC", "unknown", "Unclassified word",
      "PPC", "pronoun", "Clitic personal pronoun (le, les)",
      "PPO", "pronoun", "Possessive pronouns (mi, su, sus)",
      "PPX", "pronoun", "Clitics and personal pronouns (nos, me, nosotras, te, s\u00ed)",
  #    "PREP", "preposition", "Negative preposition (sin)",
      "PREP", "preposition", "Preposition",
      "PREP/DEL", "preposition", "Complex preposition \"despu\u00e9s del\"",
      "QU", "quantifier", "Quantifiers (sendas, cada)",
      "REL", "pronoun", "Relative pronouns (cuyas, cuyo)",
      "SE", "se", "Se (as particle)",
      "SYM", "symbol", "Symbols",
      "UMMX", "unit", "measure unit (MHz, km, mA)",
      "VCLIger", "verb", "clitic gerund verb",
      "VCLIinf", "verb", "clitic infinitive verb",
      "VCLIfin", "verb", "clitic finite verb",
      "VEadj", "verb", "Verb estar. Past participle",
      "VEfin", "verb", "Verb estar. Finite",
      "VEger", "verb", "Verb estar. Gerund",
      "VEinf", "verb", "Verb estar. Infinitive",
      "VHadj", "verb", "Verb haber. Past participle",
      "VHfin", "verb", "Verb haber. Finite",
      "VHger", "verb", "Verb haber. Gerund",
      "VHinf", "verb", "Verb haber. Infinitive",
      "VLadj", "verb", "Lexical verb. Past participle",
      "VLfin", "verb", "Lexical verb. Finite",
      "VLger", "verb", "Lexical verb. Gerund",
      "VLinf", "verb", "Lexical verb. Infinitive",
      "VMadj", "verb", "Modal verb. Past participle",
      "VMfin", "verb", "Modal verb. Finite",
      "VMger", "verb", "Modal verb. Gerund",
      "VMinf", "verb", "Modal verb. Infinitive",
      "VSadj", "verb", "Verb ser. Past participle",
      "VSfin", "verb", "Verb ser. Finite",
      "VSger", "verb", "Verb ser. Gerund",
      "VSinf", "verb", "Verb ser. Infinitive"
      ), ncol = 3, byrow = TRUE, dimnames = list(c(), c("tag", "wclass", "desc"))),
    tag.class.def.punct=matrix(c(
      "BACKSLASH", "punctuation", "backslash (\\) ",
      "CM", "comma", "comma (,)",
      "COLON", "punctuation", "colon (:)",
      "DASH", "punctuation", "dash (-)",
      "DOTS", "punctuation", "POS tag for \"...\"",
      "LP", "punctuation", "left parenthesis (\"(\", \"[\")",
      "PERCT", "punctuation", "percent sign (%)",
      "QT", "punctuation", "quotation symbol (\" \' `)",
      "RP", "punctuation", "right parenthesis (\")\", \"]\")",
      "SEMICOLON", "punctuation", "semicolon (;)",
      "SLASH", "punctuation", "slash (/)"
      ), ncol = 3, byrow = TRUE, dimnames = list(c(), c("tag", "wclass", "desc"))),
    tag.class.def.sentc=matrix(c(
      "FS", "fullstop", "Full stop punctuation marks"
      ), ncol = 3, byrow = TRUE, dimnames = list(c(), c("tag", "wclass", "desc")))
    )
  )
)
