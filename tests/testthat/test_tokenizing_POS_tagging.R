# add support for pseudo language xyzedish to be independent from any actual language support in the package
# this is basically the english support with a different name
set.lang.support("treetag",
  list("xy"=list(
    ## preset: "xy"
    lang="xy",
    encoding="UTF-8",
    preset=function(TT.cmd, TT.bin, TT.lib, unix.OS){
      TT.abbrev       <- file.path(TT.lib, "english-abbreviations")
      TT.lexicon      <- file.path(TT.lib, "english-lexicon.txt")
      TT.filter       <- "perl -pe 's/\\tV[BDHV]/\\tVB/;s/IN\\/that/\\tIN/;'"
      TT.lookup       <- file.path(TT.cmd, "lookup.perl")
      # TT.tokenizer TT.tknz.opts "|" TT.lookup.command TT.tagger TT.opts TT.params TT.filter.command
      if(isTRUE(unix.OS)){
        # preset for unix systems
        return(
          list(
            TT.tokenizer      = file.path(TT.cmd, "utf8-tokenize.perl"),
            TT.tagger         = file.path(TT.bin, "tree-tagger"),
            TT.abbrev         = TT.abbrev,
            TT.params         = file.path(TT.lib, "english-utf8.par"),
            TT.lexicon        = TT.lexicon,
            TT.lookup         = TT.lookup,
            TT.filter         = TT.filter,

            TT.tknz.opts      = paste("-e"),
            TT.lookup.command = paste("perl", TT.lookup, TT.lexicon, "|"),
            TT.filter.command = paste("|", TT.filter),
            TT.pre.tagger     = "grep -v '^$' |"
          )
        )
      } else {
        # preset for windows systems
        return(
          list(
            TT.tokenizer      = file.path(TT.cmd, "utf8-tokenize.perl"),
            TT.tagger         = file.path(TT.bin, "tree-tagger.exe"),
            TT.abbrev         = TT.abbrev,
            TT.params         = file.path(TT.lib, "english-utf8.par"),
            TT.lexicon        = c(),
            TT.lookup         = c(),
            TT.filter         = TT.filter,

            TT.tknz.opts      = paste("-e -a", TT.abbrev),
            TT.lookup.command = c(),
            TT.filter.command = paste("|", TT.filter),
            TT.pre.tagger     = c()
          )
        )
      }
    })
  )
)

set.lang.support("kRp.POS.tags",
  ## tag and class definitions
  # xy -- xyzedish
  # see http://www.ims.uni-stuttgart.de/projekte/corplex/TreeTagger/Penn-Treebank-Tagset.pdf
  list("xy"=list(
    tag.class.def.words=matrix(c(
      "CC", "conjunction", "Coordinating conjunction",
      "CD", "number", "Cardinal number",
      "DT", "determiner", "Determiner",
      "EX", "existential", "Existential there",
      "FW", "foreign", "Foreign word",
      "IN", "preposition", "Preposition or subordinating conjunction",
      "IN/that", "preposition", "Preposition or subordinating conjunction",
      "JJ", "adjective", "Adjective",
      "JJR", "adjective", "Adjective, comparative",
      "JJS", "adjective", "Adjective, superlative",
      "LS", "listmarker", "List item marker",
      "MD", "modal", "Modal",
      "NN", "noun", "Noun, singular or mass",
      "NNS", "noun", "Noun, plural",
      "NP", "name", "Proper noun, singular",
      "NPS", "name", "Proper noun, plural",
      "NS", "noun", "Noun, plural", # undocumented, bug in parameter file?
      "PDT", "predeterminer", "Predeterminer",
      "POS", "possesive", "Possessive ending",
      "PP", "pronoun", "Personal pronoun",
      "PP$", "pronoun", "Possessive pronoun",
      "RB", "adverb", "Adverb",
      "RBR", "adverb", "Adverb, comparative",
      "RBS", "adverb", "Adverb, superlative",
      "RP", "particle", " Particle",
      "SYM", "symbol", "Symbol",
      "TO", "to", "to",
      "UH", "interjection", "Interjection",
      "VB", "verb", "Verb, base form of \"to be\"",
      "VBD", "verb", "Verb, past tense of \"to be\"",
      "VBG", "verb", "Verb, gerund or present participle of \"to be\"",
      "VBN", "verb", "Verb, past participle of \"to be\"",
      "VBP", "verb", "Verb, non-3rd person singular present of \"to be\"",
      "VBZ", "verb", "Verb, 3rd person singular present of \"to be\"",
      "VH", "verb", "Verb, base form of \"to have\"",
      "VHD", "verb", "Verb, past tense of \"to have\"",
      "VHG", "verb", "Verb, gerund or present participle of \"to have\"",
      "VHN", "verb", "Verb, past participle of \"to have\"",
      "VHP", "verb", "Verb, non-3rd person singular present of \"to have\"",
      "VHZ", "verb", "Verb, 3rd person singular present of \"to have\"",
      "VV", "verb", "Verb, base form",
      "VVD", "verb", "Verb, past tense",
      "VVG", "verb", "Verb, gerund or present participle",
      "VVN", "verb", "Verb, past participle",
      "VVP", "verb", "Verb, non-3rd person singular present",
      "VVZ", "verb", "Verb, 3rd person singular present",
      "WDT", "determiner", "Wh-determiner",
      "WP", "pronoun", "Wh-pronoun",
      "WP$", "pronoun", "Possessive wh-pronoun",
      "WRB", "adverb", "Wh-adverb"
      ), ncol=3, byrow=TRUE, dimnames=list(c(),c("tag","wclass","desc"))),
    tag.class.def.punct=matrix(c(
      ",", "comma", "Comma", # not in guidelines
      "(", "punctuation", "Opening bracket", # not in guidelines
      ")", "punctuation", "Closing bracket", # not in guidelines
      ":", "punctuation", "Punctuation", # not in guidelines
      "``", "punctuation", "Quote", # not in guidelines
      "''", "punctuation", "End quote", # not in guidelines
      "#", "punctuation", "Punctuation", # not in guidelines
      "$", "punctuation", "Punctuation" # not in guidelines
      ), ncol=3, byrow=TRUE, dimnames=list(c(),c("tag","wclass","desc"))),
    tag.class.def.sentc=matrix(c(
      "SENT", "fullstop", "Sentence ending punctuation" # not in guidelines
      ), ncol=3, byrow=TRUE, dimnames=list(c(),c("tag","wclass","desc")))
    )
  )
)

# hyph_xy_dput.txt is just a small pattern sample, no more than POC
# the results are completely useless, except for testing the functionality
# so don't freak out when you look at them
samplePatternStandard <- dget("hyph_xy_dput.txt")
set.hyph.support(list("xy"=samplePatternStandard))

# testing basic tokenizing and POS tagging

context("environment")

test_that("setting environment variables", {
  # we cannot really test the treetag function without a local TreeTagger installation,
  # however, we can check if setting the environment works as expected
  set.kRp.env(TT.cmd="manual", lang="xy", TT.options=list(path=".", preset="xy"), validate=FALSE)

  expect_match(get.kRp.env(TT.cmd=TRUE),
    "manual")
  expect_match(get.kRp.env(lang=TRUE),
    "xy")
  expect_that(get.kRp.env(TT.options=TRUE),
    is_identical_to(list(path=".", preset="xy")))
})


context("tokenizing")

test_that("basic tokenizing", {
  sampleTextFile <- normalizePath("sample_text.txt")
  sampleTextStandard <- dget("sample_text_tokenized_dput.txt")
  sampleTextStandardNoDesc <- dget("sample_text_tokenized_no_desc_dput.txt")
  sampleTextObj <- readLines(sampleTextFile)
  sampleTokenizedToken <- dget("tokenized_single_token_dput.txt")

  # without a local TreeTagger installation, these tests will be limited
  # to what is possible with tokenize()
  tokenizedTextFile <- tokenize(
    sampleTextFile, lang="xy", stopwords=c("it's","one","for","you","and","me"), add.desc=TRUE)
  tokenizedTextFileNoDesc <- tokenize(
    sampleTextFile, lang="xy", stopwords=c("it's","one","for","you","and","me"), add.desc=FALSE)
  tokenizedTextObj <- tokenize(
    sampleTextObj, format="obj", lang="xy", stopwords=c("it's","one","for","you","and","me"), add.desc=TRUE)

  textToTag <- file(sampleTextFile)
  tokenizedTextConnection <- tokenize(
    textToTag, lang="xy", stopwords=c("it's","one","for","you","and","me"), add.desc=TRUE)
  close(textToTag)

  # this was fixed in koRpus 0.06-4, checking it's still working
  tokenizedToken <- tokenize("singleton", format="obj", lang="xy", add.desc=TRUE)

  # we can't compare with "is_identical_to() because the percentages may slightly differ
  expect_equal(tokenizedTextFile,
    sampleTextStandard)
  expect_equal(tokenizedTextFileNoDesc,
    sampleTextStandardNoDesc)
  expect_equal(tokenizedTextObj,
    sampleTextStandard)
  expect_equal(tokenizedTextConnection,
    sampleTextStandard)
  expect_equal(tokenizedToken,
    sampleTokenizedToken)
})

test_that("fixing old objects", {
  sampleTextFile <- normalizePath("sample_text.txt")
  expect_warning(
    sampleTextStandardOld <- fixObject(dget("sample_text_tokenized_dput_old.txt"))
  )

  tokenizedTextFile <- tokenize(
    sampleTextFile, lang="xy", stopwords=c("it's","one","for","you","and","me"), add.desc=TRUE)

  # we can't compare with "is_identical_to() because the percentages may slightly differ
  expect_equal(tokenizedTextFile,
    sampleTextStandardOld)
})


context("lexical diversity")

test_that("lexical diversity", {
  sampleTextTokenized <- dget("sample_text_tokenized_dput.txt")
  sampleTextStandard <- dget("sample_text_lexdiv_dput.txt")
  sampleTextStandardTTRChar <- dget("sample_text_TTRChar_dput.txt")

  # the summary method does some rounding which should be robust enough
  # to replicate results on other machines
  lexdivTextObj <- summary(lex.div(sampleTextTokenized, char=NULL, quiet=TRUE))
  TTRCharTextObj <- slot(TTR(sampleTextTokenized, char=TRUE, quiet=TRUE), "TTR.char")

  expect_equal(lexdivTextObj,
    sampleTextStandard)
  expect_equal(TTRCharTextObj,
    sampleTextStandardTTRChar)
})


context("hyphenation/syllable count")

test_that("hyphenation/syllable count", {
  sampleTextTokenized <- dget("sample_text_tokenized_dput.txt")
  sampleTextStandard <- dget("sample_text_hyphen_dput.txt")
  sampleTextStandardChanged <- dget("sample_text_correcthyph_dput.txt")

  hyphenTextObjNoCache <- hyphen(
    sampleTextTokenized,
    hyph.pattern=samplePatternStandard,
    cache=FALSE,
    quiet=TRUE
  )
  hyphenTextObjCache <- hyphen(
    sampleTextTokenized,
    hyph.pattern=samplePatternStandard,
    quiet=TRUE
  )
  # chcanging hyphenation
  hyphenTextObjChanged <- correct.hyph(hyphenTextObjCache, "Papua", "Pa-pu-a")
  hyphenTextObjChanged <- correct.hyph(hyphenTextObjChanged, "in-edible", "inedible")

  expect_equal(hyphenTextObjNoCache,
    sampleTextStandard)
  expect_equal(hyphenTextObjCache,
    sampleTextStandard)
  expect_equal(hyphenTextObjChanged,
    sampleTextStandardChanged)
})


context("readability")

test_that("readability", {
  pseudoWordList <- normalizePath("pseudo_word_list.txt")

  sampleTextTokenized <- dget("sample_text_tokenized_dput.txt")
  sampleTextHyphen <- dget("sample_text_hyphen_dput.txt")
  sampleTextStandard <- dget("sample_text_readability_dput.txt")

  # Coleman and Traenkle.Bailer will cause a warning because tokenize()
  # does no real POS tagging
  expect_warning(
    readabilityTextObj <- summary(readability(sampleTextTokenized,
      hyphen=sampleTextHyphen,
      index=c("all"),
      word.lists=list(
        Bormuth=pseudoWordList,
        Dale.Chall=pseudoWordList,
        Harris.Jacobson=pseudoWordList,
        Spache=pseudoWordList)), flat=TRUE)
  )

  expect_equal(readabilityTextObj,
    sampleTextStandard)
})
