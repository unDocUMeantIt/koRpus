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

# test appending new POS tags
## this adds new tags globally, keep in mind when updating test standards!
context("language support")

test_that("merging new POS tags", {
  expect_true(
   all(!c("NNP", "NNPS", "PRP$") %in% kRp.POS.tags("xy", list.tags=TRUE))
  )

  set.lang.support("kRp.POS.tags",
    list("xy"=list(
      tag.class.def.words=matrix(c(
          "NNP", "name", "Proper noun, singular",
          "NNPS", "name", "Proper noun, plural",
          "PRP$", "pronoun", "Possessive pronoun"
      ), ncol=3, byrow=TRUE, dimnames=list(c(),c("tag","wclass","desc")))
    ))
  )

  expect_true(
   all(c("NNP", "NNPS", "PRP$") %in% kRp.POS.tags("xy", list.tags=TRUE))
  )
})


# testing basic tokenizing and POS tagging

context("environment")

test_that("setting environment variables", {
  # we cannot really test the treetag function without a local TreeTagger installation,
  # however, we can check if setting the environment works as expected
  set.kRp.env(TT.cmd="manual", lang="xy", TT.options=list(path=".", preset="xy"), validate=FALSE)

  expect_match(
    get.kRp.env(TT.cmd=TRUE),
    "manual"
  )
  expect_match(
    get.kRp.env(lang=TRUE),
    "xy"
  )
  expect_that(
    get.kRp.env(TT.options=TRUE),
    is_identical_to(list(path=".", preset="xy"))
  )
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
  expect_equal(
    tokenizedTextFile,
    sampleTextStandard
  )
  expect_equal(
    tokenizedTextFileNoDesc,
    sampleTextStandardNoDesc
  )
  expect_equal(
    tokenizedTextObj,
    sampleTextStandard
  )
  expect_equal(
    tokenizedTextConnection,
    sampleTextStandard
  )
  expect_equal(
    tokenizedToken,
    sampleTokenizedToken
  )
})

test_that("fixing old objects", {
  sampleTextFile <- normalizePath("sample_text.txt")
  expect_warning(
    sampleTextStandardOld <- fixObject(dget("sample_text_tokenized_dput_old.txt"))
  )

  tokenizedTextFile <- tokenize(
    sampleTextFile, lang="xy", stopwords=c("it's","one","for","you","and","me"), add.desc=TRUE)

  # we can't compare with "is_identical_to() because the percentages may slightly differ
  expect_equal(
    tokenizedTextFile,
    sampleTextStandardOld
  )
})

context("readTagged")

test_that("importing already tagged texts", {
  sampleTextFileTreeTagged <- normalizePath("sample_text_treetagged.txt")
  sampleTextFileRDRTagged <- dget("sample_text_RDRPOSTagged_df_dput.txt")
  sampleTextTreeTaggedStandard <- dget("sample_text_treetagged_dput.txt")
  sampleTextRDRTaggedStandard <- dget("sample_text_RDRPOSTagged_kRp_dput.txt")

  # running readTagged() on a character string already tests various
  # methods, because the string is made into a connection and that
  # in turn into a matrix, both times calling the appropriate readTagged()
  # methods internally
  treeTaggedText <- readTagged(sampleTextFileTreeTagged, lang="xy")

  
  RDRPOSTaggedText <- readTagged(
    sampleTextFileRDRTagged,
    lang="xy",
    doc_id="sampleText",
    tagger="manual",
    mtx_cols=c(token="token", tag="pos", lemma=NA)
  )
  
  expect_equal(
    treeTaggedText,
    sampleTextTreeTaggedStandard
  )

  expect_equal(
    RDRPOSTaggedText,
    sampleTextRDRTaggedStandard
  )
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

  expect_equal(
    lexdivTextObj,
    sampleTextStandard
  )
  expect_equal(
    TTRCharTextObj,
    sampleTextStandardTTRChar
  )
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

  expect_equal(
    hyphenTextObjNoCache,
    sampleTextStandard
  )
  expect_equal(
    hyphenTextObjCache,
    sampleTextStandard
  )
  expect_equal(
    hyphenTextObjChanged,
    sampleTextStandardChanged
  )
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

  expect_equal(
    readabilityTextObj,
    sampleTextStandard
  )
})


context("query")

test_that("query", {
  sampleTextTokenized <- dget("sample_text_tokenized_dput.txt")

  queryTokenThe <- query(sampleTextTokenized, "token", "the")
  expect_equal(
    nrow(queryTokenThe),
    38
  )

  queryLttrGe5 <- query(sampleTextTokenized, "lttr", 5, "ge")
  expect_equal(
    nrow(queryLttrGe5),
    290
  )
  expect_equal(
    sum(queryLttrGe5[["lttr"]]),
    2178
  )
  
  queryLttr6to9 <- query(sampleTextTokenized, "lttr", c(5, 10), "gt")
  expect_equal(
    nrow(queryLttr6to9),
    191
  )
  
  querySntc5 <- query(sampleTextTokenized, "sntc", 5)
  expect_equal(
    nrow(querySntc5),
    46
  )
  
  expect_error(
    query(sampleTextTokenized, "sntcs", 30)
  )

})


context("filterByClass")

test_that("filterByClass", {
  sampleTextTreeTagged <- dget("sample_text_treetagged_dput.txt")
  
  sampleTextNoPunct <- filterByClass(sampleTextTreeTagged)
  sampleTextNoNounsVerbs <- filterByClass(
    sampleTextTreeTagged,
    corp.rm.class=c("noun","verb")
  )
  sampleTextNoPossPron <- filterByClass(
    sampleTextTreeTagged,
    corp.rm.class=c(),
    corp.rm.tag="PP$"
  )

  expect_equal(
    nrow(taggedText(sampleTextNoPunct)),
    556 # vs. 617
  )
  
  expect_equal(
    nrow(taggedText(sampleTextNoPossPron)),
    608 # vs. 617
  )

  expect_equal(
    nrow(taggedText(sampleTextNoNounsVerbs)),
    358 # vs. 617
  )

  expect_equal(
    describe(sampleTextNoPunct)[["all.chars"]],
    3491 # vs. 3551
  )

  # the "punct" value is not counted by looking at the punctuation
  # tags -- which should all have been removed now --, but on a
  # character level. due to some punctuation not removed from tokens
  # by the TreeTagger tokenizer, there's still some residual left
  expect_equal(
    describe(sampleTextNoPunct)[["punct"]],
    17 # vs. 78
  )
  
})


context("pasteText")

test_that("pasteText", {
  sampleTextTreeTagged <- dget("sample_text_treetagged_dput.txt")
  sampleTextFile <- normalizePath("sample_text.txt")

  tokenizedTextFile <- tokenize(
    sampleTextFile,
    lang="xy",
    detect=c(
      parag=TRUE,
      hline=TRUE
    ),
    add.desc=TRUE
  )

  treeTaggedPasted <- pasteText(sampleTextTreeTagged)
  tokenizedPasted <- pasteText(tokenizedTextFile)

  expect_equal(
    nchar(treeTaggedPasted),
    3550
  )

  expect_equal(
    nchar(tokenizedPasted),
    3559
  )

  expect_true(
    all(
      c(21,22,1207,1208) %in% grep("\n", unlist(strsplit(tokenizedPasted, "")))
    )
  )
  
})


context("text transformation")

# a sample sentence for later tests, defined globally for performance reasons
tokenizedSentence <- tokenize(
  "The defense mechanism most readily identifiable with Phasmatodea is camouflage.",
  format="obj",
  lang="xy",
  add.desc=TRUE
)
  
test_that("textTransform", {
  transMinor <- textTransform(tokenizedSentence, scheme="minor")
  transMajor <- textTransform(tokenizedSentence, scheme="major")
  transAllMinor <- textTransform(tokenizedSentence, scheme="all.minor")
  transAllMajor <- textTransform(tokenizedSentence, scheme="all.major")
  transDENorm <- textTransform(tokenizedSentence, scheme="de.norm")
  transDEInv <- textTransform(tokenizedSentence, scheme="de.inv")
  transEUNorm <- textTransform(tokenizedSentence, scheme="eu.norm")
  transEUInv <- textTransform(tokenizedSentence, scheme="eu.inv")
  transRandom <- textTransform(tokenizedSentence, scheme="random")
  
  expect_equal(
    sum(taggedText(transMinor)[["lttr.diff"]]),
    2
  )
  expect_equal(
    sum(taggedText(transMajor)[["lttr.diff"]]),
    8
  )
  expect_equal(
    sum(taggedText(transAllMinor)[["lttr.diff"]]),
    2
  )
  expect_equal(
    sum(taggedText(transAllMajor)[["lttr.diff"]]),
    67
  )
  expect_equal(
    sum(taggedText(transDENorm)[["lttr.diff"]]),
    1
  )
  expect_equal(
    sum(taggedText(transDEInv)[["lttr.diff"]]),
    9
  )
  expect_equal(
    sum(taggedText(transEUNorm)[["lttr.diff"]]),
    1
  )
  expect_equal(
    sum(taggedText(transEUInv)[["lttr.diff"]]),
    9
  )
  expect_true(
    sum(taggedText(transRandom)[["lttr.diff"]]) > 0
  )
})

test_that("diffText", {
  transMult <- textTransform(tokenizedSentence, scheme="minor")
  transMult <- textTransform(transMult, scheme="major")
  transMult <- textTransform(transMult, scheme="random")

  expect_equal(
    diffText(transMult)[["transfmt"]],
    c("minor","major","random")
  )
})

test_that("jumbleWords", {
  transJumbled <- jumbleWords(tokenizedSentence)

  # it's hard to test the result properly, characters
  # are reandomly reordered. but let's assume that
  # very likely less than 6 tokens remained identical
  expect_true(
    sum(taggedText(transJumbled)[["equal"]]) < 6
  )
})

test_that("clozeDelete", {
  transCloze <- clozeDelete(tokenizedSentence)

  expect_equal(
    sum(taggedText(transCloze)[["lttr.diff"]]),
    17
  )
})

test_that("originalText", {
  transCloze <- clozeDelete(tokenizedSentence)

  expect_equal(
    originalText(transCloze),
    taggedText(tokenizedSentence)
  )
})
 
