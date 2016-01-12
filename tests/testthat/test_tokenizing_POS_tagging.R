# testing basic tokenizing and POS tagging

context("environment")

test_that("setting environment variables", {
  # we cannot really test the treetag function without a local TreeTagger installation,
  # however, we can check if setting the environment works as expected
  set.kRp.env(TT.cmd="manual", lang="en", TT.options=list(path=".", preset="en"), validate=FALSE)

  expect_that(get.kRp.env(TT.cmd=TRUE),
    matches("manual"))
  expect_that(get.kRp.env(lang=TRUE),
    matches("en"))
  expect_that(get.kRp.env(TT.options=TRUE),
    is_identical_to(list(path=".", preset="en")))
})


context("tokenizing")

test_that("basic tokenizing", {
  sampleTextFile <- normalizePath("sample_text.txt")
  sampleTextStandard <- dget("sample_text_tokenized_dput.txt")
  sampleTextObj <- readLines(sampleTextFile)

  # without a local TreeTagger installation, these tests will be limited
  # to what is possible with tokenize()
  tokenizedTextFile <- tokenize(
    sampleTextFile, lang="en", stopwords=c("it's","one","for","you","and","me"))
  tokenizedTextObj <- tokenize(
    sampleTextObj, format="obj", lang="en", stopwords=c("it's","one","for","you","and","me"))

  textToTag <- file(sampleTextFile)
  tokenizedTextConnection <- tokenize(
    textToTag, lang="en", stopwords=c("it's","one","for","you","and","me"))
  close(textToTag)

  # we can't compare with "is_identical_to() because the percentages may slightly differ
  expect_that(tokenizedTextFile,
    equals(sampleTextStandard))
  expect_that(tokenizedTextObj,
    equals(sampleTextStandard))
  expect_that(tokenizedTextConnection,
    equals(sampleTextStandard))
})

test_that("lexical diversity", {
  sampleTextTokenized <- dget("sample_text_tokenized_dput.txt")
  sampleTextStandard <- dget("sample_text_lexdiv_dput.txt")

  # don't test characteristics because it's such a drain on resources
  # the summary method does some rounding which should be robust enough
  # to replicate results on other machines
  lexdivTextObj <- summary(lex.div(sampleTextTokenized, char=NULL))

  expect_that(lexdivTextObj,
    equals(sampleTextStandard))
})

test_that("hyphenation/syllable count", {
  sampleTextTokenized <- dget("sample_text_tokenized_dput.txt")
  sampleTextStandard <- dget("sample_text_hyphen_dput.txt")

  hyphenTextObj <- hyphen(sampleTextTokenized)

  expect_that(hyphenTextObj,
    equals(sampleTextStandard))
})

test_that("readability", {
  pseudoWordList <- normalizePath("pseudo_word_list.txt")

  sampleTextTokenized <- dget("sample_text_tokenized_dput.txt")
  sampleTextHyphen <- dget("sample_text_hyphen_dput.txt")
  sampleTextStandard <- dget("sample_text_readability_dput.txt")

  # Coleman and Traenkle.Bailer will cause a warning because tokenize()
  # does no real POS tagging
  readabilityTextObj <- summary(readability(sampleTextTokenized,
    hyphen=sampleTextHyphen,
    index=c("all"),
    word.lists=list(
      Bormuth=pseudoWordList,
      Dale.Chall=pseudoWordList,
      Harris.Jacobson=pseudoWordList,
      Spache=pseudoWordList)), flat=TRUE)

  expect_that(readabilityTextObj,
    equals(sampleTextStandard))
})
