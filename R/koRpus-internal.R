# Copyright 2010-2018 Meik Michalke <meik.michalke@hhu.de>
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


# these are internal functions that are being called by some of the methods of koRpus
# they are not exported, hence not to be called by users themselves
# and are therefore only documented by the comments in this file.

# define class union to make life easier
#' @include 01_class_01_kRp.tagged.R
#' @include 01_class_02_kRp.TTR.R
#' @include 01_class_03_kRp.txt.freq.R
#' @include 01_class_04_kRp.txt.trans.R
#' @include 01_class_05_kRp.analysis.R
#' @include 01_class_06_kRp.corp.freq.R
#' @include 01_class_09_kRp.lang.R
#' @include 01_class_10_kRp.readability.R
#' @include kRp.filter.wclass.R
setClassUnion("kRp.taggedText", members=c("kRp.tagged", "kRp.analysis", "kRp.txt.freq", "kRp.txt.trans"))


## function check.file()
# helper function for file checks
check.file <- function(filename, mode="exist", stopOnFail=TRUE){

  ret.value <- FALSE

  if(identical(mode, "exist") | identical(mode, "exec")){
    if(as.logical(file_test("-f", filename))){
      ret.value <- TRUE
    } else {
      if(isTRUE(stopOnFail)){
        stop(simpleError(paste("Specified file cannot be found:\n", filename)))
      } else {}
      ret.value <- FALSE
    }
  } else {}

  if(identical(mode, "exec")){
    if(as.logical(file_test("-x", filename))){
      ret.value <- TRUE
    } else {
      if(isTRUE(stopOnFail)){
        stop(simpleError(paste("Specified file cannot be executed:\n", filename)))
      } else {}
      ret.value <- FALSE
    }
  } else {}

  if(identical(mode, "dir")){
    if(as.logical(file_test("-d", filename))){
      ret.value <- TRUE
    } else {
      if(isTRUE(stopOnFail)){
        stop(simpleError(paste("Specified directory cannot be found:\n", filename)))
      } else {}
      ret.value <- FALSE
    }
  } else {}

  return(ret.value)
} ## end function check.file()


## function tag.kRp.txt()
# this function takes normal text OR objects of koRpus classes which carry the
# original information of the analyzed text somewhere, and
# tries to return a valid object of class kRp.tagged instead
# '...' will be passed through to treetag() or tokenize()
tag.kRp.txt <- function(txt, tagger=NULL, lang, objects.only=TRUE, ...){

  if(inherits(txt, "kRp.tagged")){
    return(as(txt, "kRp.tagged"))
  } else {}

  if(isTRUE(objects.only)){
    stop(simpleError(paste("Not a valid class for tag.kRp.txt():", class(txt)[1])))
  } else {
    if(is.character(txt)){
      # set the language definition
      if(is.null(lang)){
        lang <- get.kRp.env(lang=TRUE)
      } else {
        ## TODO: add some validation here
      }
      internal.tokenizer <- FALSE
      # set a fallback if tagger isn't set but objects.only=FALSE
      if(is.null(tagger)){
        if(is.null(get.kRp.env(TT.cmd=TRUE, errorIfUnset=FALSE))){
          message("No TreeTagger command specified. Using tokenize() as fallback")
          internal.tokenizer <- TRUE
        } else {
          tagger <- "kRp.env"
        }
      } else {}
      if(identical(tagger, "kRp.env")){
        if(identical(get.kRp.env(TT.cmd=TRUE), "tokenize")){
          internal.tokenizer <- TRUE
        } else {}
      } else {}
      if(identical(tagger, "tokenize") | isTRUE(internal.tokenizer)){
        tagged.txt <- tokenize(txt, tag=TRUE, lang=lang, ...)
      } else {
        tagged.txt <- treetag(txt, treetagger=tagger, lang=lang, ...)
      }
      return(tagged.txt)
    } else {
      stop(simpleError("Text object is neither of class kRp.analysis, kRp.tagged nor character!"))
    }
  }
} ## end function tag.kRp.txt()


## function basic.text.descriptives()
# txt must be a character vector
basic.text.descriptives <- function(txt){
  # number of characters, including spaces and punctuation
  # the following vector counts chars per line
  vct.all.chars <- nchar(txt)
  num.lines <- length(vct.all.chars)
  # now count each cluster of spaces as only one space
  txt.trans <- gsub("[[:space:]]+", " ", paste(txt, collapse="\n"))
  onespace.chars <- nchar(txt.trans)
  # count without any spaces
  txt.trans <- gsub("[[:space:]]", "", txt.trans)
  nospace.chars <- nchar(txt.trans)
  # count without any punctuation, i.e. only letters and digits
  txt.trans <- gsub("[^\\p{L}\\p{M}\\p{N}]", "", txt.trans, perl=TRUE)
  nopunct.chars <- nchar(txt.trans)
  num.punc <- nospace.chars - nopunct.chars
  # fially, get rid of digits as well
  txt.trans <- gsub("[\\p{N}+]", "", txt.trans, perl=TRUE)
  only.letters <- nchar(txt.trans)
  num.digits <- nopunct.chars - only.letters

  results <- list(
    all.chars=sum(vct.all.chars) + num.lines,
    lines=num.lines,
    normalized.space=onespace.chars,
    chars.no.space=nospace.chars,
    punct=num.punc,
    digits=num.digits,
    letters=only.letters
  )
  return(results)
} ## end function basic.text.descriptives()


## function basic.tagged.descriptives()
# txt must be an object of class kRp.tagged
basic.tagged.descriptives <- function(txt, lang=NULL, desc=NULL, txt.vector=NULL, update.desc=FALSE, doc_id=NA){
  if(is.null(lang)){
    lang <- txt@lang
  } else {}
  # create desc if not present
  if(!is.null(txt.vector) && is.null(desc)){
    desc <- basic.text.descriptives(txt.vector)
  }
  # count sentences
  txt.stend.tags <- kRp.POS.tags(lang, list.tags=TRUE, tags="sentc")
  txt.stend <- count.sentences(txt@TT.res, txt.stend.tags)
  # count words
  txt.nopunct <- kRp.filter.wclass(txt, corp.rm.class="nonpunct", corp.rm.tag=c(), as.vector=FALSE)
  num.words <- nrow(txt.nopunct@TT.res)
  avg.sentc.length <- num.words / txt.stend

  # character distribution
  char.distrib <- value.distribs(txt@TT.res[["lttr"]], omit.missings=FALSE)
  lttr.distrib <- value.distribs(txt.nopunct@TT.res[["lttr"]], omit.missings=FALSE)

  # txt.desc$letters had all digits removed
  # we'll use these numbers as they are usually more exact than relying on correct tokenization
   if("letters.only" %in% names(desc)){
     num.letters <- desc$letters.only + desc$digits
   } else {
    num.letters <- desc$letters + desc$digits
    # for readability calculations
    desc$letters.only <- desc$letters
    desc$letters <- distrib.to.fixed(lttr.distrib, all.values=num.letters, idx="l")
   }
  avg.word.length <- num.letters / num.words

  if(isTRUE(update.desc)){
    desc[["char.distrib"]] <- char.distrib
    desc[["lttr.distrib"]] <- lttr.distrib
    desc[["words"]] <- num.words
    desc[["sentences"]] <- txt.stend
    desc[["avg.sentc.length"]] <- avg.sentc.length
    desc[["avg.word.length"]] <- avg.word.length
    results <- desc
  } else {
    results <- append(
      desc,
      list(
        char.distrib=char.distrib,
        lttr.distrib=lttr.distrib,
        words=num.words,
        sentences=txt.stend,
        avg.sentc.length=avg.sentc.length,
        avg.word.length=avg.word.length
      )
    )
  }
  results[["doc_id"]] <- doc_id
  return(results)
} ## end function basic.tagged.descriptives()


## function clean.text()
# takes a character vector and a named list. it replaces each occurance
# of the list names by ist value and returns the changed vector
# e.g., use list("\"  "="\"") to remove two spaces behind double quotes
clean.text <- function(txt.vct, from.to=NULL, perl=FALSE){
  if(is.null(from.to)){
    return(txt.vct)
  } else {}
  stopifnot(is.character(txt.vct))
  stopifnot(is.list(from.to))
  for (idx in seq_along(from.to)){
      from <- names(from.to)[[idx]]
      to <- from.to[[idx]]
      txt.vct <- gsub(from, to, txt.vct, perl=perl)
      rm("from", "to")
    }
  return(txt.vct)
} ## end function clean.text()


## function language.setting()
language.setting <- function(tagged.object, force.lang){
  stopifnot(inherits(tagged.object, "kRp.tagged"))
  if(is.null(force.lang)){
    lang <- tagged.object@lang
  } else {
    lang <- is.supported.lang(lang.ident=force.lang)
  }
  return(lang)
} ## end function language.setting()


## function validate_tags()
# takes a character vector of POS tags and a language identifier
# checks all tags for validity
validate_tags <- function(tags, lang){
  # get all valid tags
  tag.class.def <- kRp.POS.tags(lang)

  # only proceed if all tag values are valid
  all.found.tags <- unique(tags)
  invalid.found.tags <- all.found.tags[!all.found.tags %in% tag.class.def[,"tag"]]
  if(length(invalid.found.tags) > 0){
    warning(paste0("Invalid tag(s) found: ", paste(invalid.found.tags, collapse = ", "),
      "\n  This is probably due to a missing tag in kRp.POS.tags() and",
      "\n  needs to be fixed. It would be nice if you could forward the",
      "\n  above warning dump as a bug report to the package maintaner!\n"), 
      call.=FALSE
    )
  } else {}
  # return a logical vector
  result <- !tags %in% invalid.found.tags
  return(result)
}
## end function validate_tags()


## function explain_tags()
# takes a character vector of POS tags and a language identifier
# returns either an equivalent character vector with explainations of each tag,
# or a matrix if 'cols' is > 1
explain_tags <- function(tags, lang, cols=c("wclass","desc"), valid=rep(TRUE, length(tags))){
  # get all valid tags
  tag.class.def <- kRp.POS.tags(lang)
  tags_explained <- sapply(
    seq_along(tags),
    function(tagNum){
      if(isTRUE(valid[tagNum])){
        return(tag.class.def[tag.class.def[,"tag"] == tags[tagNum], cols])
      } else {
        return(c(wclass="unknown",desc="Unknown (kRp internal)")[cols])
      }
    },
    USE.NAMES=FALSE
  )
  if(length(cols) > 1){
   tags_explained <- as.matrix(t(tags_explained))
  } else {
   tags_explained <- as.character(tags_explained)
  }
  return(tags_explained)
}
## end function explain_tags()


## function treetag.com()
treetag.com <- function(tagged.text, lang, add.desc=TRUE){
  tagged.text <- as.data.frame(tagged.text, row.names=1:nrow(tagged.text), stringsAsFactors=FALSE)
  # initialize empty target data.frame to define the order of columns
  newDf <- init.kRp.tagged.df(rows=nrow(tagged.text))
  newDf[,c("token","lemma")] <- tagged.text[,c("token","lemma")]

  valid_tags <- validate_tags(tags=tagged.text[["tag"]], lang=lang)
  # get all valid tags
  tag.class.def <- kRp.POS.tags(lang)
 
  # make tag a factor with all possible tags for this language as levels
  newDf[["tag"]] <- factor(
    tagged.text[["tag"]],
    # keep invalid tags for debugging
    levels=unique(c(tag.class.def[,"tag"], tagged.text[!valid_tags,"tag"]))
  )
  # count number of letters, add column "lttr"
  newDf[["lttr"]] <- as.numeric(nchar(tagged.text[,"token"]))

  # add further columns "wclass" and "desc"
  if(isTRUE(add.desc)){
    tagsExplained <- explain_tags(tags=tagged.text[["tag"]], lang=lang, cols=c("wclass","desc"), valid=valid_tags)
    tagsExplained.wclass <- factor(
      tagsExplained[,"wclass"],
      levels=unique(tag.class.def[,"wclass"])
    )
    tagsExplained.desc <- factor(
      tagsExplained[,"desc"],
      levels=unique(tag.class.def[,"desc"])
    )
  } else {
    tagsExplained.wclass <- factor(
      explain_tags(tags=tagged.text[["tag"]], lang=lang, cols="wclass", valid=valid_tags),
      levels=unique(tag.class.def[,"wclass"])
    )
    tagsExplained.desc <- NA
  }

  newDf[["wclass"]] <- tagsExplained.wclass
  newDf[["desc"]] <- tagsExplained.desc
    
  return(newDf)
} ## end function treetag.com()


## function dumpTextToTempfile()
# text: either plain text vector or a connection (where its content needs to be dumped to a file)
# encoding: output file encoding (will default to UTF-8 if NULL)
# pattern: tempfile name pattern
# fileext: tempfile name extension
dumpTextToTempfile <- function(text, encoding=NULL, pattern="tempTextFromObject", fileext=".txt"){
  conn.tempfile.path <- tempfile(pattern=pattern, fileext=fileext)
  # encoding can always become an issue; try to stick to UTF-8 if no other encoding was specified
  if(is.null(encoding)){
    conn.tempfile <- file(conn.tempfile.path, open="w", encoding="UTF-8")
  } else {
    conn.tempfile <- file(conn.tempfile.path, open="w", encoding=encoding)
  }
  on.exit(close(conn.tempfile))
  if(inherits(text, "connection")){
    writeLines(readLines(text, encoding=ifelse(is.null(encoding), "", encoding)), con=conn.tempfile)
  } else {
    writeLines(text, con=conn.tempfile)
  }
  return(conn.tempfile.path)
} ## end function dumpTextToTempfile()


## function stopAndStem()
# tagged.text is a data.frame from treetag() or tokenize(), to become TT.res
stopAndStem <- function(tagged.text.df, stopwords=NULL, stemmer=NULL, lowercase=TRUE){

  if(!is.null(stopwords)){
    if(!is.character(stopwords)){
      stop(simpleError("Stopwords must be specified as a character vector!"))
    } else {}
    # treat all tokens and stopwords in lower case?
    if(isTRUE(lowercase)){
      this.token <- tolower(tagged.text.df[["token"]])
      stopwords <- tolower(stopwords)
    } else {
      this.token <- tagged.text.df[["token"]]
    }
    # check if token is a stopword, add column "lttr"
    tagged.text.df[["stop"]] <- this.token %in% stopwords
  } else {
    tagged.text.df[["stop"]] <- NA
  }

  # check for stemming
  if(inherits(stemmer, "R_Weka_stemmer_interface") || is.function(stemmer)){
    tagged.text.df[["stem"]] <- stemmer(tagged.text.df[,"token"])
  } else {
    tagged.text.df[["stem"]] <- NA
  }

  return(tagged.text.df)
} ## end function stopAndStem()


## function indexSentenceDoc()
# after stopAndStem(), add columns "idx", "sntc" and "doc_id" to data.frame
indexSentenceDoc <- function(tagged.text.df, lang, doc_id=NA){
  numTokens <- nrow(tagged.text.df)
  tagged.text.df[["idx"]] <- 1:numTokens
  sentenceEnding <- kRp.POS.tags(lang=lang, tags=c("sentc"), list.classes=TRUE)
  endedSentences <- which(tagged.text.df[["wclass"]] %in% sentenceEnding)
  if(length(endedSentences) > 0){
    # handle texts that don't end with a sentence ending
    ## TODO: be smarter here -- if the sentence is a quote, the closing quote comes *after* the fullstop
    ## we'll just ignore this for now!
    if(endedSentences[length(endedSentences)] < numTokens){
      endedSentences[length(endedSentences)] <- numTokens
    } else {}
    tagged.text.df[["sntc"]] <- unlist(sapply(
      seq_along(endedSentences),
      function(numSentence){
        if(numSentence > 1){
          return(rep(numSentence, endedSentences[numSentence] - endedSentences[numSentence - 1]))
        } else {
          return(rep(numSentence, endedSentences[numSentence]))
        }
      },
      simplify=FALSE
    ))
  } else {
    tagged.text.df[["sntc"]] <- NA
  }
  tagged.text.df[["doc_id"]] <- factor(doc_id)
  return(tagged.text.df)
} ## end function indexSentenceDoc()


## function tagged.txt.rm.classes()
# takes a tagged text object and returns it without punctuation or other defined
# classes or tags. can also return tokens in lemmatized form.
# NOTE: "lemma" only takes effect if "as.vector=TRUE"!
tagged.txt.rm.classes <- function(txt, lemma=FALSE, lang, corp.rm.class, corp.rm.tag, as.vector=TRUE){
  # to avoid needless NOTEs from R CMD check
  wclass <- tag <- rel.col <- NULL

  txt.cleaned <- as.data.frame(txt)

  valid.tagset <- as.data.frame(kRp.POS.tags(lang))
  txt.rm.tags <- c()
  if(identical(corp.rm.class, "nonpunct")){
    corp.rm.class <- kRp.POS.tags(lang, tags=c("punct","sentc"), list.classes=TRUE)
  } else {}

  # "stopword" needs to be treated differently, it's another column
  if("stopword" %in% corp.rm.class){
    if(all(is.na(txt[,"stop"]))){
      warning("Stopword removal not possible: All values are NA! Did you provide a stopword list when tokenizing?", call.=FALSE)
    } else {
      txt.cleaned <- txt.cleaned[!txt.cleaned[["stop"]],]
    }
    # that's all we need -- remove the entry from the vector
    corp.rm.class <- corp.rm.class[!corp.rm.class %in% "stopword"]
  } else {}
  
  if(is.vector(corp.rm.class) && length(corp.rm.class) > 0){
    # only proceed if all class values are valid
    if(all(corp.rm.class %in% valid.tagset[,"wclass"])){
      txt.rm.tag.classes <- as.vector(subset(valid.tagset, wclass %in% corp.rm.class)[,"tag"])
      txt.rm.tags <- unique(c(txt.rm.tags, txt.rm.tag.classes))
    } else {
      stop(simpleError("Invalid value in corp.rm.class!"))
    }
  } else {}

  if(is.vector(corp.rm.tag) && length(corp.rm.tag) > 0){
    # only proceed if all class values are valid
    if(all(corp.rm.tag %in% valid.tagset[,"tag"])){
    txt.rm.tags <- unique(c(txt.rm.tags, corp.rm.tag))
    } else {
    stop(simpleError("Invalid value in corp.rm.tag!"))
    }
  } else {}

  # return only a vetor with the tokens itself, or the whole object?
  if(isTRUE(as.vector)){
    if(isTRUE(lemma)){
      txt.cleaned <- as.vector(subset(txt.cleaned, !tag %in% txt.rm.tags)[,"lemma"])
    } else{
      txt.cleaned <- as.vector(subset(txt.cleaned, !tag %in% txt.rm.tags)[,"token"])
    }
  } else {
    txt.cleaned <- subset(txt.cleaned, !tag %in% txt.rm.tags)
  }
  return(txt.cleaned)
} ## end function tagged.txt.rm.classes()


## function kRp.check.params()
kRp.check.params <- function(given, valid, where=NULL, missing=FALSE){
  compared <- given %in% valid
  invalid.params <- given[!compared]
  if(!is.null(where)){
    check.location <- paste0(" in \"",where,"\"")
  } else {
    check.location <- ""
  }
  if(!length(invalid.params) == 0){
    if(isTRUE(missing)){
      stop(simpleError(paste0("Missing elements", check.location,": \"", paste(invalid.params, collapse="\", \""), "\"")))
    } else {
      stop(simpleError(paste0("Invalid elements given", check.location,": \"", paste(invalid.params, collapse="\", \""), "\"")))
    }
  } else {
    return(TRUE)
  }
} ## end function kRp.check.params()


## function count.sentences()
# expects txt to be an object of class kRp.tagged,
# and tags a vector with POS tags indicating sentence endings
count.sentences <- function(txt, tags){
  num.sentences <- length(unlist(sapply(tags, function(x){which(txt[,"tag"] == x)}), use.names=FALSE))
  return(num.sentences)
} ## end function count.sentences()


## function value.distribs()
value.distribs <- function(value.vect, omit.missings=TRUE){
  vector.length <- length(unlist(value.vect))
  # some hard to tokenize texts can end up with really strange values here,
  # exceeding the threshold of maxsum=100 for summary() on factors.
  # as a result, the as.numeric(names()) a few lines below will fail because
  # vector.summary has one last entry called "(Other)".
  # so we'll dynamically adjust maxsum to the needed value
  value.factor <- as.factor(value.vect)
  vector.summary <- summary(value.factor, maxsum=length(levels(value.factor)))

  # try to fill up missing values with 0, e.g., found no words with 5 characters
  if(!isTRUE(omit.missings)){
    if(!is.numeric(value.vect)){
      # makes only sense for numeric values
      stop(simpleError("value.distribs(): Impute missings is only valid for numeric vectors!"))
    } else {}
    present.values <- as.numeric(names(vector.summary))
    max.value <- max(present.values)
    missing.values <- c(1:max.value)[!c(1:max.value) %in% present.values]
    append.to.summary <- rep(0, length(missing.values))
    names(append.to.summary) <- as.character(missing.values)
    vector.summary <- c(vector.summary, append.to.summary)
    # finally sort the outcome
    vector.summary <- vector.summary[order(as.numeric(names(vector.summary)))]
  } else {}

  # add cumulative values
  vect.summ.cum <- cumsum(vector.summary)
  # inverse 
  vect.summ.inv <- rbind(vector.summary, vect.summ.cum, vector.length - vect.summ.cum)
  # add percentages
  vect.summ.cum.pct <- rbind(vect.summ.inv, (vect.summ.inv * 100 / vector.length))
  dimnames(vect.summ.cum.pct)[[1]] <- c("num", "cum.sum", "cum.inv", "pct", "cum.pct", "pct.inv")
  return(vect.summ.cum.pct)
} ## end function value.distribs()


## function distrib.from.fixed()
# make distribution matrices from fixed text features
distrib.from.fixed <- function(fixed.vect, all.words, idx="s", unaccounted="x"){
  num.accounted <- fixed.vect[grep(paste0(idx, "[[:digit:]]+"), names(fixed.vect))]
  num.unaccounted <- all.words - sum(num.accounted)
  num.names <- names(num.accounted)

  distrib <- value.distribs(c(unlist(sapply(num.names, function(this.val){
      this.val.num <- fixed.vect[[this.val]]
      this.val.name <- as.numeric(gsub("[^[:digit:]]", "", this.val))
      return(rep(this.val.name, this.val.num))
    })), if(num.unaccounted > 0){rep(unaccounted, num.unaccounted)}))

  return(distrib)
} ## end function distrib.from.fixed()


## function distrib.to.fixed()
# the other way round, to be able to use the desc slot with readability.num()
distrib.to.fixed <- function(distrib, all.values, idx="s"){
  values <- distrib["num",]
  names(values) <- paste0(idx, colnames(distrib))
  values <- c(all=all.values, values)
  return(values)
} ## end function distrib.to.fixed()


## function text.analysis()
# "txt" must be a tagged and commented text object!
text.analysis <- function(txt, lang, corp.rm.class, corp.rm.tag, desc){
  ## global stuff
  # count sentences
  txt.stend.tags <- kRp.POS.tags(lang, list.tags=TRUE, tags="sentc")
  txt.vector <- as.vector(txt[,"token"])

  txt.nopunct <- tagged.txt.rm.classes(txt, lemma=FALSE, lang=lang, corp.rm.class=corp.rm.class, corp.rm.tag=corp.rm.tag)

  txt.lemma <- tagged.txt.rm.classes(txt, lemma=TRUE, lang=lang, corp.rm.class=corp.rm.class, corp.rm.tag=corp.rm.tag)
  txt.lemma.types <- unique(txt.lemma)
  ## end global stuff

  ## statistics on word classes
  word.classes <- kRp.POS.tags(lang, list.classes=TRUE)
  # true/false-matrix for word classes
  class.matrix <- sapply(word.classes, function(x){txt[,"wclass"] == x})
  class.num <- colSums(class.matrix)
  # types lemmata
  class.matrix.types <- sapply(word.classes, function(x){length(unique(txt[,"token"][class.matrix[,x]]))})
  # types lemmatized
  class.matrix.lemma <- sapply(word.classes, function(x){length(unique(txt[,"lemma"][class.matrix[,x]]))})

  # counting classes
  class.analysis <- sapply(word.classes, function(wd.class){
      txt.num.class <- txt[,"wclass"] == wd.class
      assign(wd.class, txt.vector[txt.num.class])
      }
    )

  # length of sentences
  # for this, first we need the text with only words and sentence ending punctuation
  txt.sentc.ends <- txt[txt[,"tag"] %in% kRp.POS.tags(lang=lang, list.tags=TRUE, tags=c("words", "sentc")), ]
  sentence.index <- c(0, which(txt.sentc.ends[,"tag"] %in% txt.stend.tags))
  ## note: if the last sentence didn't end with some punctuation, it's not counted as a sentence at all!
  if(identical(sentence.index, 0)){
    # seems there are no sentences at all
    # we'll take the number of all words as an estimate
    sentence.lengths <- length(txt[txt[,"tag"] %in% kRp.POS.tags(lang=lang, list.tags=TRUE, tags=c("words")), ])
  } else {
    sentence.lengths <- sapply(2:length(sentence.index), function(sntc.ends.here){
        entcs.words <- sentence.index[sntc.ends.here] - sentence.index[sntc.ends.here - 1] - 1
        return(entcs.words)
      })
  }

  ## further descriptives
  num.questions <- sum(txt.vector %in% "?")
  num.exclamats <- sum(txt.vector %in% "!")
  num.semicolon <- sum(txt.vector %in% ";")
  num.colon     <- sum(txt.vector %in% ":")

  add.results <- list(
              all.words=txt.nopunct,
              all.lemmata=txt.lemma.types,
              classes=class.analysis,
              lemmata=length(txt.lemma.types),
              freq.token=class.num,
              freq.types=class.matrix.types,
              freq.lemma=class.matrix.lemma,
              sentc.length=sentence.lengths,
              sentc.distrib=value.distribs(sentence.lengths),
              questions=num.questions,
              exclam=num.exclamats,
              semicolon=num.semicolon,
              colon=num.colon
          )
  results <- append(add.results, desc)
  return(results)
} ## end function text.analysis()


## function word.freq()
# takes singe character strings or vectors and looks up their appearance frequency in corpora data
# "rel" can be used to define the interesting relation:
# "pct", "pmio", "rank.avg", "rank.min", "inDocs" or "idf"
word.freq <- function(txt, corp.freq, rel, zero.NAs=FALSE){
  # some basic sanity checks
  stopifnot(inherits(corp.freq, "kRp.corp.freq"))
  if(!is.character(txt)){
    stop(simpleError("Word frequency analysis can only be run on character data!"))
  } else {}

  if(!all(rel %in% c("pct", "pmio", "log10", "rank.avg", "rank.min", "rank.rel.avg", "rank.rel.min", "inDocs", "idf"))){
    stop(simpleError(paste0("Option \"rel\" must be \"pct\", \"pmio\", \"log10\", \"rank.avg\", \"rank.min\", \"rank.rel.avg\",\n",
      "\"rank.rel.min\", \"inDocs\" or \"idf\"!")))
  } else {}

  corp.index <- match(txt, corp.freq@words$word)
  results <- corp.freq@words[corp.index, rel]

  if(isTRUE(zero.NAs) & any(is.na(results))){
    if(inherits(results, "data.frame")){
      rowsWithNAs <- unique(which(is.na(results), arr.ind=TRUE)[,"row"])
      results[rowsWithNAs,] <- rep(0, ncol(results))
    } else {
      results[is.na(results)] <- 0
    }
  } else {}
  return(results)
} ## end function word.freq()


## function type.freq()
# this function will identify unique types in a tagged text object
# and count how often it appears in the text
# - txt: must be a tagged text object
# - vector.only: if the numbers and letters are not needed, return the vector of types an quit
type.freq <- function(txt, case.sens=TRUE, verbose=FALSE, lemma=FALSE, fail.if.no.lemmas=TRUE, vector.only=FALSE){
  # shall we count all tokens or their lemmas?
  if(isTRUE(lemma)){
    # do a sanity check, are lemmata present?
    if(identical(unique(txt[,"lemma"]), "")){
      if(isTRUE(fail.if.no.lemmas)){
        stop(simpleError(paste0("You asked to use lemmata, but your text object doesn't include any!",
          "\n  This is probably the case because you used tokenize() instead of treetag().")))
      } else {
        return(NULL)
      }
    } else {
      relevant.tokens <- "lemma"
    }
  } else {
    relevant.tokens <- "token"
  }

  all.tokens <- txt[,c(relevant.tokens,"lttr")]
  if(!isTRUE(case.sens)){
    all.tokens[[relevant.tokens]] <- tolower(all.tokens[[relevant.tokens]])
  } else {}
  corp.freq <- unique(all.tokens)
  colnames(corp.freq) <- c("type","lttr")
  if(isTRUE(vector.only)){
    return(corp.freq[["type"]])
  } else {
    corp.freq[["freq"]] <- 0
    num.tokens <- nrow(all.tokens)
    num.types <- nrow(corp.freq)
    if(isTRUE(verbose)){
      type.counter <- 1
    } else {}
    for (tp in seq_along(corp.freq[["type"]])){
      corp.freq[tp, "freq"] <- sum(all.tokens[[relevant.tokens]] %in% corp.freq[tp, "type"], na.rm=TRUE)
      if(isTRUE(verbose)){
        message(paste0("\t", floor(100*type.counter/num.types), "% complete, processing ", relevant.tokens, " ", type.counter, " of ", num.types, ": \"", corp.freq[tp, "type"], "\""))
        message(paste0(" (found ", corp.freq[tp, "freq"], " times in ", num.tokens, " ", relevant.tokens, "s)\n"))
        type.counter <- type.counter + 1
      } else {}
    }
    # order results
    corp.freq <- corp.freq[order(corp.freq[,"freq"], corp.freq[,"lttr"], decreasing=TRUE),]
    rownames(corp.freq) <- NULL
  }
  return(corp.freq)
} ## end function type.freq()


## function frqcy.summarize()
frqcy.summarize <- function(pct.data, na.rm=TRUE){
  summary.pct <- summary(pct.data)
  sd.pct <- sd(pct.data, na.rm=na.rm)
  quant.pct <- quantile(pct.data, probs=seq(0,1,0.05), na.rm=na.rm)
  results <- list(summary=summary.pct, sd=sd.pct, quantiles=quant.pct)
  return(results)
} ## end function frqcy.summarize()


## function frqcy.by.rel()
frqcy.by.rel <- function(txt.commented, corp.freq, corp.rm.class, corp.rm.tag, rel){
  # to avoid needless NOTEs from R CMD check
  wclass <- tag <- rel.col <- NULL

  # look up percentages for each part of the text
  # call internal function word.freq()
  txt <- as.character(txt.commented[["token"]])
  txt.rel.all <- as.numeric(word.freq(txt, corp.freq=corp.freq, rel=rel, zero.NAs=TRUE))
  # add percent info to the commented text
  new.commented <- cbind(txt.commented, rel.col=txt.rel.all)

  # now let's do some calculations...
  frqcy.summary <- frqcy.summarize(txt.rel.all)

  # there are probably some NAs in our text. they're the result of words not found
  # in the language corpus, hence we'll compute another summary and assume NA equals to probabilty of 0
  txt.rel.noNAs <- txt.rel.all
  frqcy.summary.noNAs <- frqcy.summarize(txt.rel.noNAs)
  # if defined, remove entries of certain classes from the word list
  txt.dropped.classes <- droplevels(subset(new.commented, !wclass %in% corp.rm.class)) 
  txt.dropped.classes <- droplevels(subset(txt.dropped.classes, !tag %in% corp.rm.tag)) 
  txt.rel.sub <- subset(txt.dropped.classes, select = rel.col)$rel.col
  # exclude NAs as well, with probablility = 0
  frqcy.summary.excluded <- frqcy.summarize(txt.rel.sub)
  # and finally, analyze only unique words
  txt.types <- unique(txt.dropped.classes)
  txt.rel.types <- subset(txt.types, select = rel.col)$rel.col
  frqcy.summary.types <- frqcy.summarize(txt.rel.types)

  results <- list(
    rel.col=txt.rel.all,
    summary.known=frqcy.summary,
    summary.all=frqcy.summary.noNAs,
    summary.excluded=frqcy.summary.excluded,
    summary.types=frqcy.summary.types)
  return(results)
} ## end function frqcy.by.rel()

## function frqcy.of.types()
# takes a vector of tokens (and optionally types) and returns a list with two
# possible entries, bot a data.fame with two columns: tokens/types and its absolute frequency
frqcy.of.types <- function(tokens, byTypes=TRUE, byTokens=TRUE){
  result <- list(byTypes=NA, byTokens=NA)
  freq.table <- table(tokens)
  types <- names(freq.table)

  if(isTRUE(byTypes)){
    result[["byTypes"]] <- data.frame(
      type=types,
      freq=as.vector(freq.table),
      row.names=types,
      stringsAsFactors=FALSE)
  } else {}

  if(isTRUE(byTokens)){
    result[["byTokens"]] <- data.frame(
      token=tokens,
      freq=as.vector(freq.table[tokens]),
      stringsAsFactors=FALSE)
  } else {}
  
  return(result)
} ## end function frqcy.of.types()


## function text.freq.analysis()
# expects tagged text, commented text and valid corp.freq objects
text.freq.analysis <- function(txt.commented, corp.freq, corp.rm.class, corp.rm.tag, lang, tfidf=FALSE){
  # to avoid needless NOTEs from R CMD check
  wclass <- NULL

  stopifnot(inherits(corp.freq, "kRp.corp.freq"))

  frq.pmio      <- frqcy.by.rel(txt.commented, corp.freq=corp.freq, corp.rm.class=corp.rm.class, corp.rm.tag=corp.rm.tag, rel="pmio")
  frq.log10     <- frqcy.by.rel(txt.commented, corp.freq=corp.freq, corp.rm.class=corp.rm.class, corp.rm.tag=corp.rm.tag, rel="log10")
  frq.rank.avg  <- frqcy.by.rel(txt.commented, corp.freq=corp.freq, corp.rm.class=corp.rm.class, corp.rm.tag=corp.rm.tag, rel="rank.rel.avg")
  frq.rank.min  <- frqcy.by.rel(txt.commented, corp.freq=corp.freq, corp.rm.class=corp.rm.class, corp.rm.tag=corp.rm.tag, rel="rank.rel.min")
  # calculate tf-idf measure
  frq.idf <- frqcy.by.rel(txt.commented, corp.freq=corp.freq, corp.rm.class=corp.rm.class, corp.rm.tag=corp.rm.tag, rel="idf")
  # frequency of types in this document
  type.freq <- frqcy.of.types(tokens=txt.commented[["token"]], byTypes=FALSE, byTokens=TRUE)
  tfidf <- type.freq[["byTokens"]][["freq"]] * frq.idf[["rel.col"]]
  # recreate the commented text with percent info, to substitute the old object
  new.commented <- cbind(txt.commented,
                          pmio=frq.pmio[["rel.col"]],
                          log10=frq.log10[["rel.col"]],
                          rank.avg=frq.rank.avg[["rel.col"]],
                          rank.min=frq.rank.min[["rel.col"]],
                          tf=type.freq[["byTokens"]][["freq"]],
                          idf=frq.idf[["rel.col"]],
                          tfidf=tfidf)

  # information on words-per-sentence and commas-per-sentence
  num.sentences <- dim(subset(new.commented, wclass %in% kRp.POS.tags(lang, tags="sentc", list.classes=TRUE)))[1]
  num.commas    <- dim(subset(new.commented, wclass %in% "comma"))[1]
  freq.commas   <- num.commas/num.sentences
  num.words     <- dim(subset(new.commented, !wclass %in% kRp.POS.tags(lang, tags=c("punct","sentc"), list.classes=TRUE)))[1]
  freq.words    <- num.words/num.sentences
  freq.w.p.c    <- num.words/num.commas
  res.sentences <- data.frame(words.p.sntc=freq.words, comma.p.sntc=freq.commas, words.p.comma=freq.w.p.c)

  freq.analysis <- list(
    frq.pmio=frq.pmio[-c(1,2)],
    frq.log10=frq.log10[-c(1,2)],
    frq.rank.avg=frq.rank.avg[-c(1,2)],
    frq.rank.min=frq.rank.min[-c(1,2)],
    sentence.factors=res.sentences)

  results <- list(commented=new.commented, freq.analysis=freq.analysis)

  return(results)
} ## end function text.freq.analysis()


## function create.corp.freq.object()
# this function should be used to create corpus frequency objects
# the idea is to have this one function so that any kind of corpora data
# can be squeezed into the format we want.
# "matrix.freq" needs to be a matrix with three columns:
#     "num" (some ID), "word" (the actual running word form) and "freq" (absolute frequency).
#     optional columns are "lemma", "tag", "wclass", "inDocs" and "idf".
# "df.meta" must be a data.frame with two columns: "meta" (name of meta information) and its "value".
# "dscrpt.meta" must be a data.frame with six columns: "tokens" (old: "words"), "types" (old: "dist.words"),
#   "words.p.sntc", "chars.p.sntc", "chars.p.wform" and "chars.p.word"; if NULL its value is set to an empty default
# "extra.cols" is an optional data.frame with additional columns, e.g. valence data
# "casSens" determines whether frequency stats should distinguish between letter cases or consolidate otherwise identical tokens
create.corp.freq.object <- function(matrix.freq, num.running.words, df.meta, df.dscrpt.meta,
  matrix.table.bigrams=NULL, matrix.table.cooccur=NULL, extra.cols=NULL, caseSens=TRUE, quiet=FALSE){
  tokenFreq <- as.numeric(matrix.freq[,"freq"])
  if(!isTRUE(caseSens)){
    # first look for tokens that only differ in letter case and
    # replace freq with sum of all occurances
    lowerTokens <- tolower(matrix.freq[,"word"])
#     caseSensTokens <- unique(lowerTokens[duplicated(lowerTokens)])
#     caseSensFreq <- as.list(rep(0, length(caseSensTokens)))
#     names(caseSensFreq) <- caseSensTokens
#     caseSensFreq <- as.environment(caseSensFreq)
    caseSensFreq <- new.env()
    if(!isTRUE(quiet)){
      num.tokens <- length(lowerTokens) # length(caseSensTokens)
      message(paste0("Re-calculating token frequencies (case insensitve, ", num.tokens, " tokens)"))
      # just some feedback, so we know the machine didn't just freeze...
      prgBar <- txtProgressBar(min=0, max=num.tokens, style=3)
    } else {}

    start.with <- new.env()
    start.with[["count"]] <- 1
    # we don't really need this object, sapply updates the environment caseSensFreq
    tmp <- sapply(
      seq_along(tokenFreq),
      function(numToken){
        if(!isTRUE(quiet)){
          # update progress bar
          setTxtProgressBar(prgBar, start.with[["count"]])
        } else {}
        thisLowToken <- lowerTokens[numToken]
        if(is.null(caseSensFreq[[thisLowToken]])){
          caseSensFreq[[thisLowToken]] <- tokenFreq[numToken]
        } else {
          caseSensFreq[[thisLowToken]] <- caseSensFreq[[thisLowToken]] + tokenFreq[numToken]
        }
        start.with[["count"]] <- start.with[["count"]] + 1
      }
    )
    tokenFreq <- unlist(sapply(
      lowerTokens,
      function(thisToken){
        caseSensFreq[[thisToken]]
      },
      USE.NAMES=FALSE
    ))
    if(!isTRUE(quiet)){
      setTxtProgressBar(prgBar, num.tokens)
      close(prgBar)
    } else {}
  } else {}

  # try to work around missing meta information
  if(is.na(num.running.words)){
    num.running.words <- sum(tokenFreq)
  }
  # calculate rank data
  rank.avg <- rank(tokenFreq, ties.method="average")
  rank.min <- rank(tokenFreq, ties.method="min")
  # for better comparability, compute relative ranks
  # can take values between 0 and 100
  rank.rel.avg <- (rank.avg / max(rank.avg)) * 100
  rank.rel.min <- (rank.min / max(rank.min)) * 100

  words.per.mio <- tokenFreq %/% (num.running.words/1000000)
  log10.per.mio <- log10(words.per.mio)
  # correct for lowest frequency words and log10(0), which returns -Inf
  log10.per.mio[log10.per.mio < 0] <- 0

  # check if the optional columns "lemma", "tag", "wclass", "inDocs" and "idf" are present
  if("lemma" %in% colnames(matrix.freq)){
    have.lemma <- matrix.freq[,"lemma"]
  } else {
    have.lemma <- NA
  }
  if("tag" %in% colnames(matrix.freq)){
    have.tag <- matrix.freq[,"tag"]
  } else {
    have.tag <- NA
  }
  if("wclass" %in% colnames(matrix.freq)){
    have.wclass <- matrix.freq[,"wclass"]
  } else {
    have.wclass <- NA
  }
  if("inDocs" %in% colnames(matrix.freq)){
    have.inDocs <- matrix.freq[,"inDocs"]
  } else {
    have.inDocs <- NA
  }
  if("idf" %in% colnames(matrix.freq)){
    have.idf <- matrix.freq[,"idf"]
  } else {
    have.idf <- NA
  }
  df.words <- data.frame(
              num=as.numeric(matrix.freq[,"num"]),
              word=matrix.freq[,"word"],
              lemma=have.lemma,
              tag=have.tag,
              wclass=have.wclass,
              lttr=nchar(matrix.freq[,"word"], allowNA=TRUE),
              freq=tokenFreq,
              pct=tokenFreq/num.running.words,
              pmio=words.per.mio,
              log10=log10.per.mio,
              rank.avg=rank.avg,
              rank.min=rank.min,
              rank.rel.avg=rank.rel.avg,
              rank.rel.min=rank.rel.min,
              inDocs=have.inDocs,
              idf=have.idf,
              stringsAsFactors=FALSE)
  # add extra columns, if given
  if(!is.null(extra.cols)){
    df.words <- cbind(df.words, extra.cols, stringsAsFactors=FALSE)
  } else {}
  # set missing information to a valid defaults
  if(is.null(df.dscrpt.meta)){
    df.dscrpt.meta <- slot(kRp_corp_freq(), "desc")
  } else {}
  if(is.null(df.meta)){
    df.meta <- slot(kRp_corp_freq(), "meta")
  } else {}
  if(is.null(matrix.table.bigrams)){
    df.table.bigrams <- slot(kRp_corp_freq(), "bigrams")
  } else {
    message("Fetching bigram tokens from data... ", appendLF=FALSE)
    df.table.bigrams <- data.frame(
      token1=df.words[
        sapply(
          as.numeric(matrix.table.bigrams[,"token1"]),
          function(x){
            which(df.words[,"num"] == x)
          }
        ), "word"],
      token2=df.words[
        sapply(
          as.numeric(matrix.table.bigrams[,"token2"]),
          function(x){
            which(df.words[,"num"] == x)
          }
        ), "word"],
      freq=as.numeric(matrix.table.bigrams[,"freq"]),
      sig=as.numeric(matrix.table.bigrams[,"sig"])
    )
    # sort by frequency
    message("sorting... ", appendLF=FALSE)
    df.table.bigrams <- df.table.bigrams[with(df.table.bigrams, order(freq, decreasing=TRUE)),]
    message("done.")
  }
  if(is.null(matrix.table.cooccur)){
    df.table.cooccur <- slot(kRp_corp_freq(), "cooccur")
  } else {
    message("Fetching co-occurrence tokens from data... ", appendLF=FALSE)
    df.table.cooccur <- data.frame(
      token1=df.words[
        sapply(
          as.numeric(matrix.table.cooccur[,"token1"]),
          function(x){
            which(df.words[,"num"] == x)
          }
        ), "word"],
      token2=df.words[
        sapply(
          as.numeric(matrix.table.cooccur[,"token2"]),
          function(x){
            which(df.words[,"num"] == x)
          }
        ), "word"],
      freq=as.numeric(matrix.table.cooccur[,"freq"]),
      sig=as.numeric(matrix.table.cooccur[,"sig"])
    )
    # sort by frequency
    message("sorting... ", appendLF=FALSE)
    df.table.cooccur <- df.table.cooccur[with(df.table.cooccur, order(freq, decreasing=TRUE)),]
    message("done.")
  }
  results <- kRp_corp_freq(
    meta=df.meta,
    words=df.words,
    desc=df.dscrpt.meta,
    bigrams=df.table.bigrams,
    cooccur=df.table.cooccur,
    caseSens=caseSens
  )
  return(results)
} ## end function create.corp.freq.object()


## function noInf.summary()
# helper function to produce characteristics summaries without infinite values
# used in the show methods
# data must be a vector
noInf.summary <- function(data, add.sd=FALSE){
  data[is.infinite(data)] <- NA
  print(summary(data))
  if(isTRUE(add.sd)){
    cat("   SD\n ", round(sd(data, na.rm=TRUE), digits=4), "\n", sep="")
  } else {}
} ## end function noInf.summary()


## function is.supported.lang()
# determins if a language is supported, and returns the correct identifier
is.supported.lang <- function(lang.ident, support="treetag"){
  if(identical(support, "treetag")){
    treetag.supported <- as.list(as.environment(.koRpus.env))[["langSup"]][["kRp.POS.tags"]][["tags"]]
    if(lang.ident %in% names(treetag.supported)){
      res.ident <- lang.ident
    } else {
      stop(simpleError(
        paste0(
          "Unknown tag definition requested: ", lang.ident, "\n",
          "See ?available.koRpus.lang() for a list of supported languages."
        )
      ))
    }
  } else {}

  return(res.ident)
} ## end function is.supported.lang()


## function txt.compress()
# will gzip an object and return file size and compression ratio
# can be used to estimate redundancy on a global level
txt.compress <- function(obj, level=9, ratio=FALSE, in.mem=TRUE){

  if(is.character(obj)){
    txt <- obj
  } else if(inherits(obj, "kRp.tagged")){
    txt <- kRp.text.paste(obj)
  } else {
    stop(simpleError("Cannot compress objects which are neither character nor of a koRpus class!"))
  }

  if(isTRUE(in.mem)){
    zz.gz <- memCompress(txt, "gzip")
    zz.gz.size <- object.size(zz.gz)
    if(isTRUE(ratio)){
      zz.non <- memCompress(txt, "none")
      zz.non.size <- object.size(zz.non)
      ratio <- as.numeric(zz.gz.size / zz.non.size)
    } else {
      zz.non.size <- NA
      ratio <- NA
    }
  } else {
    tmp.path <- tempfile("koRpus.gz")
      if(!dir.create(tmp.path, recursive=TRUE)) stop(simpleError("Compression skipped: Can't create temporary directory!"))
    # if the function is done, remove the tempdir
    on.exit(unlink(tmp.path, recursive=TRUE))

    # (probably) create two connection, one compressed
    zz.gz <- gzfile(file.path(tmp.path, "bloat.gz"), open="w", compression=level)
    writeLines(txt, zz.gz, sep=" ")
    close(zz.gz)
    zz.gz.size <- file.info(file.path(tmp.path, open="bloat.gz"))$size
    if(isTRUE(ratio)){
      zz.non <- file(file.path(tmp.path, "bloat"), open="w")
      writeLines(txt, zz.non, sep=" ")
      close(zz.non)
      zz.non.size <- file.info(file.path(tmp.path, open="bloat"))$size
      ratio <- zz.gz.size / zz.non.size
    } else {
      zz.non.size <- NA
      ratio <- NA
    }
  }

  results <- list(file.size=zz.non.size, gz.size=zz.gz.size, ratio=ratio)

  return(results)
} ## end function txt.compress()


## function read.udhr()
# this function will read the Universal Declaration of Human Rights from text files by
# the UDHR in Unicode project: http://unicode.org/udhr/downloads.html
# txt.path must be a character string pinting to the directory where the bulk files
# were extracted, or to the ZIP file.
read.udhr <- function(txt.path, quiet=TRUE){

  # check if txt.path is a zip file
  if(!as.logical(file_test("-d", txt.path))){
    if(file.exists(txt.path) & grepl("(\\.zip|\\.ZIP)$", txt.path)){
      # ok, seems to be an existing zip file
      # unpack it to a temporary location and alter variable to use that location
      tmp.path <- tempfile("koRpus.UDHR")
      if(!dir.create(tmp.path, recursive=TRUE)) stop(simpleError("UDHR: Can't create temporary directory!"))
      # if the function is done, remove the tempdir
      on.exit(unlink(tmp.path, recursive=TRUE))
      unzip(txt.path, exdir=tmp.path)
      udhr.path <- tmp.path
    } else {
      stop(simpleError(paste("Cannot access UDHR location:", txt.path)))
    }
  } else {
    udhr.path <- txt.path
  }

  #   # requires package XML
  #   udhr.xml <- xmlParse(file.path(udhr.path, "index.xml"))
  #   udhr.list <- xmlSApply(xmlRoot(udhr.xml), xmlAttrs)
  ## since there was no windows package XML for R 2.13, this is a primitive parser which does the job:
  udhr.XML <- readLines(file.path(udhr.path, "index.xml"), warn=FALSE)
  # remove comments
  udhr.XML <- gsub("[[:space:]]*<!--(.*)-->", "", udhr.XML[grep("<udhr ", udhr.XML)])
  # filter out only the interesting parts
  udhr.XML <- gsub("([[:space:]]+<udhr )(.*)/>", "\\2", udhr.XML, perl=TRUE)
  # split vector into a list with ell elements; then we have basically what xmlParse() and xmlSApply() returned
  udhr.XML <- gsub("([[:alnum:]]+)=('[^']+'|'')[[:space:]]+", "\\1=\\2#", udhr.XML, perl=TRUE)
  # as a safety measure, put iso639-3 in quotes
  udhr.XML <- gsub("#iso639-3=", "#\"iso639-3\"=", udhr.XML)
  udhr.XML.list <- strsplit(udhr.XML, split="#")
  udhr.list <- lapply(seq_along(udhr.XML.list), function(cur.entry){eval(parse(text=paste0("c(", paste(udhr.XML.list[[cur.entry]], collapse=", "), ")")))})

  names(udhr.list) <- seq_along(udhr.list)
  # correct for missing values and variables
  udhr.list.corr <- sapply(udhr.list, function(udhr.entry){
      # older index files split up l+v and n+nv, newer have combined f and n elements
      if("f" %in% names(udhr.entry)){
        udhr.entry[["file"]] <- file.path(udhr.path, paste0("udhr_", udhr.entry[["f"]], ".txt"))
        udhr.entry[["name"]] <- udhr.entry[["n"]]
      } else if("l" %in% names(udhr.entry)){
        if(!"v" %in% names(udhr.entry)){
          udhr.entry[["v"]] <- NA
          udhr.entry[["file"]] <- file.path(udhr.path, paste0("udhr_", udhr.entry[["l"]], ".txt"))
        } else {
          udhr.entry[["file"]] <- file.path(udhr.path, paste0("udhr_", udhr.entry[["l"]], "_", udhr.entry[["v"]], ".txt"))
        }
        if(!"nv" %in% names(udhr.entry)){
          udhr.entry[["nv"]] <- NA
          udhr.entry[["name"]] <- udhr.entry[["n"]]
        } else {
          udhr.entry[["name"]] <- paste0(udhr.entry[["n"]], " (",udhr.entry[["nv"]],")")
        }
      } else {
        stop(simpleError("Erm, sorry -- looks like the format if the index.xml file has changed, please file a bug report!"))
      }
      # now try to load the translated text from file and add it to the entry
      udhr.file <- udhr.entry[["file"]]
      if(file.exists(udhr.file)){
        # remove the trailing notice also, because it will keep gzip from
        # optimizing for the actual charset
        udhr.entry[["text"]] <- gsub("^.*udhr. --- ", "", paste(suppressWarnings(scan(udhr.file, what=character(), quiet=quiet)), collapse=" "))
      } else {
        udhr.entry[["text"]] <- NA
      }
      # reorder again, otherwise t() will result in strange outcomes...
      entry.names <- sort(names(udhr.entry))
      return(udhr.entry[entry.names])
    }
  )

  udhr.df.corr <- as.data.frame(t(udhr.list.corr), stringsAsFactors=FALSE)
  # remove entries without a text to compare
  results <- udhr.df.corr[!is.na(udhr.df.corr[,"text"]), ]

  return(results)
} ## end function read.udhr()


## function text.1st.letter()
# changes the first letter of a word to upper or lower case
# if case="change", the present case will be switched to the other
text.1st.letter <- function(word, case){

  results <- sapply(word, function(cur.word){
    word.vector <- unlist(strsplit(cur.word, split=""))

    if(identical(case, "upper")){
      word.vector[1] <- toupper(word.vector[1])
    } else{}
    if(identical(case, "lower")){
      word.vector[1] <- tolower(word.vector[1])
    } else{}
    if(identical(case, "change")){
      if(isTRUE(grepl("([[:lower:]])", word.vector[1]))){
        word.vector[1] <- toupper(word.vector[1])
      } else if(isTRUE(grepl("([[:upper:]])", word.vector[1]))){
        word.vector[1] <- tolower(word.vector[1])
      } else {}
    } else{}

    word.new <- paste(word.vector, collapse="")
    return(word.new)
    }, USE.NAMES=FALSE)

  return(results)
} ## end function text.1st.letter()


## function taggz()
# takes a vector of tokens and adds internal tags
# unicode escapes:
#   <e2><80><99> -> \u2019 right single quotation mark
#   <e2><80><93> -> \u2013 en dash
#' @importFrom data.table data.table :=
taggz <- function(tokens, abbrev=NULL, heur.fix=list(pre=c("\u2019","'"), suf=c("\u2019","'")), ign.comp="", sntc=c(".","!","?",";",":")){
  # make R CMD check happy...
  token <- tag <- NULL
  tagged.text <- data.table(token=tokens, tag="unk.kRp")
  Encoding(tagged.text$token) <- "UTF-8"
  Encoding(tagged.text$tag) <- "UTF-8"
  tagged.text[token == "" & tag == "unk.kRp", tag := "unk"]
  # all letters, assume it's a word
  tagged.text[!grepl(paste("[^\\p{L}\\p{M}",paste(ign.comp, collapse=""),"]"), token, perl=TRUE) & tag == "unk.kRp", tag := "word.kRp"]
  # all digits, assume it's a number
  tagged.text[!grepl(paste("[^\\p{N}",paste(ign.comp, collapse=""),"]"), token, perl=TRUE) & tag == "unk.kRp", tag := "no.kRp"]
  # assume it's an abbreviation
  tagged.text[token %in% abbrev & tag == "unk.kRp", tag := "abbr.kRp"]
  # assume it's a sentence ending
  tagged.text[token %in% sntc & tag == "unk.kRp", tag := ".kRp"]
  # all dots
  tagged.text[!grepl("[^.]", token, perl=TRUE) & tag == "unk.kRp", tag := "-kRp"]
  tagged.text[token == "," & tag == "unk.kRp", tag := ",kRp"]
  tagged.text[!grepl(paste("[^\\p{Ps}",paste(ign.comp, collapse=""),"]"), token, perl=TRUE) & tag == "unk.kRp", tag := "(kRp"]
#       } else if(tkn %in% c("(","{","[")){
#         return(c(token=tkn, tag="(kRp"))
  tagged.text[!grepl(paste("[^\\p{Pe}",paste(ign.comp, collapse=""),"]"), token, perl=TRUE) & tag == "unk.kRp", tag := ")kRp"]
#       } else if(tkn %in% c(")","}","]")){
#         return(c(token=tkn, tag=")kRp"))
#       } else if(tkn %in% c("-","\u2013")){
#         return(c(token=tkn, tag="-kRp"))
  tagged.text[token %in% c("\"","'","''","`","``","\u2019","\u2019\u2019") & tag == "unk.kRp", tag := "''kRp"]
  tagged.text[!grepl(paste("[^\"\\p{Pi}\\p{Pf}",paste(ign.comp, collapse=""),"]"), token, perl=TRUE) & tag == "unk.kRp", tag := "''kRp"]
  tagged.text[!grepl(paste("[^\\p{Pd}",paste(ign.comp, collapse=""),"]"), token, perl=TRUE) & tag == "unk.kRp", tag := "-kRp"]
  # any other punctuation
  tagged.text[!grepl(paste("[^\\p{P}",paste(ign.comp, collapse=""),"]"), token, perl=TRUE) & tag == "unk.kRp", tag := "-kRp"]
  # simple heuristics for abbreviations
  tagged.text[!grepl(paste("[^\\p{L}\\p{M}.",paste(ign.comp, collapse=""),"]"), token, perl=TRUE) & tag == "unk.kRp", tag := "abbr.kRp"]
  # simple heuristics for pre- and suffixes
  tagged.text[!grepl(paste("[^\\p{L}\\p{M}\\p{N}",paste(unique(unlist(heur.fix), ign.comp), collapse=""),"]"), token, perl=TRUE) & tag == "unk.kRp", tag := "word.kRp"]
  # automatic healine or paragraph detection:
  tagged.text[token == "<kRp.h>" & tag == "unk.kRp", tag := "hon.kRp"]
  tagged.text[token == "</kRp.h>" & tag == "unk.kRp", tag := "hoff.kRp"]
  tagged.text[token == "<kRp.p/>" & tag == "unk.kRp", tag := "p.kRp"]

  tagged.text <- as.matrix(tagged.text)
  
  return(tagged.text)
} ## end function taggz()


## function tokenz()
# a simple tokenizer. txt must be a character vector with stuff to tokenize
# will return a vector with one token per element
# 'abbrev' must be a text file encoded in 'encoding', with one abbreviation per line
tokenz <- function(txt, split="[[:space:]]", ign.comp="-", heuristics="abbr", abbrev=NULL,
        encoding="unknown", heur.fix=list(pre=c("\u2019","'"), suf=c("\u2019","'")), tag=FALSE,
        sntc=c(".","!","?",";",":"), detect=c(parag=FALSE, hline=FALSE)){

  ## clean ign.comp for use in regexp
  ign.comp.rex <- ign.comp
  ign.comp.rex[ign.comp.rex %in% "-"] <- "\\-"
  ign.comp.rex <- paste(ign.comp.rex, collapse="")

  ## prep the text
  # if headlines and paragraphs should be autodetected
  if(any(detect)){
    hline <- ifelse("hline" %in% names(detect), detect[["hline"]], FALSE)
    parag <- ifelse("parag" %in% names(detect), detect[["parag"]], FALSE)
    lines.empty <- grepl("^([[:space:]]*)$", txt)
    if(isTRUE(hline)){
      # lines without any sentence ending punctuation are considered headlines
      headlines <-  !lines.empty & !grepl("[.;:!?]", txt) & !grepl(",$", txt)
      txt[headlines] <- paste("<kRp.h>", txt[headlines], "</kRp.h>", sep=" ")
    } else {}
    if(isTRUE(parag)){
      paragraphs <- lines.empty & c(FALSE, !lines.empty[-length(lines.empty)])
      if(isTRUE(hline)){
        # if the previous entry was a headline, don't tag a paragraph as well
        paragraphs <- paragraphs & c(FALSE, !headlines[-length(headlines)])
      } else {}
      txt[paragraphs] <- "<kRp.p/>"
    } else {}
  } else {}

  tk.pre.lst <- unlist(strsplit(txt, split))

  # check for abbreviations to consider
  if(!is.null(abbrev)){
    check.file(abbrev, mode="exist")
    abbrev.vect <- readLines(abbrev, encoding=encoding)
  } else {
    abbrev.vect <- NULL
  }

  # call our own simple tokenizer
  # it should split strings at every non-letter character, with the
  # exception of a defined word-binding (like "-")
  tokenized.text <- as.vector(unlist(sapply(tk.pre.lst, function(tkn){
      # in case of empty elements, skip to the next one
      if(identical(tkn,"")){
        return()
      } else {}
      # if automatic healine or paragraph detection was used, filter their tags
      if(tkn %in% c("<kRp.h>", "</kRp.h>", "<kRp.p/>")){
        return(tkn)
      } else {}
      # if it's all just letters and numbers, leave as is
      if(!grepl("[^\\p{L}\\p{M}\\p{N}]", tkn, perl=TRUE)){
        return(tkn)
      } else {
        # first, split off escaped quotes
        if(grepl("\"", tkn, perl=TRUE)){
          tkn <- gsub("(\")([^\\s])", "\\1 \\2", tkn, perl=TRUE)
          tkn <- gsub("([^\\s])(\")", "\\1 \\2", tkn, perl=TRUE)
        } else {}
        # create a vector for later exclusion if a pre/suffix heuristic was chosen and already applied
        tkn.nonheur <- tkn
        all.fixes <- c()
        # for pre- and suffixes, set number of occurring letters
        pre.max.num <- "" # the empty default reads a "exactly one letter"
        suf.max.num <- "{1,2}"
        # see what we need to check for
        check.suffix <- ifelse(any(c("en", "fr", "suf") %in% heuristics), TRUE, FALSE)
        check.prefix <- ifelse(any(c("fr", "pre") %in% heuristics), TRUE, FALSE)
        if("fr" %in% heuristics){
          # in french longer suffixes like "'elle" are common
          suf.max.num <- "+"
        } else {}
        # probably split off french prefixes like l'animal
        if(isTRUE(check.prefix)){
          stopifnot(length(heur.fix$pre) > 0)
          heur.prefix <- paste0(heur.fix$pre, collapse="")
          # also take care of cases where there's non-letter stuff before the prefix or after the prefixed word
          tkn <- gsub(paste0("(^[\\p{Z}\\p{S}\\p{P}]*)([\\p{L}\\p{M}\\p{N}]", pre.max.num, "[", heur.prefix, "])([\\p{L}\\p{M}\\p{N}]+)([\\p{Z}\\p{S}\\p{P}]*$)"),
            "\\1 \\2 \\3 \\4", tkn, perl=TRUE)
          all.fixes <- unique(c(all.fixes, heur.prefix))
          tkn.nonheur <- gsub(paste0("[",all.fixes,"]", collapse=""), "", tkn)
        } else {}
        # the same for possessive 's and the like
        if(isTRUE(check.suffix)){
          stopifnot(length(heur.fix$suf) > 0)
          heur.suffix <- paste0(heur.fix$suf, collapse="")
          # also take care of cases where there's non-letter stuff before the suffix or after the suffixed word
          tkn <- gsub(paste0("(^[\\p{Z}\\p{S}\\p{P}]*)([\\p{L}\\p{M}\\p{N}]+)([", heur.suffix, "][\\p{L}\\p{M}\\p{N}]", suf.max.num, ")([\\p{Z}\\p{S}\\p{P}]*$)"),
            "\\1 \\2 \\3 \\4", tkn, perl=TRUE)
          all.fixes <- unique(c(all.fixes, heur.suffix))
          tkn.nonheur <- gsub(paste0("[",all.fixes,"]", collapse=""), "", tkn)
        } else {}
        # check for possible abbreviations
        if(is.null(abbrev.vect) | !tkn %in% abbrev.vect){
          # do the heuristics
          # currently, single letters followed by a dot will be taken for an abbreviated name;
          # the former implementation demanded at least two appearances of that:
          #if("abbr" %in% heuristics & grepl("(\\p{L}\\p{M}*\\.){2,}", tkn, perl=TRUE)){
          if("abbr" %in% heuristics & grepl("(^\\p{P}*\\p{L}\\p{M}*\\.)", tkn, perl=TRUE)){
            # separate closing/opening brackets and dashes
            tkn <- gsub("([\\p{Pe}\\p{Ps}\\p{Pd}])([\\p{L}\\p{M}])", "\\1 \\2", tkn, perl=TRUE)
            tkn <- gsub("(\\.)([^\\p{L}\\p{M}])", "\\1 \\2", tkn, perl=TRUE)
          # take care of probably already applied prefix/suffix heuristics here:
          } else if(grepl("([\\p{L}\\p{M}\\p{N}]+)([^\\p{L}\\p{M}\\p{N}\\s]+)|([^\\p{L}\\p{M}\\p{N}\\s]+)([\\p{L}\\p{M}\\p{N}]+)", tkn.nonheur, perl=TRUE)){
            # this should be some other punctuation or strange stuff...
            tkn <- gsub(paste0("([^\\p{L}\\p{M}\\p{N}",ign.comp.rex,"])([\\p{L}\\p{M}\\p{N}])"), "\\1 \\2", tkn, perl=TRUE)
            tkn <- gsub(paste0("([\\p{L}\\p{M}\\p{N}])([^\\p{L}\\p{M}\\p{N}",ign.comp.rex,"])"), "\\1 \\2", tkn, perl=TRUE)
          } else {}
          # is there clusters of undefined nonword stuff left?
          if(grepl("([^\\p{L}\\p{M}\\p{N}\\s]{2,})", tkn, perl=TRUE)){
            # as long as it's not dots:
            tkn <- gsub(paste0("([^\\p{Zs}])([^\\p{L}\\p{M}\\p{N}.",ign.comp.rex,"\\p{Zs}])"), "\\1 \\2", tkn, perl=TRUE)
            tkn <- gsub(paste0("([^\\p{L}\\p{M}\\p{N}.",ign.comp.rex,"\\p{Zs}])([^\\p{Zs}])"), "\\1 \\2", tkn, perl=TRUE)
            # keep "..." intact
            tkn <- gsub("([^\\p{L}\\p{M}\\p{N}.\\s])([.])", "\\1 \\2", tkn, perl=TRUE)
          } else {}
        } else {}
        new.tkn <- unlist(strsplit(tkn, " "))
        # remove empty elements
        new.tkn <- new.tkn[!new.tkn %in% ""]
        return(new.tkn)
      }
    }, USE.NAMES=FALSE)))

  if(isTRUE(tag)){
    tokenized.text <- taggz(tokenized.text, abbrev=abbrev.vect, heur.fix=heur.fix, sntc=sntc, ign.comp=ign.comp)
  } else {}

  return(tokenized.text)
} ## end function tokenz()


## function queryList()
queryList <- function(obj, var, query, rel, as.df, ignore.case, perl){
  this.query <- query[[1]]
  this.query.vars <- names(this.query)
  this.q.var <- this.query.vars[[1]]
  this.q.query <- this.query[[1]]
  this.q.rel <- ifelse("rel" %in% this.query.vars, this.query[["rel"]], rel)
  this.q.ignore.case <- ifelse("ignore.case" %in% this.query.vars, this.query[["ignore.case"]], ignore.case)
  this.q.perl <- ifelse("perl" %in% this.query.vars, this.query[["perl"]], perl)
  if(length(query) == 1){
    obj <- query(obj=obj, var=this.q.var, query=this.q.query, rel=this.q.rel, as.df=as.df, ignore.case=this.q.ignore.case, perl=this.q.perl)
  } else {
    remaining.queries <- query[-1]
    remaining.obj <- query(obj=obj, var=this.q.var, query=this.q.query, rel=this.q.rel, as.df=FALSE, ignore.case=this.q.ignore.case, perl=this.q.perl)
    obj <- query(obj=remaining.obj, var=var, query=remaining.queries, rel=rel, as.df=as.df, ignore.case=ignore.case, perl=perl)
  }
  return(obj)
} ## end function queryList()


## function headLine()
# takes text and builds a border around it
headLine <- function(txt, level=1){
  if(identical(level, 1)){
    headlineTxt <- paste0("## ", txt, " ##")
    headlineLine <- paste(rep("#", nchar(headlineTxt)), collapse="")
    headlineFull <- paste0(headlineLine, "\n", headlineTxt, "\n", headlineLine)
  } else if(identical(level, 2)){
    headlineLine <- paste(rep("=", nchar(txt)), collapse="")
    headlineFull <- paste0(txt, "\n", headlineLine)
  } else {
    headlineLine <- paste(rep("-", nchar(txt)), collapse="")
    headlineFull <- paste0(txt, "\n", headlineLine)
  }
  return(headlineFull)
} ## end function headLine()


## function matching.lang()
# helper function to match language definitions,
# called by treetag()
matching.lang <- function(lang, lang.preset){
  if(!identical(lang, lang.preset)){
    warning(
      "Language \"",lang,"\" doesn't match the preset \"", lang.preset,"\". If you run into errors, you have been warned!",
      call.=FALSE
    )
  } else {}
}
## end function matching.lang()


## function paste.tokenized.text()
paste.tokenized.text <- function(txt){
  # put all text together
  all.text <- paste(txt, collapse=" ")
  # remove superfluous spaces
  all.text <- gsub("([[:space:]]{1})([\\(\\[\\{])([[:space:]]{1}|$)", " \\2", all.text, perl=TRUE)
  all.text <- gsub("([[:space:]]{1})([\\)\\]\\}])([[:space:]]{1}|$)", "\\2 ", all.text, perl=TRUE)
  all.text <- gsub("([[:space:]]{1})([,;.:])([[:space:]]{1}|$)", "\\2 ", all.text, perl=TRUE)
}
## end function paste.tokenized.text()


## function checkLangPreset()
# checks if a given language preset is defined at all, and either returns TRUE/error or the full preset definition
checkLangPreset <- function(preset, returnPresetDefinition=TRUE){
  # koRpus dropped support for non-UTF-8 presets and renamed former presets omitting the "-utf8" suffix
  # to not break compatibility, we'll just gracefully remove the suffix
  if(grepl("utf8", preset)){
    preset <- gsub("-utf8$", "", preset)
    warning(paste0("UTF-8 is now the default encoding, please rename your preset from \"", preset, "-utf8\" into just \"", preset, "\"!"), call.=FALSE)
  } else {}
  preset.definition <- as.list(as.environment(.koRpus.env))[["langSup"]][["treetag"]][["presets"]][[preset]]
  if(isTRUE(returnPresetDefinition)){
    if(is.null(preset.definition)){
      stop(simpleError(paste0("Manual TreeTagger configuration: \"", preset, "\" is not a valid preset!")))
    } else {
      return(preset.definition)
    }
  } else {
    return(ifelse(is.null(preset.definition), stop(simpleError(paste0("Manual TreeTagger configuration: \"", preset, "\" is not a valid preset!"))), TRUE))
  }
}
## end function checkLangPreset()


## function checkTTOptions()
# this helper function does some basic validity checks on provided TT.options
# if all goes well, returns a named list with valid settings
# if manual.config=FALSE returns an empty list because options are omitted anyway
checkTTOptions <- function(TT.options, manual.config, TT.tknz=TRUE){
  result <- list()
  if(!is.null(TT.options) & !is.list(TT.options)){
    warning("You provided \"TT.options\", but not as a list!")
  } else {}
  optNames <- names(TT.options)

  if(isTRUE(manual.config)){
    # basic check for valid element names
    validOptions <- c(
      "path",
      "preset",
      "tokenizer",
      "tknz.opts",
      "pre.tagger",
      "tagger",
      "abbrev",
      "params",
      "lexicon",
      "lookup",
      "filter",
      "no.unknown",
      "splitter",
      "splitter.opts"
    )
    undefined.options <- !optNames %in% validOptions
    if(any(undefined.options)){
      stop(simpleError(paste0(
        "You used undefined names in TT.options:\n  \"",
        paste0(optNames[undefined.options], collapse="\", \""),
        "\""
      )))
    } else {}

    if(!"path" %in% optNames){
      stop(simpleError("Manual TreeTagger configuration demanded, but not even a path was defined!"))
    } else {
      # specify basic paths
      result[["TT.path"]] <- TT.options[["path"]]
      result[["TT.bin"]] <- file.path(result[["TT.path"]],"bin")
      result[["TT.cmd"]] <- file.path(result[["TT.path"]],"cmd")
      result[["TT.lib"]] <- file.path(result[["TT.path"]],"lib")
      # check if this is really a TreeTagger root directory
      sapply(c(result[["TT.bin"]], result[["TT.cmd"]], result[["TT.lib"]]), function(chk.dir){check.file(chk.dir, mode="dir")})
    }

    # basic options, cannot be toyed with
    result[["TT.opts"]] <- "-token -lemma -sgml -pt-with-lemma -quiet"
    # allow some dedicated options to be set without jeopardizing the output format
    if(!is.null(TT.options[["no.unknown"]])){
      result[["TT.opts"]] <- ifelse(
        isTRUE(TT.options[["no.unknown"]]),
        paste0(result[["TT.opts"]], " -no-unknown"),
        result[["TT.opts"]]
      )
    } else {}

    if(!is.null(TT.options[["preset"]])){
      result[["preset"]] <- checkLangPreset(preset=TT.options[["preset"]])
    } else {
      # if no preset was defined, we need some more information
      if(isTRUE(TT.tknz)){
        needed.options <- c("tokenizer", "tagger", "params")
      } else {
        needed.options <- c("tagger", "params")
      }
      missing.options <- !needed.options %in% optNames
      if(any(missing.options)){
        stop(simpleError(paste0(
          "Manual TreeTagger configuration demanded, but not enough optinons given!\n  Missing options: \"",
          paste0(needed.options[missing.options], collapse="\", \""),
          "\""
        )))
      } else {}
    }
  } else {}

  return(result)
} ## end function checkTTOptions()


## function winPath()
# all of sudden, the constructions of file paths stopped working for some windows users,
# so we're forced to do something really, really ugly and replace all R-like "/"
# file separators with the windows-like "\\" manually.
winPath <- function(path){
  return(gsub("/", "\\\\", path))
}
# just for the record: i really *hate* windows!
## end function winPath()


## function check_lang_packages()
# checks what koRpus.lang.* packages are currently installed or loaded
# returns a named list with a list for each installed package, providing
# entries named "available", "installed", "loaded", and "title"
# availabe: also check for all available packages in 'repos'
# available.only: omit all installed packages which cannot be found in 'repos'
#' @importFrom utils available.packages packageDescription
check_lang_packages <- function(
  available=FALSE,
  repos="https://undocumeantit.github.io/repos/l10n/",
  available.only=FALSE,
  pattern="^koRpus.lang.*"
){
  ### this function should be kept close to identical to the respective function
  ### in the 'sylly' package, except for the pattern
  result <- list()
  if(isTRUE(available)){
    available_packages <- utils::available.packages(repos=repos)
    available_koRpus_lang <- grepl(pattern, available_packages[,"Package"])
    supported_lang <- unique(available_packages[available_koRpus_lang,"Package"])
  } else {
    available_koRpus_lang <- FALSE
    supported_lang <- NULL
  }

  loaded_packages <- loadedNamespaces()
  loaded_koRpus_lang <- grepl(pattern, loaded_packages)
  installed_packages <- unique(dir(.libPaths()))
  installed_koRpus_lang <- grepl(pattern, installed_packages)

  have_koRpus_lang <- any(installed_koRpus_lang, available_koRpus_lang)

  if(isTRUE(have_koRpus_lang)){
    if(isTRUE(available.only)){
      all_packages <- supported_lang
    } else {
      all_packages <- unique(c(installed_packages[installed_koRpus_lang], supported_lang))
    }
    for (this_package in all_packages){
      result[[this_package]] <- list(available=NA, installed=FALSE, loaded=FALSE, title="(unknown)")
      if(all(isTRUE(available), this_package %in% supported_lang)){
        result[[this_package]][["available"]] <- TRUE
      } else {}
      if(this_package %in% unique(installed_packages[installed_koRpus_lang])){
        result[[this_package]][["installed"]] <- TRUE
        this_package_index <- which.min(!installed_packages %in% this_package)
        result[[this_package]][["title"]] <- utils::packageDescription(installed_packages[this_package_index])[["Title"]]
      } else {}
      if(this_package %in% unique(loaded_packages[loaded_koRpus_lang])){
        result[[this_package]][["loaded"]] <- TRUE
      } else {}
    }
  } else {}
  
  return(result)
} ## end function check_lang_packages()
