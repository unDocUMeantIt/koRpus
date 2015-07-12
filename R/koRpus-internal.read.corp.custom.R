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

# these internal functions do the real corpus import,
# so they're mostly called by read.corp.custom()


## function kRp.corp.custom.prepare()
# prepare data to feed to internal functions
# called by kRp.read.corp.custom.calc(), see below
kRp.corp.custom.prepare <- function(corpus, format="file", tagger="kRp.env", force.lang=NULL, caseSens=TRUE, ...){
  if(inherits(corpus, "kRp.tagged")){
    tokens <- slot(corpus, "TT.res")[["token"]]
    tokenizedTexts <- list(corpus)
    tokensList <- list(vector=tokens)
  } else if(is.list(corpus)) {
    tokenizedTexts <- corpus
    tokensList <- lapply(tokenizedTexts, function(this.tagged.txt){
        return(taggedText(this.tagged.txt)[["token"]])
      })
    tokens <- unlist(tokensList)
  } else {
    # for inverse document frequency we need statistics per document,
    # therefore keep a list with individual results and flatten that later
    if(identical(format, "file") && check.file(corpus, mode="dir", stopOnFail=FALSE)){
      txt.files <- dir(corpus)
      tokenizedTexts <- lapply(txt.files, function(this.txt.file){
          txt.full.path <- file.path(corpus, this.txt.file)
          return(tag.kRp.txt(txt=txt.full.path, tagger=tagger, lang=force.lang, objects.only=FALSE, format=format, ...))
        })
      tokensList <- lapply(tokenizedTexts, function(this.tagged.txt){
          return(taggedText(this.tagged.txt)[["token"]])
        })
      names(tokensList) <- txt.files
      tokens <- unlist(tokensList)
    } else {
      tokenizedTexts <- list(tag.kRp.txt(txt=corpus, tagger=tagger, lang=force.lang, objects.only=FALSE, format=format, ...))
      tokens <- taggedText(tokenizedTexts[[1]])[["token"]]
      tokensList <- list(vector=tokens)
    }
  }
  
  if(!isTRUE(caseSens)){
    tokens <- tolower(tokens)
    tokensList <- lapply(tokensList, tolower)
  } else {}

  results <- list(tokens=tokens, tokensList=tokensList, tokenizedTexts=tokenizedTexts)
  return(results)
} ## end function kRp.corp.custom.prepare()


## function kRp.corp.custom.analysis()
# calculate basic frequencies
# called by kRp.read.corp.custom.calc(), see below
kRp.corp.custom.analysis <- function(tokens, quiet=TRUE){
  
  # this can be handled quick if quiet=TRUE, by using table()
  if(isTRUE(quiet)){
    freq.df <- frqcy.of.types(tokens=tokens, byTypes=TRUE, byTokens=FALSE)
    types <- freq.df[["byTypes"]][["type"]]
    num.tokens <- sum(freq.df[["byTypes"]][["freq"]])
    num.types <- nrow(freq.df[["byTypes"]])
    corp.freq <- matrix(
      data=c(types, freq.df[["byTypes"]][["freq"]]),
      ncol=2, dimnames=list(c(), c("word", "freq")))
  } else {
    # get types
    types <- unique(tokens)
    num.tokens <- length(tokens)
    num.types <- length(types)

    ## now do the counting!
    corp.freq <- matrix(ncol=2, dimnames=list(c(), c("word", "freq")))[-1,]
    type.counter <- 1
    for (tp in types){
      cat(paste0("\t", floor(100 * type.counter / num.types), "% complete, processing token ", type.counter, " of ", num.types, ": \"", tp, "\""))
      type.freq <- sum(match(tokens, tp), na.rm=TRUE)
      if(!isTRUE(quiet)){
        cat(paste0(" (found ", type.freq, " times in ", num.tokens, " tokens)\n"))
      } else {}
      corp.freq <- rbind(corp.freq, c(word=tp, freq=type.freq))
      type.counter <- type.counter + 1
    }
  }

  results <- list(types=types, num.types=num.types, num.tokens=num.tokens, corp.freq=corp.freq)
  return(results)
} ## end function kRp.corp.custom.analysis()


## function kRp.idf()
# calculate inverse document frequency
# log(total number of documents / number of documents containing type)
# tokensList is a list with one entry for each document
# called by kRp.read.corp.custom.calc(), see below
kRp.idf <- function(freq.obj, tokensList, log.base=10) {
  corp.freq <- freq.obj[["corp.freq"]]
  types <- freq.obj[["types"]]
  numTypes <- freq.obj[["num.types"]]
  numDocs <- length(tokensList)

  # first create a matrix with types (rows) by documents
  inDocsMatrix <- sapply(tokensList, function(this.doc){
    return(types %in% this.doc)
  })
  rownames(inDocsMatrix) <- types
  # now count how often each type was present in the documents
  inDocsNum <- rowSums(inDocsMatrix)
  # and finally the idf
  idf <- log(numDocs / inDocsNum, base=log.base)
  corp.freq <- cbind(corp.freq, inDocs=inDocsNum, idf=idf)
  
  freq.obj[["corp.freq"]] <- corp.freq
  return(freq.obj)
} ## end function kRp.idf()


##################################################################
## if this signature changes, check read.corp.custom() as well! ##
##################################################################

## function kRp.read.corp.custom.calc()
# this is the *actual* helper function that is caled by methods
kRp.read.corp.custom.calc <- function(corpus, format="file", quiet=TRUE, caseSens=TRUE, log.base=10,
    tagger="kRp.env", force.lang=NULL, ...){

  # prepare data
  data <- kRp.corp.custom.prepare(corpus=corpus, format=format, tagger=tagger,
    force.lang=force.lang, caseSens=caseSens, ...)
  # basic frequencies
  freq.obj <- kRp.corp.custom.analysis(tokens=data[["tokens"]], quiet=quiet)
  # idf  
  freq.obj <- kRp.idf(freq.obj=freq.obj, tokensList=data[["tokensList"]], log.base=log.base)

  corp.freq <- freq.obj[["corp.freq"]]
  # add wclass, lemma and tag info from tagged objects
  allExtraInfo <- subAllExtraInfo <- data.frame(token=NA, tag=NA, lemma=NA, wclass=NA)[-1,]
  for (thisText in data[["tokenizedTexts"]]){
    allExtraInfo <- rbind(allExtraInfo, taggedText(thisText)[,c("token","tag", "lemma", "wclass")])
  }
  if(!isTRUE(caseSens)){
    allExtraInfo[["token"]] <- tolower(allExtraInfo[["token"]])
  } else {}
  # only leave one entry per token, take the one with the most frequent wclass
  # it's a bit bogus, since identical tokens can be of different meaning, but
  # there's not so much we can do about it here
  for (thisToken in corp.freq[,"word"]) {
    subThisToken <- allExtraInfo[allExtraInfo[["token"]] == thisToken,]
    # the most frequent word class for this token
    thisTokenWclassFreq <- names(sort(table(subThisToken[["wclass"]]), decreasing=TRUE))[1]
    subAllExtraInfo <- rbind(subAllExtraInfo, subThisToken[which(subThisToken[["wclass"]] == thisTokenWclassFreq)[1],])
  }
  corp.freq <- cbind(corp.freq, subAllExtraInfo[, c("tag", "lemma", "wclass")], stringsAsFactors=FALSE)
  # sort the matrix
  corp.freq <- corp.freq[order(as.numeric(corp.freq[,"freq"]), decreasing=TRUE), ]
  # add num variable
  corp.freq <- cbind(num=1:freq.obj[["num.types"]], corp.freq)

  # descriptive statistics
  dscrpt.meta <- data.frame(
    tokens=freq.obj[["num.tokens"]],
    types=freq.obj[["num.types"]],
    words.p.sntc=NA,
    chars.p.sntc=NA,
    chars.p.wform=NA,
    chars.p.word=NA)

  # call internal function create.corp.freq.object()
  results <- create.corp.freq.object(matrix.freq=corp.freq,
            num.running.words=freq.obj[["num.tokens"]],
            df.meta=as.data.frame(matrix(ncol=2, dimnames=list(c(),c("meta", "value")))),
            df.dscrpt.meta=dscrpt.meta)

  return(results)
} ## end function kRp.read.corp.custom.calc()
