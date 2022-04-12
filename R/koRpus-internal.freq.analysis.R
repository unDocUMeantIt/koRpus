# Copyright 2010-2022 Meik Michalke <meik.michalke@hhu.de>
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

# this internal function does the real frequency analysis,
# so it's mostly called by freq.analysis()

###############################################################
## if this signature changes, check freq.analysis() as well! ##
###############################################################

## function kRp.freq.analysis.calc()
kRp.freq.analysis.calc <- function(
  txt.file,
  corp.freq=NULL,
  desc.stat=TRUE,
  corp.rm.class="nonpunct",
  corp.rm.tag=c()
){
  lang <- language(txt.file)

  if(identical(corp.rm.class, "nonpunct")){
    corp.rm.class <- kRp.POS.tags(lang, tags=c("punct","sentc"), list.classes=TRUE)
  } else {}

  if(all(hasFeature(txt.file, "corp_freq"), is.null(corp.freq))){
    corp.freq <- corpusCorpFreq(txt.file)
  } else {}

  if(!is.null(corp.freq)){
    # before we even start, check if we're alright:
    stopifnot(inherits(corp.freq, "kRp.corp.freq"))
    frequency.pre <- text.freq.analysis(
      txt.commented=taggedText(txt.file),
      corp.freq=corp.freq,
      corp.rm.class=corp.rm.class,
      corp.rm.tag=corp.rm.tag,
      lang=lang
    )
    # commented will be overwritten with a new version containing percentages for each word
    taggedText(txt.file) <- frequency.pre[["commented"]]
    corpusFreq(txt.file) <- frequency.pre[["freq.analysis"]]
    if(isTRUE(desc.stat)){
      describe(txt.file) <- text.analysis(frequency.pre[["commented"]], lang=lang, corp.rm.class=corp.rm.class, corp.rm.tag=corp.rm.tag, desc=describe(txt.file))
    } else {}
  } else {
    corpusFreq(txt.file) <- list(NA)
    # nothing to update here for describe()
  }

  return(txt.file)
} ## end function kRp.freq.analysis.calc()


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
  txt.commented <- cbind(txt.commented, rel.col=txt.rel.all)

  # now let's do some calculations...
  frqcy.summary <- frqcy.summarize(txt.rel.all)

  # there are probably some NAs in our text. they're the result of words not found
  # in the language corpus, hence we'll compute another summary and assume NA equals to probabilty of 0
  txt.rel.noNAs <- txt.rel.all
  frqcy.summary.noNAs <- frqcy.summarize(txt.rel.noNAs)
  # if defined, remove entries of certain classes from the word list
  txt.dropped.classes <- droplevels(subset(txt.commented, !wclass %in% corp.rm.class)) 
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
text.freq.analysis <- function(
  txt.commented,
  corp.freq,
  corp.rm.class,
  corp.rm.tag,
  lang
){
  # to avoid needless NOTEs from R CMD check
  wclass <- NULL

  stopifnot(inherits(corp.freq, "kRp.corp.freq"))

  frq.data.rel <- c("pmio", "log10", "rank.rel.avg", "rank.rel.min", "idf")
  frq.data <- lapply(
    frq.data.rel,
    function(this_rel){
      return(
        frqcy.by.rel(
          txt.commented,
          corp.freq=corp.freq,
          corp.rm.class=corp.rm.class,
          corp.rm.tag=corp.rm.tag,
          rel=this_rel
        )
      )
    }
  )
  names(frq.data) <- frq.data.rel
  # frequency of types in this document
  type.freq <- frqcy.of.types(tokens=txt.commented[["token"]], byTypes=FALSE, byTokens=TRUE)
  tfidf <- type.freq[["byTokens"]][["freq"]] * frq.data[["idf"]][["rel.col"]]

  txt.commented[["pmio"]] <- frq.data[["pmio"]][["rel.col"]]
  txt.commented[["log10"]] <- frq.data[["log10"]][["rel.col"]]
  txt.commented[["rank.avg"]] <- frq.data[["rank.rel.avg"]][["rel.col"]]
  txt.commented[["rank.min"]] <- frq.data[["rank.rel.min"]][["rel.col"]]
  txt.commented[["tf"]] <- type.freq[["byTokens"]][["freq"]]
  txt.commented[["idf"]] <- frq.data[["idf"]][["rel.col"]]
  txt.commented[["tfidf"]] <- tfidf
            
  # information on words-per-sentence and commas-per-sentence
  num.sentences <- sum(txt.commented[["wclass"]] %in% kRp.POS.tags(lang, tags="sentc", list.classes=TRUE))
  num.commas    <- sum(txt.commented[["wclass"]] %in% "comma")
  freq.commas   <- num.commas/num.sentences
  num.words     <- sum(!txt.commented[["wclass"]] %in% kRp.POS.tags(lang, tags=c("punct","sentc"), list.classes=TRUE))
  freq.words    <- num.words/num.sentences
  freq.w.p.c    <- num.words/num.commas
  res.sentences <- data.frame(words.p.sntc=freq.words, comma.p.sntc=freq.commas, words.p.comma=freq.w.p.c)

  freq.analysis <- list(
    frq.pmio=frq.data[["pmio"]][-c(1,2)],
    frq.log10=frq.data[["log10"]][-c(1,2)],
    frq.rank.avg=frq.data[["rank.rel.avg"]][-c(1,2)],
    frq.rank.min=frq.data[["rank.rel.min"]][-c(1,2)],
    sentence.factors=res.sentences
  )

  results <- list(commented=txt.commented, freq.analysis=freq.analysis)

  return(results)
} ## end function text.freq.analysis()
