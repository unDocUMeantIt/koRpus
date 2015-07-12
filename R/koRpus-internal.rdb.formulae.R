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


# this internal function does the real readability calculations,
# so it's mostly called by readability()

## TODO:
# lit:
# linsear write
# strain

# Fry Graph
# Raygor Estimate Graph
  # http://en.wikipedia.org/wiki/Raygor_Readability_Estimate
#  - harris-jacobson wide range (noch keine formel)
#  - dolch sight words suite (noch keine formel)
#  - mcalpine EFLAW
# devereaux (smith, s. kane)

#  - grade eqivalent zum DRP

## additional options:
# - analyze.text:
#     TRUE for kRp.rdb.formulae(), i.e. get values from text object examination,
#     FALSE to calculate with the values in txt.features
# - txt.features: a named list with key values needed to calculate

kRp.rdb.formulae <- function(txt.file=NULL,
      hyphen=NULL,
      index=c(),
      parameters=list(),
      word.lists=list(),
      fileEncoding="UTF-8",
      tagger=NULL,
      force.lang=NULL,
      sentc.tag="sentc",
      nonword.class="nonpunct",
      nonword.tag=c(),
      quiet=FALSE, 
      analyze.text=TRUE,
      txt.features=list(), ...){

  ## TODO: validation
  # the following implementations have already been checked against various tools
  # to validate the correctness of calculation. this doesn't mean they always came to identical
  # results at once, since the accuracy of input data (like number of syllables or sentences)
  # varies considerably. but if these differences were manually corrected, the results were similar:
  # - ARI                   [OUT, RDS, FRT]
  #   - NRI                 [RDS (labeled as "simplified")]
  # - Bormuth Mean Cloze    [RDS]
  # - Coleman-Liau          [OUT, RDS]
  # - Dale-Chall            [RDS]
  #   - PSK                 [RDS]
  #   - Dale-Chall (1948)   [OKP]
  # - DRP                   [RDS]
  # - Farr-Jenkins-Paterson [RDS]
  #   - PSK                 [RDS]
  # - Flesch                [OUT, RDS, TAL, LLB, JRT]
  #   - PSK                 [RDS]
  #   - Szigriszt (es)      [INF]
  #   - Flesch.es           [INF]
  # - Flesch-Kincaid        [OUT, RDS, JRT]
  # - FOG                   [GFI, RDS, OUT, JRT]
  #   - PSK                 [RDS]
  # - FORCAST               [RDS]
  # - Harris-Jacobson (HJ2) [RDS]
  # - LIX                   [RDS]
  # - Linsear Write         [FRT]
  # - RIX                   [RDS]
  # - SMOG                  [OUT, RDS]
  # - Spache                [RDS]
  # - Wheeler-Smith         [RDS, WHE]
  #
  # these measures produce plausible results, but need checking:
  # - ARI simplified
  # - Coleman Formulas (1-4)
  # - Danielson-Bryan (1-2)
  # - Dickes-Steiwer's Handformel
  # - Easy Listening Formula
  # - Flesch.de
  # - Flesch.fr
  # - Flesch.nl
  # - Fucks
  # - Harris-Jacobson (1-5)
  # - Neue Wiener Sachtextformeln (1-4)
  # - SMOG Qu
  # - SMOG C
  # - Strain
  # - Traenkle-Bailer
  #
  # these measures look bogus:
  # - TRI
  #
  # tools used:
  # FRT: http://www.readabilityformulas.com/free-readability-formula-tests.php
  # GFI: http://gunning-fog-index.com
  # INF: INFLESZ v1.0, http://www.legibilidad.com/home/descargas.html
  # JRT: http://juicystudio.com/services/readability.php
  # LLB: http://www.leichtlesbar.ch
  # OKP: http://www.lefthandlogic.com/htmdocs/tools/okapi/okapi.php
  # OUT: http://www.online-utility.org/english/readability_test_and_improve.jsp
  # RDS: Readability Studio, version 3.2.7.0 (14 jan 2011)
  # TAL: http://www.textalyser.net
  # 
  # other:
  # WHE: example from original article by Wheeler & Smith

  ## set default parameters as fallback
  # default.params() is defined in koRpus-internal.readability.R
  default.parameters <- default.params()
  # see if just the default parameters should be returned:
  if(identical(parameters, "dput")){
    return(dput(default.params, control="useSource"))
  } else {}

  # this function tries to get the actual word vector out of a given
  # word list, be it a vector, matrix, data.frame or file name
  read.word.list <- function(word.list){
    if(length(word.list) == 1 && is.numeric(word.list)){
        # we got a number, seems to be no word list, but already the result
        return(word.list)
      } else {}
    if(((is.data.frame(word.list) | is.matrix(word.list)) & isTRUE(ncol(word.list) == 1)) |
      (is.vector(word.list) & length(word.list) > 1)){
      local.word.list <- as.character(unlist(word.list))
    } else if(check.file(word.list, mode="exist")){
      wl.file.con <- file(word.list, open="r", encoding=fileEncoding)
      local.word.list <- as.character(unlist(readLines(wl.file.con)))
      close(wl.file.con)
    }
    return(local.word.list)
  }

  # this part calculates the percentage of words in the analyzed text
  # that are not found in a given list of words. this list must be
  # a data.frame/matrix with one column or a vector.
  difficult.words <- function(words.only, word.list, only.once=FALSE){
    # define the built-in word lists
    # it's not enough to have the data file around
    ## this much doesn't work as of now, we can't ship any word list due to copyright restrictions
    # the code is left intact though, should we get permission to include word lists in the future...
    if(length(word.list) == 1){
      if(is.numeric(word.list)){
        # we got numbers, seems to be no word list, but almost already the result
        if(!is.numeric(words.only)){
          # ok, this is not a number, so i guess it's actual words, let's simply count them
          words.only <- length(words.only)
        } else {}
        pct.not.listed <- word.list * 100 / words.only
        result <- list(words.not.listed=word.list, num.not.listed=word.list, pct.not.listed=pct.not.listed,
          num.listed=(words.only-word.list), pct.listed=(100-pct.not.listed))
        return(result)
      } else if(word.list %in% c(
        # list internal lists here ...
        "some.free.word.list")){
          wlist.to.load <- parse(text=paste0("if(!exists(\"", word.list, "\", envir=.koRpus.env, inherits=FALSE)){
            data(", word.list, ", package=\"koRpus\", envir=.koRpus.env)} else {}
            local.word.list <- as.vector(unlist(get(\"", word.list, "\", envir=.koRpus.env)))"))
          eval(wlist.to.load)
      } else {
        stop(simpleError(paste0("Invalid word list: \"", word.list, "\".\nPlease provide a valid file name, or a data.frame/matrix with one column, or a character vector!")))
      }
    } else if((is.vector(word.list)) & length(word.list) > 1){
      local.word.list <- word.list
    } else {
      stop(simpleError(paste0("Invalid word list: \"", word.list, "\".\nPlease provide a valid file name, or a data.frame/matrix with one column, or a character vector!")))
    }
    # make sure it's a vector, and we don't care about cases
    local.word.list <- tolower(local.word.list)
    local.tokens <- tolower(words.only)
    # Space counts unfamiliar words only once
    if(isTRUE(only.once)){
      words.not.listed <- unique(words.only[!local.tokens %in% local.word.list])
      # number of tokens not on the list
      num.not.listed <- sum(!unique(local.tokens) %in% local.word.list)
    } else {
      words.not.listed <- words.only[!local.tokens %in% local.word.list]
      # number of tokens not on the list
      num.not.listed <- sum(!local.tokens %in% local.word.list)
    }
    pct.not.listed <- num.not.listed * 100 / length(local.tokens)
    result <- list(words.not.listed=words.not.listed, num.not.listed=num.not.listed, pct.not.listed=pct.not.listed,
      num.listed=(length(local.tokens)-num.not.listed), pct.listed=(100-pct.not.listed))
    return(result)
  }

  long.words <- function(min.num, letters=FALSE){
    if(isTRUE(letters)){
      # will return the number of words with at least min.num characters!
      if(min.num %in% colnames(txt.desc[["lttr.distrib"]])){
        # can be that column name and numer are different, e.g. no words with three letters
        # therefore, we transform the number into character
        result <- txt.desc[["lttr.distrib"]]["cum.inv", as.character(min.num)]
      } else {
        result <- 0
      }
    } else {
      if(min.num %in% colnames(slot(hyphen, "desc")[["syll.distrib"]])){
        result <- slot(hyphen, "desc")[["syll.distrib"]]["cum.inv", as.character(min.num)]
      } else {
        result <- 0
      }
    }
    return(result)
  }

  check.flavour <- function(given, default, default.name="default"){
    if(identical(given, default)){
      return(default.name)
    } else {
      return(c("custom"))
    }
  }

  # check for given magic numbers
  if(!is.list(parameters) | identical(length(parameters), 0)){
    stop(simpleError("Missing accurate paramteter list!"))
  } else {
    # first complain if unknown parameters supplied
    valid.params <- c("ARI", "Bormuth", "Coleman", "Coleman.Liau",
      "Dale.Chall", "Danielson.Bryan", "Dickes.Steiwer",
      "ELF", "Farr.Jenkins.Paterson", "Flesch", "Flesch.Kincaid",
      "FOG", "FORCAST", "Harris.Jacobson", "Linsear.Write", "LIX", "nWS", "RIX",
      "SMOG", "Spache", "Strain", "Traenkle.Bailer", "TRI", "Tuldava", "Wheeler.Smith")
    kRp.check.params(names(parameters), valid.params, where="parameters")
  }

  all.valid.indices <- c("ARI", "ARI.NRI", "ARI.simple", "Bormuth", "Coleman", "Coleman.Liau",
      "Dale.Chall", "Dale.Chall.old", "Dale.Chall.PSK", "Danielson.Bryan",
      "Dickes.Steiwer", "DRP", "ELF", "Farr.Jenkins.Paterson", "Farr.Jenkins.Paterson.PSK",
      "Flesch", "Flesch.de", "Flesch.es", "Flesch.fr", "Flesch.Kincaid",
      "Flesch.nl", "Flesch.PSK", "Flesch.Szigriszt", "FOG", "FOG.NRI", "FOG.PSK", "FORCAST", "FORCAST.RGL",
      "Fucks", "Harris.Jacobson", "Linsear.Write", "LIX", "nWS", "RIX", "SMOG", "SMOG.C",
      "SMOG.de", "SMOG.simple", "Spache", "Spache.de", "Spache.old", "Strain", "Traenkle.Bailer", "TRI",
      "Tuldava", "Wheeler.Smith", "Wheeler.Smith.de")
  # activate all?
  if(identical(index, "all")){
    index <- all.valid.indices
  } else {}

  need.sylls <- c("Coleman", "ELF", "Farr.Jenkins.Paterson", "Farr.Jenkins.Paterson.PSK",
    "Flesch", "Flesch.de", "Flesch.es", "Flesch.fr", "Flesch.Kincaid",
    "Flesch.nl", "Flesch.PSK", "Flesch.Szigriszt", "FOG", "FOG.NRI", "FOG.PSK", "FORCAST", "FORCAST.RGL",
    "Linsear.Write", "nWS", "SMOG", "SMOG.C", "SMOG.de", "SMOG.simple",
    "Strain", "TRI", "Tuldava", "Wheeler.Smith", "Wheeler.Smith.de")


  if(isTRUE(analyze.text)){
    #######################
    ## analyze.text=TRUE ##
    #######################
    if("lang" %in% names(list(...))){
      # since 'lang' is a valid argument for treetag(), it might have been set
      stop(simpleError("You defined 'lang' in the '...' argument. This is confusing me! Use 'force.lang' instead."))
    } else {}
    # for backward compatibility
    if("treetagger" %in% names(list(...))){
      stop(simpleError("The option 'treetagger' is deprecated and was removed. Use 'tagger' instead."))
    } else {}

    if(inherits(txt.file, "kRp.tagged")){
      lang <- language.setting(as(txt.file,"kRp.tagged"), force.lang)
    } else if(!is.null(force.lang)){
      lang <- force.lang
    } else {
      lang <- get.kRp.env(lang=TRUE)
    }

    if(identical(nonword.class, "nonpunct")){
      nonword.class <- kRp.POS.tags(lang, tags=c("punct","sentc"), list.classes=TRUE)
    } else {}

    if(inherits(txt.file, "kRp.tagged")){
      txt.freq <- txt.file
      tagged.words.only <- kRp.filter.wclass(txt.freq, corp.rm.class=nonword.class, corp.rm.tag=nonword.tag)
      if(is.null(slot(txt.freq, "desc")$all.words)){
        slot(txt.freq, "desc")$all.words <- slot(tagged.words.only, "TT.res")[["token"]]
      } else {}
    } else {
      txt.freq <- freq.analysis(txt.file=txt.file, desc.stat=TRUE, force.lang=lang,
                tagger=tagger, corp.rm.class=nonword.class, corp.rm.tag=nonword.tag, ...)
      tagged.words.only <- kRp.filter.wclass(txt.freq, corp.rm.class=nonword.class, corp.rm.tag=nonword.tag)
    }
    txt.desc <- slot(txt.freq, "desc")
    # set objects.only=FALSE to enable automatic tagging if a file name is given
    tagged.text <- tag.kRp.txt(txt.file, lang=lang, objects.only=FALSE)

    # check how to handle the hyphen parameter
    # first see if there's results to re-use
    if("hyphen" %in% slotNames(txt.freq)){
      if(inherits(slot(txt.freq ,"hyphen"), "kRp.hyphen")){
        if(nrow(slot(slot(txt.freq ,"hyphen"), "hyphen")) > 0 && is.null(hyphen)){
          hyphen <- slot(txt.freq ,"hyphen")
        } else {}
      } else {}
    } else {}
    # we don't need hyphenation for certain formulas,
    # then we'll skip that step automatically
    if(any(index %in% need.sylls)){
      if(is.null(hyphen)){
        hyphen <- hyphen(tagged.text, corp.rm.class=nonword.class, corp.rm.tag=nonword.tag, quiet=quiet)
      } else {
        stopifnot(inherits(hyphen, "kRp.hyphen"))
      }
      sylls.available <- TRUE
    } else {
      if(inherits(hyphen, "kRp.hyphen")){
        sylls.available <- TRUE
      } else {
        # we'll just create an empty object to not disturb the other functions
        hyphen <- new("kRp.hyphen")
        sylls.available <- FALSE
      }
    }

    if(identical(sentc.tag, "sentc")){
      sentc.tag <- kRp.POS.tags(lang, tags="sentc", list.tags=TRUE)
    } else {}

    ## some global calculations, for use in several indices
    # we'll use some default minimum values for the number of sentences
    # and average sentence length
    txt.words.only <- txt.desc$all.words
    num.sentences <- ifelse(txt.desc$sentences < 1, 1, txt.desc$sentences)
    num.words <- txt.desc$words
    num.all.chars <- txt.desc$all.chars
    # txt.desc$letters had all digits removed
    num.letters <- txt.desc$letters[["all"]]
    lttr.distrib <- txt.desc$lttr.distrib
    fixed.letters <- txt.desc$letters
    avg.sntc.len <- ifelse(txt.desc$sentences < 1, num.words, txt.desc$avg.sentc.length)
    avg.word.len <- txt.desc$avg.word.length
    sntc.per.word <- num.sentences / num.words
    sntc.per100 <- sntc.per.word * 100
    lett.per100 <- num.letters * 100 / num.words
    num.punct <- txt.desc$punct
    if(isTRUE(sylls.available)){
      num.syll <- slot(hyphen, "desc")[["num.syll"]]
      num.monosyll <- slot(hyphen, "desc")[["syll.distrib"]]["num", 1]
      pct.monosyll <- num.monosyll * 100 / num.words
      syll.distrib <- slot(hyphen, "desc")[["syll.distrib"]]
      syll.uniq.distrib <- slot(hyphen, "desc")[["syll.uniq.distrib"]]
      avg.syll.word <- slot(hyphen, "desc")[["avg.syll.word"]]
      syll.per100 <- slot(hyphen, "desc")[["syll.per100"]]
      fixed.syllables <- distrib.to.fixed(syll.distrib, all.values=num.syll, idx="s")
    } else {
      num.syll <- NA
      num.monosyll <- NA
      pct.monosyll <- NA
      syll.distrib <- NA
      syll.uniq.distrib <- NA
      avg.syll.word <- NA
      syll.per100 <- NA
      fixed.syllables <- NULL
    }
    # check if the "Dickes.Steiwer" parameters are set for TTR
    DS.case.sens <- ifelse(
      is.null(parameters$Dickes.Steiwer$case.sens),
      default.parameters$Dickes.Steiwer$case.sens,
      parameters$Dickes.Steiwer$case.sens)
    num.TTR <- slot(TTR(tagged.text, case.sens=DS.case.sens, quiet=TRUE), "TTR")
    # some word classes needed by one formula or the other
    num.conjunctions <- sum(slot(tagged.text, "TT.res")$wclass %in% "conjunction", na.rm=TRUE)
    num.prepositions <- sum(slot(tagged.text, "TT.res")$wclass %in% "preposition", na.rm=TRUE)
    num.pronouns <- sum(slot(tagged.text, "TT.res")$wclass %in% "pronoun", na.rm=TRUE)
    num.foreign <- sum(slot(tagged.text, "TT.res")$wclass %in% "foreign", na.rm=TRUE)
  } else {
    ########################
    ## analyze.text=FALSE ##
    ########################
    # get needed values from txt.features
    txt.desc <- txt.features
    txt.words.only <- txt.features$words            # needed for measures with word lists
    num.sentences <- txt.features$sentences
    num.words <- txt.features$words
    num.all.chars <- txt.features$all.chars         # danielson-bryan
    num.letters <- txt.features$letters[["all"]]
    num.punct <- txt.features$punct
    lttr.distrib <- distrib.from.fixed(txt.features$letters, all.words=num.words, idx="l", unaccounted="x")
    txt.desc[["lttr.distrib"]] <- lttr.distrib
    fixed.letters <- txt.features$letters
    avg.sntc.len <- num.words / num.sentences
    avg.word.len <- num.letters / num.words
    sntc.per.word <- num.sentences / num.words
    sntc.per100 <- sntc.per.word * 100
    lett.per100 <- avg.word.len * 100
    num.syll <- txt.features$syllables[["all"]]
    num.monosyll <- txt.features$syllables[["s1"]]
    pct.monosyll <- num.monosyll * 100 / num.words
    syll.distrib <- distrib.from.fixed(txt.features$syllables, all.words=num.words, idx="s", unaccounted="x")
    syll.uniq.distrib <- txt.features$syll.uniq.distrib
    fixed.syllables <- txt.features$syllables
    avg.syll.word <- num.syll / num.words
    syll.per100 <- avg.syll.word * 100
    num.TTR <- txt.features$TTR
    num.conjunctions <- txt.features$conjunctions
    num.prepositions <- txt.features$prepositions
    num.pronouns <- txt.features$pronouns
    num.foreign <- txt.features$foreign
    if(inherits(txt.file, "kRp.tagged")){
      lang <- language.setting(as(txt.file,"kRp.tagged"), force.lang)
      tagged.text <- txt.file
    } else if(!is.null(force.lang)){
      lang <- force.lang
      tagged.text <- new("kRp.tagged")
    } else {
      lang <- character()
      tagged.text <- new("kRp.tagged")
    }
    if(!inherits(hyphen, "kRp.hyphen")){
      hyphen <- new("kRp.hyphen",
        desc=list(
          num.syll=num.syll,
          syll.distrib=syll.distrib,
          syll.uniq.distrib=NA,
          avg.syll.word=avg.syll.word,
          syll.per100=syll.per100
        ))
    } else {}
    txt.freq <- tagged.text
  }

  # keep the descriptives in the results
  desc <- list(
    sentences=num.sentences,
    words=num.words,
    letters=fixed.letters,
    all.chars=num.all.chars,
    syllables=fixed.syllables,
    lttr.distrib=lttr.distrib,
    syll.distrib=syll.distrib,
    syll.uniq.distrib=syll.uniq.distrib,
    punct=num.punct,
    conjunctions=num.conjunctions,
    prepositions=num.prepositions,
    pronouns=num.pronouns,
    foreign=num.foreign,
    TTR=num.TTR,
    avg.sentc.length=avg.sntc.len,
    avg.word.length=avg.word.len,
    avg.syll.word=avg.syll.word,
    sntc.per.word=sntc.per.word,
    sntc.per100=sntc.per100,
    lett.per100=lett.per100,
    syll.per100=syll.per100,
    FOG.hard.words=NULL,
    Bormuth.NOL=NULL,
    Dale.Chall.NOL=NULL,
    Harris.Jacobson.NOL=NULL,
    Spache.NOL=NULL
  )

  # initialize result object
  if(inherits(txt.file, "kRp.readability")){
    all.results <- txt.file
    slot(all.results, "lang") <- lang
    slot(all.results, "TT.res") <- slot(tagged.text, "TT.res")
    slot(all.results, "hyphen") <- hyphen
    slot(all.results, "desc") <- desc
    slot(all.results, "param") <- parameters
  } else {
    all.results <- new("kRp.readability",
      lang=lang,
      TT.res=slot(tagged.text, "TT.res"),
      desc=desc,
      hyphen=hyphen,
      param=parameters)
  }

  #####################################
  ## readability measures start here ##
  #####################################

  ## Automated Readability Index (ARI)
  if("ARI" %in% index){
    flavour <- "default"
    valid.params <- c("asl", "awl", "const")
    if("ARI" %in% names(parameters)){
      flavour <- check.flavour(parameters$ARI, default.parameters$ARI)
      prms <- parameters$ARI
      if(identical(prms, "NRI")){
        flavour <- "NRI"
        prms <- c(asl=0.4, awl=6, const=27.4)
      } else {}
      if(identical(prms, "simple")){
        flavour <- "simplified"
        prms <- c(asl=1, awl=9, const=0)
      } else {}
    } else {
      prms <- default.parameters$ARI
    }
    kRp.check.params(names(prms), valid.params, where="ARI")
    kRp.check.params(valid.params, names(prms), where="ARI", missing=TRUE)
    ARI.grade <- prms[["asl"]] * avg.sntc.len + prms[["awl"]] * avg.word.len - prms[["const"]]
    if(identical(flavour, "simplified")){
      slot(all.results, "ARI") <- list(flavour=flavour, index=ARI.grade, grade=NA)
    } else {
      slot(all.results, "ARI") <- list(flavour=flavour, index=NA, grade=ARI.grade)
    }
  } else{}
  # recursive calls for alternative shortcuts
  if("ARI.NRI" %in% index){
     # setting hyphen to NULL here, because otherwise we could end up with pseudo hyphen objects crashing the function
     slot(all.results, "ARI.NRI") <- slot(kRp.rdb.formulae(txt.freq, hyphen=NULL, index=c("ARI"), parameters=list(ARI="NRI"),
      fileEncoding=fileEncoding, tagger=tagger, force.lang=force.lang, sentc.tag=sentc.tag,
      nonword.class=nonword.class, nonword.tag=nonword.tag, analyze.text=analyze.text, txt.features=txt.features, quiet=TRUE), "ARI")
  } else {}
  if("ARI.simple" %in% index){
     # setting hyphen to NULL here, because otherwise we could end up with pseudo hyphen objects crashing the function
     slot(all.results, "ARI.simple") <- slot(kRp.rdb.formulae(txt.freq, hyphen=NULL, index=c("ARI"), parameters=list(ARI="simple"),
      fileEncoding=fileEncoding, tagger=tagger, force.lang=force.lang, sentc.tag=sentc.tag,
      nonword.class=nonword.class, nonword.tag=nonword.tag, analyze.text=analyze.text, txt.features=txt.features, quiet=TRUE), "ARI")
  } else {}

  ## Bormuth Grade Level
  if("Bormuth" %in% index | "DRP" %in% index ){
    # check for word list and add it to params, otherwise skip this index
    if(!"Bormuth" %in% names(word.lists) | is.null(word.lists[["Bormuth"]])){
      warning("Bormuth: Missing word list, hence not calculated.", call.=FALSE)
    } else {
      flavour <- "default"
      valid.params <- c("clz", "meanc", "grade")
      valid.params.meanc <- c("const", "awl", "afw", "asl1", "asl2", "asl3")
      valid.params.grade <- c("const", "m1", "m2", "m3", "c1", "c2", "c3", "mc1", "mc2", "mc3")
      bormuth.word.list <- read.word.list(word.lists[["Bormuth"]])
      if("Bormuth" %in% names(parameters)){
        flavour <- check.flavour(parameters$Bormuth, default.parameters$Bormuth)
        prms <- parameters$Bormuth
      } else {
        prms <- default.parameters$Bormuth
      }
      kRp.check.params(names(prms), valid.params, where="Bormuth")
      kRp.check.params("meanc", names(prms), where="Bormuth", missing=TRUE)
      prms.meanc <- prms$meanc
      kRp.check.params(names(prms.meanc), valid.params.meanc, where="Bormuth$meanc")
      kRp.check.params(valid.params.meanc, names(prms.meanc), where="Bormuth$meanc", missing=TRUE)
      fam.words.all.txt <- difficult.words(txt.words.only, bormuth.word.list)
      fam.words.txt <- fam.words.all.txt[["num.listed"]]/num.words
      fam.words.nol <- fam.words.all.txt[["words.not.listed"]]
      bmc <- prms.meanc[["const"]] - (prms.meanc[["awl"]] * avg.word.len) + (prms.meanc[["afw"]] * fam.words.txt^3) -
        (prms.meanc[["asl1"]] * avg.sntc.len) + (prms.meanc[["asl2"]] * avg.sntc.len^2) - (prms.meanc[["asl3"]] * avg.sntc.len^3)

      if("grade" %in% names(prms)) {
        prms.grade <- prms$grade
        kRp.check.params(names(prms.grade), valid.params.grade, where="Bormuth$grade")
        kRp.check.params(valid.params.grade, names(prms.grade), where="Bormuth$grade", missing=TRUE)
        if("clz" %in% names(prms)){
          cloze.crit <- prms[["clz"]] / 100
        } else {
          cloze.crit <- default.parameters$Bormuth[["clz"]] / 100
        }
        # here comes the rather long formula...
        bm.grade <- prms.grade[["const"]] + (prms.grade[["m1"]] * bmc) - (prms.grade[["m2"]] * bmc^2) + (prms.grade[["m3"]] * bmc^3) +
          (prms.grade[["c1"]] * cloze.crit) - (prms.grade[["c2"]] * cloze.crit^2) - (prms.grade[["c3"]] * cloze.crit^3) -
          (prms.grade[["mc1"]] * bmc * cloze.crit) + (prms.grade[["mc2"]] * (bmc * cloze.crit)^2) - (prms.grade[["mc3"]] * (bmc * cloze.crit)^3)
      } else {
        bm.grade <- NA
      }
      # preserve hard words in the desc slot
      if(isTRUE(analyze.text)){
        slot(all.results, "desc")[["Bormuth.NOL"]] <- length(fam.words.nol)
      } else {
        slot(all.results, "desc")[["Bormuth.NOL"]] <- bormuth.word.list
      }
      slot(all.results, "Bormuth") <- list(flavour=flavour, word.list=bormuth.word.list, not.on.list=fam.words.nol, pct.fam=fam.words.txt * 100, mean.cloze=bmc * 100, grade=bm.grade)
    }
  } else{}

  ## Coleman Formulas
  if("Coleman" %in% index){
    # this formula needs proper POS tags; skip if missing
    if(all(slot(tagged.text, "TT.res")[["tag"]] %in% kRp.POS.tags("kRp", list.tags=TRUE))){
      # this text was just tagged with tokenize() and misses important tags
      warning("Coleman: POS tags are not elaborate enough, can't count pronouns and prepositions. Formulae skipped.", call.=FALSE)
    } else {
      flavour <- "default"
      valid.params <- c("syll", "clz1", "clz2", "clz3", "clz4")
      if("Coleman" %in% names(parameters)){
        flavour <- check.flavour(parameters$Coleman, default.parameters$Coleman)
        prms <- parameters$Coleman
      } else {
        prms <- default.parameters$Coleman
      }
      kRp.check.params(names(prms), valid.params, where="Coleman")
      # we don't necessarily need all params
      kRp.check.params("syll", names(prms), where="Coleman", missing=TRUE)
      coleman.words   <- slot(hyphen, "desc")[["syll.distrib"]]["cum.sum", prms[["syll"]]] * 100 / num.words
      coleman.sentc   <- sntc.per100
      coleman.pronoun <- num.pronouns * 100 / num.words
      coleman.prepos  <- num.prepositions * 100 / num.words

      if(!any(c("clz1", "clz2", "clz3", "clz4") %in% names(prms))){
        stop(simpleError("Coleman: you need to specify parameters for at least *one* of the four formulas!"))
      }

      if("clz1" %in% names(prms)){
        prms.c1 <- prms$clz1
        valid.prms <- c("word", "const")
        kRp.check.params(names(prms.c1), valid.prms, where="Coleman$clz1")
        kRp.check.params(valid.prms, names(prms.c1), where="Coleman$clz1", missing=TRUE)
        Coleman1 <- (prms.c1[["word"]] * coleman.words) - prms.c1[["const"]]
      } else {
        Coleman1 <- NA
      }

      if("clz2" %in% names(prms)){
        prms.c2 <- prms$clz2
        valid.prms <- c("word", "sntc", "const")
        kRp.check.params(names(prms.c2), valid.prms, where="Coleman$clz2")
        kRp.check.params(valid.prms, names(prms.c2), where="Coleman$clz2", missing=TRUE)
        Coleman2 <- (prms.c2[["word"]] * coleman.words) + (prms.c2[["sntc"]] * coleman.sentc) - prms.c2[["const"]]
      } else {
        Coleman2 <- NA
      }

      if("clz3" %in% names(prms)){
        prms.c3 <- prms$clz3
        valid.prms <- c("word", "sntc", "pron", "const")
        kRp.check.params(names(prms.c3), valid.prms, where="Coleman$clz3")
        kRp.check.params(valid.prms, names(prms.c3), where="Coleman$clz3", missing=TRUE)
        Coleman3 <- (prms.c3[["word"]] * coleman.words) + (prms.c3[["sntc"]] * coleman.sentc) + (prms.c3[["pron"]] * coleman.pronoun) - prms.c3[["const"]]
      } else {
        Coleman3 <- NA
      }

      if("clz4" %in% names(prms)){
        prms.c4 <- prms$clz4
        valid.prms <- c("word", "sntc", "pron", "prep", "const")
        kRp.check.params(names(prms.c4), valid.prms, where="Coleman$clz4")
        kRp.check.params(valid.prms, names(prms.c4), where="Coleman$clz4", missing=TRUE)
        Coleman4 <- (prms.c4[["word"]] * coleman.words) + (prms.c4[["sntc"]] * coleman.sentc) + (prms.c4[["pron"]] * coleman.pronoun) - (prms.c4[["prep"]] * coleman.prepos) - prms.c4[["const"]]
      } else {
        Coleman4 <- NA
      }
      slot(all.results, "Coleman") <- list(flavour=flavour, num.pron=coleman.pronoun, num.prep=coleman.prepos, C1=Coleman1, C2=Coleman2, C3=Coleman3, C4=Coleman4)
    }
  } else {}

  ## Coleman-Liau
  if("Coleman.Liau" %in% index){
    flavour <- "default"
    valid.params <- c("ecp", "grade", "short")
    valid.params.ecp <- c("const", "char", "sntc")
    valid.params.grade <- c("ecp", "const")
    valid.params.short <- c("awl", "spw", "const")
    if("Coleman.Liau" %in% names(parameters)){
      flavour <- check.flavour(parameters$Coleman.Liau, default.parameters$Coleman.Liau)
      prms <- parameters$Coleman.Liau
    } else {
      prms <- default.parameters$Coleman.Liau
    }

    kRp.check.params(names(prms), valid.params, where="Coleman.Liau")
    if(sum(c("ecp", "grade") %in% names(prms)) != 2 & !isTRUE("short" %in% names(prms))) {
      stop(simpleError("Coleman.Liau: You need to specify either parameters for \"ecp\" & \"grade\", or \"short\"!"))
    }

    if("ecp" %in% names(prms)){
      prms.ecp <- prms$ecp
      prms.grade <- prms$grade
      kRp.check.params(names(prms.ecp), valid.params.ecp, where="Coleman.Liau$ecp")
      kRp.check.params(valid.params.ecp, names(prms.ecp), where="Coleman.Liau$ecp", missing=TRUE)
      kRp.check.params(names(prms.grade), valid.params.grade, where="Coleman.Liau$grade")
      kRp.check.params(valid.params.grade, names(prms.grade), where="Coleman.Liau$grade", missing=TRUE)
      co.li.ECP <- prms.ecp[["const"]] - (prms.ecp[["char"]] * lett.per100) + (prms.ecp[["sntc"]] * sntc.per100)
      co.li.grade <- (prms.grade[["ecp"]] * (co.li.ECP / 100)) + prms.grade[["const"]]
    } else {
      co.li.ECP <- NA
      co.li.grade <- NA
    }
    if("short" %in% names(prms)){
      prms.short <- prms$short
      kRp.check.params(names(prms.short), valid.params.short, where="Coleman.Liau$short")
      kRp.check.params(valid.params.short, names(prms.short), where="Coleman.Liau$short", missing=TRUE)
      co.li.short <- prms.short[["awl"]] * avg.word.len - prms.short[["spw"]] * sntc.per.word - prms.short[["const"]]
    } else {
      co.li.short <- NA
    }
    slot(all.results, "Coleman.Liau") <- list(flavour=flavour, ECP=co.li.ECP, grade=co.li.grade, short=co.li.short)
  } else{}

  ## Dale-Chall
  ## TODO: remove names and numbers, they're not counted as unfamiliar!
  if("Dale.Chall" %in% index){
    # check for word list and add it to params, otherwise skip this index
    if(!"Dale.Chall" %in% names(word.lists) | is.null(word.lists[["Dale.Chall"]])){
      warning("Dale-Chall: Missing word list, hence not calculated.", call.=FALSE)
    } else {
      flavour <- "default"
      valid.params <- c("const", "dword", "asl")
      dale.chall.word.list <- read.word.list(word.lists[["Dale.Chall"]])
      if("Dale.Chall" %in% names(parameters)){
        flavour <- check.flavour(parameters$Dale.Chall, default.parameters$Dale.Chall, default.name="New Dale-Chall (1995)")
        prms <- parameters$Dale.Chall
      } else {
        prms <- default.parameters$Dale.Chall
      }
      if(identical(prms, "PSK")){
        flavour <- "Powers-Sumner-Kearl"
        diff.words.all.txt <- difficult.words(txt.words.only, dale.chall.word.list)
        diff.words.txt <- diff.words.all.txt[["pct.not.listed"]]
        diff.words.nol <- diff.words.all.txt[["words.not.listed"]]
        dale.chall.raw <- 3.2672  + (0.0596 * avg.sntc.len) + (0.1155 * diff.words.txt)
        dale.chall.grade <- get.grade.level(dale.chall.raw, measure="Dale.Chall.PSK")
      } else if(identical(prms, "old")){
        flavour <- "Dale-Chall (1948)"
        diff.words.all.txt <- difficult.words(txt.words.only, dale.chall.word.list)
        diff.words.txt <- diff.words.all.txt[["pct.not.listed"]]
        diff.words.nol <- diff.words.all.txt[["words.not.listed"]]
        dale.chall.raw <- (0.1579 * diff.words.txt) + (0.0496 * avg.sntc.len) + 3.6365
        dale.chall.grade <- get.grade.level(dale.chall.raw, measure="Dale.Chall")
      } else {
        kRp.check.params(names(prms), valid.params, where="Dale.Chall")
        kRp.check.params(valid.params, names(prms), where="Dale.Chall", missing=TRUE)
        diff.words.all.txt <- difficult.words(txt.words.only, dale.chall.word.list)
        diff.words.txt <- diff.words.all.txt[["pct.not.listed"]]
        diff.words.nol <- diff.words.all.txt[["words.not.listed"]]
        dale.chall.raw <- prms[["const"]] - (prms[["dword"]] * diff.words.txt) - (prms[["asl"]] * avg.sntc.len)
        dale.chall.grade <- get.grade.level(dale.chall.raw, measure="Dale.Chall.new")
      }
      # preserve hard words in the desc slot
      if(isTRUE(analyze.text)){
        slot(all.results, "desc")[["Dale.Chall.NOL"]] <- length(diff.words.nol)
      } else {
        slot(all.results, "desc")[["Dale.Chall.NOL"]] <- dale.chall.word.list
      }
      slot(all.results, "Dale.Chall") <- list(flavour=flavour, word.list=dale.chall.word.list, not.on.list=diff.words.nol, pct=diff.words.txt, raw=dale.chall.raw,
      grade=dale.chall.grade[["grade"]], grade.min=dale.chall.grade[["grade.min"]],
      age=dale.chall.grade[["age"]], age.min=dale.chall.grade[["age.min"]])
    }
  } else{}
  # recursive calls for alternative shortcuts
  if("Dale.Chall.PSK" %in% index){
    slot(all.results, "Dale.Chall.PSK") <- slot(kRp.rdb.formulae(txt.freq, hyphen=hyphen, index=c("Dale.Chall"), parameters=list(Dale.Chall="PSK"),
      word.lists=list(Dale.Chall=word.lists[["Dale.Chall"]]),
      fileEncoding=fileEncoding, tagger=tagger, force.lang=force.lang, sentc.tag=sentc.tag,
      nonword.class=nonword.class, nonword.tag=nonword.tag, analyze.text=analyze.text, txt.features=txt.features, quiet=TRUE), "Dale.Chall")
  } else {}
  if("Dale.Chall.old" %in% index){
    slot(all.results, "Dale.Chall.old") <- slot(kRp.rdb.formulae(txt.freq, hyphen=hyphen, index=c("Dale.Chall"), parameters=list(Dale.Chall="old"),
      word.lists=list(Dale.Chall=word.lists[["Dale.Chall"]]),
      fileEncoding=fileEncoding, tagger=tagger, force.lang=force.lang, sentc.tag=sentc.tag,
      nonword.class=nonword.class, nonword.tag=nonword.tag, analyze.text=analyze.text, txt.features=txt.features, quiet=TRUE), "Dale.Chall")
  } else {}

  ## Danielson-Bryan
  # since after tokenization we cannot really count
  # the blanks any more, we'll assume one blank between
  # every two words, hence blanks = num.words - 1
  if("Danielson.Bryan" %in% index){
    flavour <- "default"
    valid.params <- c("db1", "db2")
    if("Danielson.Bryan" %in% names(parameters)){
      flavour <- check.flavour(parameters$Danielson.Bryan, default.parameters$Danielson.Bryan)
      prms <- parameters$Danielson.Bryan
    } else{
      prms <- default.parameters$Danielson.Bryan
    }

    kRp.check.params(names(prms), valid.params, where="parameters$Danielson.Bryan")
    # we don't necessarily need all params
    if(!any(c("db1", "db2") %in% names(prms))){
      stop(simpleError("Danielson.Bryan: you need to specify parameters for at least *one* of the two formulas!"))
    }

    db.num.blanks <- num.words - 1
    db.avg.let.blnk <- num.all.chars / db.num.blanks
    db.avg.let.sntc <- num.all.chars / num.sentences

    if("db1" %in% names(prms)){
      prms.db1 <- prms$db1
      valid.prms <- c("cpb", "cps", "const")
      kRp.check.params(names(prms.db1), valid.prms, where="parameters$Danielson.Bryan$db1")
      kRp.check.params(valid.prms, names(prms.db1), where="parameters$Danielson.Bryan$db1", missing=TRUE)
      DB1 <- (prms.db1[["cpb"]] * db.avg.let.blnk) + (prms.db1[["cps"]] * db.avg.let.sntc) - prms.db1[["const"]]
    } else {
      DB1 <- NA
    }

    if("db2" %in% names(prms)){
      prms.db2 <- prms$db2
      valid.prms <- c( "const", "cpb", "cps")
      kRp.check.params(names(prms.db2), valid.prms, where="parameters$Danielson.Bryan$db2")
      kRp.check.params(valid.prms, names(prms.db2), where="parameters$Danielson.Bryan$db2", missing=TRUE)
      DB2 <- prms.db2[["const"]] - (prms.db2[["cpb"]] * db.avg.let.blnk) - (prms.db2[["cps"]] * db.avg.let.sntc)
      DB2.grade <- get.grade.level(DB2, measure="Danielson.Bryan")
    } else {
      DB2 <- NA
      DB2.grade <- NA
    }

    slot(all.results, "Danielson.Bryan") <- list(flavour=flavour, avg.blank=db.avg.let.blnk, avg.sentc=db.avg.let.sntc,
      DB1=DB1, DB2=DB2, DB2.grade=DB2.grade[["grade"]], DB2.grade.min=DB2.grade[["grade.min"]])
  } else {}

  ## Degrees of Reading Power (DRP)
  if("DRP" %in% index){
    # check for word list and add it to params, otherwise skip this index
    if(identical(slot(all.results, "Bormuth"), list(NA))){
      warning("DRP: Missing Bormuth Mean Cloze, hence not calculated.", call.=FALSE)
    } else {
      # re-use the Bormuth score that we just calculated
      drp.final <- (1 - slot(all.results, "Bormuth")[["mean.cloze"]]) * 100
      slot(all.results, "DRP") <- list(DRP=drp.final)
    }
  } else {}

  ## Dickes-Steiwer
  # only "shortcut formula" due to missing information from POS tagging
  # if more elaborate info on pronouns was available,
  # the "computer formula" could easily be added
  if("Dickes.Steiwer" %in% index){
    flavour <- "default"
    valid.params <- c("const", "awl", "asl", "ttr", "case.sens")
    if("Dickes.Steiwer" %in% names(parameters)){
      flavour <- check.flavour(parameters$Dickes.Steiwer, default.parameters$Dickes.Steiwer)
      prms <- parameters$Dickes.Steiwer
    } else{
      prms <- default.parameters$Dickes.Steiwer
    }
    kRp.check.params(names(prms), valid.params, where="Dickes.Steiwer")
    kRp.check.params(valid.params, names(prms), where="Dickes.Steiwer", missing=TRUE)
    Dickes.Steiwer.score <- prms[["const"]] - (log(avg.word.len + 1) * prms[["awl"]]) - (log(avg.sntc.len + 1) * prms[["asl"]]) - (num.TTR * prms[["ttr"]])
    slot(all.results, "Dickes.Steiwer") <- list(flavour=flavour, TTR=num.TTR, Dickes.Steiwer=Dickes.Steiwer.score)
  } else {}

  ## Easy Listening Formula
  if("ELF" %in% index){
    flavour <- "default"
    valid.params <- c("syll")
    if("ELF" %in% names(parameters)){
      flavour <- check.flavour(parameters$ELF, default.parameters$ELF)
      prms <- parameters$ELF
    } else {
      prms <- default.parameters$ELF
    }
    kRp.check.params(names(prms), valid.params, where="ELF")
    kRp.check.params(valid.params, names(prms), where="ELF", missing=TRUE)
    ELF.exsyls <- long.words(prms[["syll"]] - 1)
    ELF.score <- ELF.exsyls / num.sentences
    slot(all.results, "ELF") <- list(flavour=flavour, num.exsyls=ELF.exsyls, ELF=ELF.score)
  } else {}

## "Fasse-Dich-Kurz-Index" (FDK-Index, Rolf W. Schirm)
# 1. Zaehlen Sie die Saetze (ohne Ueberschrift, Gruss oder Anrede)
# 2. Zaehlen Sie die Woerter, die mehr als zwei Silben haben (ohne Namen und Fachwoerter)
# 3. Errechnen Sie den "Fasse-Dich-Kurz-Index (FDK-Index)" nach der Formel:
#
# FDK = Anzahl der Woerter mit mehr als 2 Silben * 10 / Anzah1 der Saetze
#
# FDK bis 10: Sehr knapper Stil (Telegramm, Fernscheiben)
# FDK 11 - 25: Zeitgemaesser Stil (kurz, knapp, praezise)
# FDK 26 - 50: Weitschweifiger Stil (haeufig im geschaeftlichen Schriftverkehr)
# FDK ueber 50: Schwuelstiger, unklarer Stil

  ## Farr-Jenkins-Paterson
  # simplified Flesch RE
  # monsy: percentage of monosyllabic words
  # asl: number of sentenvces
  if("Farr.Jenkins.Paterson" %in% index){
    flavour <- "default"
    valid.params <- c("const", "asl", "monsy")
    if("Farr.Jenkins.Paterson" %in% names(parameters)){
      flavour <- check.flavour(parameters$Farr.Jenkins.Paterson, default.parameters$Farr.Jenkins.Paterson)
      prms <- parameters$Farr.Jenkins.Paterson
      if(identical(prms, "PSK")){
       flavour <- "Powers-Sumner-Kearl"
       prms <- c(const=8.4335, asl=-0.0923, monsy=-0.0648)
      } else {}
    } else {
      prms <- default.parameters$Farr.Jenkins.Paterson
    }
    kRp.check.params(names(prms), valid.params, where="Farr.Jenkins.Paterson")
    kRp.check.params(valid.params, names(prms), where="Farr.Jenkins.Paterson", missing=TRUE)
    fjp.RE <- prms[["const"]] - (prms[["asl"]] * avg.sntc.len) + (prms[["monsy"]] * pct.monosyll)
    fjp.grade <- get.grade.level(fjp.RE, measure="Flesch")
    slot(all.results, "Farr.Jenkins.Paterson") <- list(flavour=flavour, FJP=fjp.RE, grade=fjp.grade[["grade"]], grade.min=fjp.grade[["grade.min"]])
  } else {}
  # recursive calls for alternative shortcuts
  if("Farr.Jenkins.Paterson.PSK" %in% index){
    slot(all.results, "Farr.Jenkins.Paterson.PSK") <- slot(kRp.rdb.formulae(txt.freq, hyphen=hyphen, index=c("Farr.Jenkins.Paterson"), parameters=list(Farr.Jenkins.Paterson="PSK"),
      fileEncoding=fileEncoding, tagger=tagger, force.lang=force.lang, sentc.tag=sentc.tag,
      nonword.class=nonword.class, nonword.tag=nonword.tag, analyze.text=analyze.text, txt.features=txt.features, quiet=TRUE), "Farr.Jenkins.Paterson")
  } else {}

  ## Flesch Reading Ease
  if("Flesch" %in% index){
    flavour <- "en (Flesch)"
    valid.params <- c("const", "asl", "asw")
    if("Flesch" %in% names(parameters)){
      flavour <- check.flavour(parameters$Flesch, default.parameters$Flesch, "en (Flesch)")
      prms <- parameters$Flesch
      if(identical(prms, "en")){
        flavour <- "en (Flesch)"
        prms <- c(const=206.835, asl=1.015, asw=84.6)
      } else {}
      if(identical(prms, "de")){
        flavour <-  "de (Amstad)"
        prms <- c(const=180, asl=1, asw=58.5)
      } else {}
      if(identical(prms, "es")){
        # Fernandez-Huerta
        flavour <- "es (Fernandez-Huerta)"
        prms <- c(const=206.835, asl=1.02, asw=60)
      } else {}
      if(identical(prms, "es-s")){
        # Fernandez-Huerta
        flavour <- "es (Szigriszt)"
        prms <- c(const=206.835, asl=1, asw=62.3)
      } else {}
      if(identical(prms, "nl")){
        # Douma
        flavour <- "nl (Douma)"
        prms <- c(const=206.835, asl=0.33, asw=77)
      } else {}
      if(identical(prms, "fr")){
        # Kandel & Moles
        flavour <- "fr (Kandel-Moles)"
        prms <- c(const=209, asl=1.15, asw=68)
      } else {}
      if(identical(prms, "PSK")){
        flavour <- "Powers-Sumner-Kearl"
        prms <- c(const=-2.2029, asl=-0.0778, asw=-4.55)
      } else {}
    } else {
      prms <- default.parameters$Flesch
    }
      kRp.check.params(names(prms), valid.params, where="Flesch")
      kRp.check.params(valid.params, names(prms), where="Flesch", missing=TRUE)
      Flesch.RE <- prms[["const"]] - (prms[["asl"]] * avg.sntc.len) - (prms[["asw"]] * avg.syll.word)
      if(identical(flavour, "Powers-Sumner-Kearl")){
        slot(all.results, "Flesch") <- list(flavour=flavour, RE=NA, grade=Flesch.RE, grade.min=Flesch.RE, age=(Flesch.RE + 5))
      } else {
        Flesch.grade <- get.grade.level(Flesch.RE, measure="Flesch")
        slot(all.results, "Flesch") <- list(flavour=flavour, RE=Flesch.RE, grade=Flesch.grade[["grade"]], grade.min=Flesch.grade[["grade.min"]], age=NA)
      }
  } else {}
  # recursive calls for alternative shortcuts
  if("Flesch.PSK" %in% index){
    slot(all.results, "Flesch.PSK") <- slot(kRp.rdb.formulae(txt.freq, hyphen=hyphen, index=c("Flesch"), parameters=list(Flesch="PSK"),
      fileEncoding=fileEncoding, tagger=tagger, force.lang=force.lang, sentc.tag=sentc.tag,
      nonword.class=nonword.class, nonword.tag=nonword.tag, analyze.text=analyze.text, txt.features=txt.features, quiet=TRUE), "Flesch")
  } else {}
  if("Flesch.de" %in% index){
    slot(all.results, "Flesch.de") <- slot(kRp.rdb.formulae(txt.freq, hyphen=hyphen, index=c("Flesch"), parameters=list(Flesch="de"),
      fileEncoding=fileEncoding, tagger=tagger, force.lang=force.lang, sentc.tag=sentc.tag,
      nonword.class=nonword.class, nonword.tag=nonword.tag, analyze.text=analyze.text, txt.features=txt.features, quiet=TRUE), "Flesch")
  } else {}
  if("Flesch.es" %in% index){
    slot(all.results, "Flesch.es") <- slot(kRp.rdb.formulae(txt.freq, hyphen=hyphen, index=c("Flesch"), parameters=list(Flesch="es"),
      fileEncoding=fileEncoding, tagger=tagger, force.lang=force.lang, sentc.tag=sentc.tag,
      nonword.class=nonword.class, nonword.tag=nonword.tag, analyze.text=analyze.text, txt.features=txt.features, quiet=TRUE), "Flesch")
  } else {}
  if("Flesch.Szigriszt" %in% index){
    # Flesch-Szigriszt (Indice de Legibilidad de Flesch-Szigriszt (IFSZ))
    # see http://www.legibilidad.com/home/acercade.html
    slot(all.results, "Flesch.Szigriszt") <- slot(kRp.rdb.formulae(txt.freq, hyphen=hyphen, index=c("Flesch"), parameters=list(Flesch="es-s"),
      fileEncoding=fileEncoding, tagger=tagger, force.lang=force.lang, sentc.tag=sentc.tag,
      nonword.class=nonword.class, nonword.tag=nonword.tag, analyze.text=analyze.text, txt.features=txt.features, quiet=TRUE), "Flesch")
  } else {}
  if("Flesch.fr" %in% index){
    slot(all.results, "Flesch.fr") <- slot(kRp.rdb.formulae(txt.freq, hyphen=hyphen, index=c("Flesch"), parameters=list(Flesch="fr"),
      fileEncoding=fileEncoding, tagger=tagger, force.lang=force.lang, sentc.tag=sentc.tag,
      nonword.class=nonword.class, nonword.tag=nonword.tag, analyze.text=analyze.text, txt.features=txt.features, quiet=TRUE), "Flesch")
  } else {}
  if("Flesch.nl" %in% index){
    slot(all.results, "Flesch.nl") <- slot(kRp.rdb.formulae(txt.freq, hyphen=hyphen, index=c("Flesch"), parameters=list(Flesch="nl"),
      fileEncoding=fileEncoding, tagger=tagger, force.lang=force.lang, sentc.tag=sentc.tag,
      nonword.class=nonword.class, nonword.tag=nonword.tag, analyze.text=analyze.text, txt.features=txt.features, quiet=TRUE), "Flesch")
  } else {}

  ## Flesch-Kincaid Grade Level
  if("Flesch.Kincaid" %in% index){
    flavour <- "default"
    valid.params <- c("asl", "asw", "const")
    if("Flesch.Kincaid" %in% names(parameters)){
      flavour <- check.flavour(parameters$Flesch.Kincaid, default.parameters$Flesch.Kincaid)
      prms <- parameters$Flesch.Kincaid
    } else {
      prms <- default.parameters$Flesch.Kincaid
    }
    kRp.check.params(names(prms), valid.params, where="Flesch.Kincaid")
    kRp.check.params(valid.params, names(prms), where="Flesch.Kincaid", missing=TRUE)
    Flesch.Kincaid.GL <- (prms[["asl"]] * avg.sntc.len) + (prms[["asw"]] * avg.syll.word) - prms[["const"]]
    slot(all.results, "Flesch.Kincaid") <- list(flavour=flavour, grade=Flesch.Kincaid.GL, age=Flesch.Kincaid.GL+5)
  } else {}

  ## FORCAST
  if("FORCAST" %in% index){
    flavour <- "default"
    valid.params <- c("syll", "const", "mult")
    if("FORCAST" %in% names(parameters)){
      flavour <- check.flavour(parameters$FORCAST, default.parameters$FORCAST)
      prms <- parameters$FORCAST
      if(identical(prms, "RGL")){
        flavour <- "precise reading grade level"
        prms <- c(syll=1, const=20.43, mult=.11)
      } else {}
    } else {
      prms <- default.parameters$FORCAST
    }
    kRp.check.params(names(prms), valid.params, where="FORCAST")
    kRp.check.params(valid.params, names(prms), where="FORCAST", missing=TRUE)
    FORCAST.monosyll <- slot(hyphen, "desc")[["syll.distrib"]]["cum.sum", prms[["syll"]]] * 150 / num.words
    FORCAST.grade <- prms[["const"]] - (FORCAST.monosyll * prms[["mult"]])
    FORCAST.age <- FORCAST.grade + 5
    slot(all.results, "FORCAST") <- list(flavour=flavour, grade=FORCAST.grade, age=FORCAST.age)
  } else {}
  # recursive calls for alternative shortcuts
  if("FORCAST.RGL" %in% index){
    slot(all.results, "FORCAST.RGL") <- slot(kRp.rdb.formulae(txt.freq, hyphen=hyphen, index=c("FORCAST"), parameters=list(FORCAST="RGL"),
      fileEncoding=fileEncoding, tagger=tagger, force.lang=force.lang, sentc.tag=sentc.tag,
      nonword.class=nonword.class, nonword.tag=nonword.tag, analyze.text=analyze.text, txt.features=txt.features, quiet=TRUE), "FORCAST")
  } else {}

  ## Fucks Stilcharakteristik
  if("Fucks" %in% index){
    Fucks.raw <- avg.word.len * avg.sntc.len
    Fucks.grade <- sqrt(Fucks.raw)
    slot(all.results, "Fucks") <- list(Fucks=Fucks.raw, grade=Fucks.grade)
  } else {}

  ## Gunning FOG Index
  if("FOG" %in% index){
    flavour <- "default"
    valid.params <- c("syll", "const", "suffix")
    if("FOG" %in% names(parameters)){
      flavour <- check.flavour(parameters$FOG, default.parameters$FOG)
      prms <- parameters$FOG
      # we need syllable and suffix parameters for all variants
      if("syll" %in% names(prms)){
        FOG.sylls <- prms[["syll"]]
      } else {
        FOG.sylls <- default.parameters$FOG[["syll"]]
      }
      if("suffix" %in% names(prms)){
        FOG.suffix <- prms[["suffix"]]
      } else {
        FOG.suffix <- default.parameters$FOG[["suffix"]]
      }
    } else {
      prms <- default.parameters$FOG
      FOG.sylls <- prms[["syll"]]
      FOG.suffix <- prms[["suffix"]]
    }
    FOG.hyphen <- hyphen

    # a list for dropped tokens
    FOG.dropped <- list(names=NA, combi=NA, verbs=NA)

    if(isTRUE(analyze.text)){
      # exclude certain words
      # proper nouns/names will completely be omitted
      FOG.names <- which(slot(tagged.words.only, "TT.res")[["wclass"]] == "name")
      # check for verbs ending in -es, -ed, or -ing (or anything else set in prms[["suffix"]])
      # these endings must not be counted as syllables
      FOG.verbs <- which(slot(tagged.words.only, "TT.res")[["wclass"]] == "verb")
      FOG.verb.suffix <- paste0("(", paste(FOG.suffix, collapse="|"), ")$")
      FOG.verbs <- FOG.verbs[grepl(FOG.verb.suffix, slot(tagged.words.only, "TT.res")[FOG.verbs,"token"])]
      # count one syllable less for these
      if(length(FOG.verbs) > 0){
        FOG.dropped[["verbs"]] <- slot(tagged.words.only, "TT.res")[FOG.verbs[slot(FOG.hyphen, "hyphen")[FOG.verbs,"syll"] == FOG.sylls],]
        slot(FOG.hyphen, "hyphen")[FOG.verbs,"syll"] <- slot(FOG.hyphen, "hyphen")[FOG.verbs,"syll"] - 1
      } else {}
      # syllables of combined words must be counted separately
      # \p{Pd} matches dashes, indicating hyphenated/combined words
      FOG.combi <- which(grepl("\\p{Pd}", slot(tagged.words.only, "TT.res")[["token"]], perl=TRUE))
      # next step: split into seperate parts, count syllables for each and
      # drop all if none of the parts is long enough on its own
      FOG.combi.dopped <- c()
      for(combi in FOG.combi){
        ## TODO: should this be cached or not?
        FOG.this.combi <- hyphen(unlist(strsplit(slot(tagged.words.only, "TT.res")[combi, "token"], "\\p{Pd}", perl=TRUE)), hyph.pattern=lang, quiet=TRUE)
        if(all(slot(FOG.this.combi, "hyphen")[["syll"]] < FOG.sylls)){
          # don't count this as a hard word
          FOG.combi.dopped <- combi
        } else {}
      }

      FOG.num.syll <- slot(FOG.hyphen, "hyphen")$syll
      FOG.all.dropped <- c()
      # disarm the found names
      if(length(FOG.names) > 0){
        FOG.dropped[["names"]] <- slot(tagged.words.only, "TT.res")[FOG.names,]
        FOG.all.dropped <- c(FOG.all.dropped, FOG.names)
      } else {}
      if(length(FOG.combi.dopped) > 0){
        FOG.dropped[["combi"]] <- slot(tagged.words.only, "TT.res")[FOG.combi.dopped,]
        FOG.all.dropped <- c(FOG.all.dropped, FOG.combi.dopped)
      } else {}
      if(length(FOG.all.dropped) > 0){
        FOG.num.syll[FOG.all.dropped] <- 0
      } else {}

      FOG.hard.words.absolute <- sum(FOG.num.syll > 2, na.rm=TRUE)
    } else {
      FOG.hard.words.absolute <- txt.features$FOG.hard.words
    }

    FOG.hard.words <- FOG.hard.words.absolute * 100 / num.words

    if(identical(prms, "PSK")){
      flavour <- "Powers-Sumner-Kearl"
      Gunning.FOG <- 3.0680 + (0.0877 * avg.sntc.len) + (0.0984 * FOG.hard.words)
    } else if(identical(prms, "NRI")){
      flavour <- "New FOG (NRI)"
      F0G.easy.words <- (num.words - FOG.hard.words) * 100 / num.words
      Gunning.FOG <- (((F0G.easy.words + (3 * FOG.hard.words)) / sntc.per100) - 3) / 2
    } else {
      kRp.check.params(names(prms), valid.params, where="FOG")
      kRp.check.params(valid.params, names(prms), where="FOG", missing=TRUE)
      if(isTRUE(analyze.text)){
        FOG.hard.words <- sum(FOG.num.syll > (FOG.sylls - 1), na.rm=TRUE) * 100 / num.words
      } else {}
      Gunning.FOG <- (avg.sntc.len + FOG.hard.words) * prms[["const"]]
    }
    # preserve hard words in the desc slot
    slot(all.results, "desc")[["FOG.hard.words"]] <- FOG.hard.words.absolute
    slot(all.results, "FOG") <- list(flavour=flavour, dropped=FOG.dropped, FOG.hard.words=FOG.hard.words.absolute, FOG=Gunning.FOG)
  } else {}
  # recursive calls for alternative shortcuts
  if("FOG.PSK" %in% index){
    slot(all.results, "FOG.PSK") <- slot(kRp.rdb.formulae(txt.freq, hyphen=hyphen, index=c("FOG"), parameters=list(FOG="PSK"),
      fileEncoding=fileEncoding, tagger=tagger, force.lang=force.lang, sentc.tag=sentc.tag,
      nonword.class=nonword.class, nonword.tag=nonword.tag, analyze.text=analyze.text, txt.features=txt.features, quiet=TRUE), "FOG")
  } else {}
  if("FOG.NRI" %in% index){
    slot(all.results, "FOG.NRI") <- slot(kRp.rdb.formulae(txt.freq, hyphen=hyphen, index=c("FOG"), parameters=list(FOG="NRI"),
      fileEncoding=fileEncoding, tagger=tagger, force.lang=force.lang, sentc.tag=sentc.tag,
      nonword.class=nonword.class, nonword.tag=nonword.tag, analyze.text=analyze.text, txt.features=txt.features, quiet=TRUE), "FOG")
  } else {}

  ## Harris-Jacobson
  # V1: percent unfamiliar words ("short list", grades 1 + 2)
  # V2: asl
  # V3: percent of long words (6+ letters)
  # correction grade table see harris & jacobson (1974), p. 9
  if("Harris.Jacobson" %in% index){
    # check for word list and add it to params, otherwise skip four formulae of this index
    if(!"Harris.Jacobson" %in% names(word.lists) | is.null(word.lists[["Harris.Jacobson"]])){
      warning("Harris.Jacobson: Missing word list, hence not calculated.", call.=FALSE)
    } else {
      flavour <- "default"
      valid.params <- c("char", "hj1", "hj2", "hj3", "hj4", "hj5")
      hj.word.list <- read.word.list(word.lists[["Harris.Jacobson"]])
      if("Harris.Jacobson" %in% names(parameters)){
        flavour <- check.flavour(parameters$Harris.Jacobson, default.parameters$Harris.Jacobson)
        prms <- parameters$Harris.Jacobson
      } else {
        prms <- default.parameters$Harris.Jacobson
      }

      kRp.check.params(names(prms), valid.params, where="Harris.Jacobson")
      # we don't necessarily need all params
      kRp.check.params(c("char"), names(prms), where="Harris.Jacobson", missing=TRUE)

      hj.diff.words.all.txt <- difficult.words(txt.words.only, hj.word.list)
      hj.diff.words.txt <- hj.diff.words.all.txt[["pct.not.listed"]]
      hj.diff.words.nol <- hj.diff.words.all.txt[["words.not.listed"]]

      # percentage of long words
      hj.long.words <- (long.words(prms[["char"]], letters=TRUE) * 100) / num.words

      if(!any(c("hj1", "hj2", "hj3", "hj4", "hj5") %in% names(prms))){
      stop(simpleError("Harris.Jacobson: you need to specify parameters for at least *one* of the five formulas!"))
      }

      if("hj1" %in% names(prms)){
        prms.hj1 <- prms$hj1
        valid.prms <- c("dword", "asl", "const")
        kRp.check.params(names(prms.hj1), valid.prms, where="Harris.Jacobson$hj1")
        kRp.check.params(valid.prms, names(prms.hj1), where="Harris.Jacobson$hj1", missing=TRUE)
        # HJ1 <- 0.094 * V1 + 0.168 * V2 + 0.502
        Harris.Jacobson1 <- (prms.hj1[["dword"]] * hj.diff.words.txt) + (prms.hj1[["asl"]] * avg.sntc.len) + prms.hj1[["const"]]
      } else {
        Harris.Jacobson1 <- NA
      }

      if("hj2" %in% names(prms)){
        prms.hj2 <- prms$hj2
        valid.prms <- c("dword", "asl", "const")
        kRp.check.params(names(prms.hj2), valid.prms, where="Harris.Jacobson$hj2")
        kRp.check.params(valid.prms, names(prms.hj2), where="Harris.Jacobson$hj2", missing=TRUE)
        # HJ2 <- 0.140 * V1 + 0.153 * V2 + 0.560
        Harris.Jacobson2 <- (prms.hj2[["dword"]] * hj.diff.words.txt) + (prms.hj2[["asl"]] * avg.sntc.len) + prms.hj2[["const"]]
      } else {
        Harris.Jacobson2 <- NA
      }

      if("hj3" %in% names(prms)){
        prms.hj3 <- prms$hj3
        valid.prms <- c("asl", "lword", "const")
        kRp.check.params(names(prms.hj3), valid.prms, where="Harris.Jacobson$hj3")
        kRp.check.params(valid.prms, names(prms.hj3), where="Harris.Jacobson$hj3", missing=TRUE)
        # HJ3 <- 0.158 * V2 + 0.055 * V3 + 0.355
        Harris.Jacobson3 <- (prms.hj3[["asl"]] * avg.sntc.len) + (prms.hj3[["lword"]] * hj.long.words) + prms.hj3[["const"]]
      } else {
        Harris.Jacobson3 <- NA
      }

      if("hj4" %in% names(prms)){
        prms.hj4 <- prms$hj4
        valid.prms <- c("dword", "asl", "lword", "const")
        kRp.check.params(names(prms.hj4), valid.prms, where="Harris.Jacobson$hj4")
        kRp.check.params(valid.prms, names(prms.hj4), where="Harris.Jacobson$hj4", missing=TRUE)
        # HJ4 <- 0.070 * V1 + 0.125 * V2 + 0.037 * V3 + 0.497 ## best validity, formula of choice
        Harris.Jacobson4 <- (prms.hj4[["dword"]] * hj.diff.words.txt) + (prms.hj4[["asl"]] * avg.sntc.len) + (prms.hj4[["lword"]] * hj.long.words) + prms.hj4[["const"]]
      } else {
        Harris.Jacobson4 <- NA
      }

      if("hj5" %in% names(prms)){
        prms.hj5 <- prms$hj5
        valid.prms <- c("dword", "asl", "lword", "const")
        kRp.check.params(names(prms.hj5), valid.prms, where="Harris.Jacobson$hj5")
        kRp.check.params(valid.prms, names(prms.hj5), where="Harris.Jacobson$hj5", missing=TRUE)
        # HJ5 <- 0.118 * V1 + 0.134 * V2 + 0.032 * V3 + 0.424 ## > 3. grade
        Harris.Jacobson5 <- (prms.hj5[["dword"]] * hj.diff.words.txt) + (prms.hj5[["asl"]] * avg.sntc.len) + (prms.hj5[["lword"]] * hj.long.words) + prms.hj5[["const"]]
      } else {
        Harris.Jacobson5 <- NA
      }

      # preserve hard words in the desc slot
      if(isTRUE(analyze.text)){
        slot(all.results, "desc")[["Harris.Jacobson.NOL"]] <- length(hj.diff.words.nol)
      } else {
        slot(all.results, "desc")[["Harris.Jacobson.NOL"]] <- hj.word.list
      }
      slot(all.results, "Harris.Jacobson") <- list(flavour=flavour,
        word.list=hj.word.list, not.on.list=hj.diff.words.nol, pct=hj.diff.words.txt,
        pct.long=hj.long.words,
        HJ1=Harris.Jacobson1, HJ2=Harris.Jacobson2, HJ3=Harris.Jacobson3, HJ4=Harris.Jacobson4, HJ5=Harris.Jacobson5)
    }
  } else {}

  ## Laesbarhetsindex (LIX)
  if("LIX" %in% index){
    flavour <- "default"
    valid.params <- c("char", "const")
    if("LIX" %in% names(parameters)){
      flavour <- check.flavour(parameters$LIX, default.parameters$LIX)
      prms <- parameters$LIX
    } else {
      prms <- default.parameters$LIX
    }
    kRp.check.params(names(prms), valid.params, where="LIX")
    kRp.check.params(valid.params, names(prms), where="LIX", missing=TRUE)
    LIX.long.words <- long.words(prms[["char"]], letters=TRUE)
    LIX.idx <- (num.words / num.sentences) + ((LIX.long.words * prms[["const"]]) / num.words)
    LIX.rating <- get.grade.level(LIX.idx, measure="LIX")
    LIX.grade <- get.grade.level(LIX.idx, measure="LIX.grade")
    slot(all.results, "LIX") <- list(flavour=flavour, index=LIX.idx, rating=LIX.rating[["grade"]], grade=LIX.grade[["grade"]], grade.min=LIX.grade[["grade.min"]])
  } else {}

  ## Linsear Write
  if("Linsear.Write" %in% index){
    flavour <- "default"
    valid.params <- c("short.syll", "long.syll", "thrs")
    if("Linsear.Write" %in% names(parameters)){
      flavour <- check.flavour(parameters$Linsear.Write, default.parameters$Linsear.Write)
      prms <- parameters$Linsear.Write
    } else {
      prms <- default.parameters$Linsear.Write
    }
    kRp.check.params(names(prms), valid.params, where="Linsear.Write")
    kRp.check.params(valid.params, names(prms), where="Linsear.Write", missing=TRUE)
    hard.words <- (long.words(prms[["long.syll"]] - 1) * 100) / num.words
    easy.words <- 100 - ((long.words(prms[["short.syll"]]) * 100) / num.words)
    Linsear.Write.raw <- (easy.words + (hard.words * 3)) / sntc.per100
    if(Linsear.Write.raw > prms[["thrs"]]){
      Linsear.Write.RM <- Linsear.Write.raw / 2
    } else {
      Linsear.Write.RM <- (Linsear.Write.raw - 2) / 2
    }
    slot(all.results, "Linsear.Write") <- list(flavour=flavour, easy.words=easy.words, hard.words=hard.words, raw=Linsear.Write.raw, grade=Linsear.Write.RM)
  } else {}

  ## Neue Wiener Sachtextformeln
  if("nWS" %in% index){
    flavour <- "default"
    valid.params <- c("ms.syll", "iw.char", "es.syll", "nws1", "nws2", "nws3", "nws4")
    if("nWS" %in% names(parameters)){
      flavour <- check.flavour(parameters$nWS, default.parameters$nWS)
      prms <- parameters$nWS
    } else {
      prms <- default.parameters$nWS
    }
    kRp.check.params(names(prms), valid.params, where="nWS")
    # we don't necessarily need all params
    kRp.check.params(c("ms.syll", "iw.char", "es.syll"), names(prms), where="nWS", missing=TRUE)

    wien.MS <- long.words(prms[["ms.syll"]] - 1) / num.words
    wien.SL <- avg.sntc.len
    wien.IW <- long.words(prms[["iw.char"]], letters=TRUE) / num.words
    wien.ES <- slot(hyphen, "desc")[["syll.distrib"]]["cum.sum", prms[["es.syll"]]] / num.words

    if(!any(c("nws1", "nws2", "nws3", "nws4") %in% names(prms))){
      stop(simpleError("nWS: You need to specify parameters for at least *one* of the four formulas!"))
    }

    if("nws1" %in% names(prms)){
      prms.w1 <- prms$nws1
      valid.prms <- c("ms", "sl", "iw", "es", "const")
      kRp.check.params(names(prms.w1), valid.prms, where="nWS$nws1")
      kRp.check.params(valid.prms, names(prms.w1), where="nWS$nws1", missing=TRUE)
      nWS1 <- (prms.w1[["ms"]] * wien.MS) + (prms.w1[["sl"]] * wien.SL) + (prms.w1[["iw"]] * wien.IW) - (prms.w1[["es"]] * wien.ES) - prms.w1[["const"]]
    } else {
      nWS1 <- NA
    }

    if("nws2" %in% names(prms)){
      prms.w2 <- prms$nws2
      valid.prms <- c("ms", "sl", "iw", "const")
      kRp.check.params(names(prms.w2), valid.prms, where="nWS$nws2")
      kRp.check.params(valid.prms, names(prms.w2), where="nWS$nws2", missing=TRUE)
      nWS2 <- (prms.w2[["ms"]] * wien.MS) + (prms.w2[["sl"]] * wien.SL) + (prms.w2[["iw"]] * wien.IW) - prms.w2[["const"]]
    } else {
      nWS2 <- NA
    }

    if("nws3" %in% names(prms)){
      prms.w3 <- prms$nws3
      valid.prms <- c("ms", "sl", "const")
      kRp.check.params(names(prms.w3), valid.prms, where="nWS$nws3")
      kRp.check.params(valid.prms, names(prms.w3), where="nWS$nws3", missing=TRUE)
      nWS3 <- (prms.w3[["ms"]] * wien.MS) + (prms.w3[["sl"]] * wien.SL) - prms.w3[["const"]]
    } else {
      nWS3 <- NA
    }

    if("nws4" %in% names(prms)){
      prms.w4 <- prms$nws4
      valid.prms <- c("ms", "sl", "const")
      kRp.check.params(names(prms.w4), valid.prms, where="nWS$nws4")
      kRp.check.params(valid.prms, names(prms.w4), where="nWS$nws4", missing=TRUE)
      nWS4 <- (prms.w4[["sl"]] * wien.SL) + (prms.w4[["ms"]] * wien.MS) - prms.w4[["const"]]
    } else {
      nWS4 <- NA
    }

    slot(all.results, "Wiener.STF") <- list(flavour=flavour, nWS1=nWS1, nWS2=nWS2, nWS3=nWS3, nWS4=nWS4)
  } else {}

  ## RIX
  if("RIX" %in% index){
    flavour <- "default"
    valid.params <- c("char")
    if("RIX" %in% names(parameters)){
      flavour <- check.flavour(parameters$RIX, default.parameters$RIX)
      prms <- parameters$RIX
    } else {
      prms <- default.parameters$RIX
    }
    kRp.check.params(names(prms), valid.params, where="RIX")
    kRp.check.params(valid.params, names(prms), where="RIX", missing=TRUE)
    RIX.long.words <- long.words(prms[["char"]], letters=TRUE)
    RIX.idx <- RIX.long.words / num.sentences
    RIX.grade <- get.grade.level(RIX.idx, measure="RIX")
    slot(all.results, "RIX") <- list(flavour=flavour, index=RIX.idx, grade=RIX.grade[["grade"]], grade.min=RIX.grade[["grade.min"]])
  } else {}

  ## Simple Measure of Gobbledygook (SMOG)
  if("SMOG" %in% index){
    flavour <- "default"
    valid.params <- c("sqrt", "fact", "syll", "const", "sqrt.const")
    if("SMOG" %in% names(parameters)){
      flavour <- check.flavour(parameters$SMOG, default.parameters$SMOG)
      prms <- parameters$SMOG
      if(identical(prms, "de")){
        flavour <- "de (\"Qu\", Bamberger-Vanecek)"
        prms <- c(sqrt=1, fact=30, syll=3, const=-2, sqrt.const=0)
      } else {}
      if(identical(prms, "C")){
        flavour <- "Fomula C"
        prms <- c(sqrt=0.9986, fact=30, syll=3, const=2.8795, sqrt.const=5)
      } else {}
      if(identical(prms, "simple")){
        flavour <- "simplified"
        prms <- c(sqrt=1, fact=30, syll=3, const=3, sqrt.const=0)
      } else {}
    } else {
      prms <- default.parameters$SMOG
    }
    kRp.check.params(names(prms), valid.params, where="SMOG")
    kRp.check.params(valid.params, names(prms), where="SMOG", missing=TRUE)
    SMOG.grade <- prms[["sqrt"]] * sqrt((prms[["fact"]] * (long.words(prms[["syll"]] - 1) / num.sentences)) + prms[["sqrt.const"]]) + prms[["const"]]
    slot(all.results, "SMOG") <- list(flavour=flavour, grade=SMOG.grade, age=SMOG.grade + 5)
  } else {}
  # recursive calls for alternative shortcuts
  if("SMOG.de" %in% index){
    slot(all.results, "SMOG.de") <- slot(kRp.rdb.formulae(txt.freq, hyphen=hyphen, index=c("SMOG"), parameters=list(SMOG="de"),
      fileEncoding=fileEncoding, tagger=tagger, force.lang=force.lang, sentc.tag=sentc.tag,
      nonword.class=nonword.class, nonword.tag=nonword.tag, analyze.text=analyze.text, txt.features=txt.features, quiet=TRUE), "SMOG")
  } else {}
  if("SMOG.C" %in% index){
    slot(all.results, "SMOG.C") <- slot(kRp.rdb.formulae(txt.freq, hyphen=hyphen, index=c("SMOG"), parameters=list(SMOG="C"),
      fileEncoding=fileEncoding, tagger=tagger, force.lang=force.lang, sentc.tag=sentc.tag,
      nonword.class=nonword.class, nonword.tag=nonword.tag, analyze.text=analyze.text, txt.features=txt.features, quiet=TRUE), "SMOG")
  } else {}
  if("SMOG.simple" %in% index){
    slot(all.results, "SMOG.simple") <- slot(kRp.rdb.formulae(txt.freq, hyphen=hyphen, index=c("SMOG"), parameters=list(SMOG="simple"),
      fileEncoding=fileEncoding, tagger=tagger, force.lang=force.lang, sentc.tag=sentc.tag,
      nonword.class=nonword.class, nonword.tag=nonword.tag, analyze.text=analyze.text, txt.features=txt.features, quiet=TRUE), "SMOG")
  } else {}

  ## Spache
  if("Spache" %in% index){
    # check for word list and add it to params, otherwise skip this index
    if(!"Spache" %in% names(word.lists) | is.null(word.lists[["Spache"]])){
      warning("Spache: Missing word list, hence not calculated.", call.=FALSE)
    } else {
      flavour <- "default"
      valid.params <- c("asl", "dword", "const")
      spache.word.list <- read.word.list(word.lists[["Spache"]])
      if("Spache" %in% names(parameters)){
        flavour <- check.flavour(parameters$Spache, default.parameters$Spache, default.name="Revised formula (1978)")
        prms <- parameters$Spache
        if(identical(prms, "old")){
          flavour <- "First formula (1953)"
          prms <- list(asl=0.141, dword=0.086, const=0.839)
        } else {}
      } else {
        prms <- default.parameters$Spache
      }
      kRp.check.params(names(prms), valid.params, where="Spache")
      kRp.check.params(valid.params, names(prms), where="Spache", missing=TRUE)
      diff.words.all.txt <- difficult.words(txt.words.only, spache.word.list, only.once=TRUE)
      diff.words.txt <- diff.words.all.txt[["pct.not.listed"]]
      diff.words.nol <- diff.words.all.txt[["words.not.listed"]]
      Spache.grade <- prms[["asl"]] * avg.sntc.len + prms[["dword"]] * diff.words.txt + prms[["const"]]
      # preserve hard words in the desc slot
      if(isTRUE(analyze.text)){
        slot(all.results, "desc")[["Spache.NOL"]] <- length(diff.words.nol)
      } else {
        slot(all.results, "desc")[["Spache.NOL"]] <- spache.word.list
      }
      slot(all.results, "Spache") <- list(flavour=flavour, word.list=spache.word.list, not.on.list=diff.words.nol, pct=diff.words.txt, grade=Spache.grade)
    }
  } else {}
  # recursive calls for alternative shortcuts
  if("Spache.old" %in% index){
    slot(all.results, "Spache.old") <- slot(kRp.rdb.formulae(txt.freq, hyphen=hyphen, index=c("Spache"), parameters=list(Spache="old"),
      word.lists=list(Spache=word.lists[["Spache"]]),
      fileEncoding=fileEncoding, tagger=tagger, force.lang=force.lang, sentc.tag=sentc.tag,
      nonword.class=nonword.class, nonword.tag=nonword.tag, analyze.text=analyze.text, txt.features=txt.features, quiet=TRUE), "Spache")
  } else {}

  ## Strain Index
  # http://strainindex.wordpress.com/2007/09/25/hello-world/
  if("Strain" %in% index){
    flavour <- "default"
    valid.params <- c("sent", "const")
    if("Strain" %in% names(parameters)){
      flavour <- check.flavour(parameters$Strain, default.parameters$Strain)
      prms <- parameters$Strain
    } else {
      prms <- default.parameters$Strain
    }
    kRp.check.params(names(prms), valid.params, where="Strain")
    kRp.check.params(valid.params, names(prms), where="Strain", missing=TRUE)
    # syllables per 3 sentences divided by 10
    Strain.id <- (num.syll / (num.sentences / prms[["sent"]])) / prms[["const"]]
    slot(all.results, "Strain") <- list(flavour=flavour, index=Strain.id)
  } else {}

  ## Traenkle-Bailer
  # new Dickes-Steiwer for german texts
  if("Traenkle.Bailer" %in% index){
    # this formula needs proper POS tags; skip if missing
    if(all(slot(tagged.text, "TT.res")[["tag"]] %in% kRp.POS.tags("kRp", list.tags=TRUE))){
      # this text was just tagged with tokenize() and misses important tags
      warning("Traenkle.Bailer: POS tags are not elaborate enough, can't count prepositions and conjuctions. Formulae skipped.", call.=FALSE)
    } else {
      flavour <- "default"
      valid.params <- c("TB1", "TB2")
      if("Traenkle.Bailer" %in% names(parameters)){
        flavour <- check.flavour(parameters$Traenkle.Bailer, default.parameters$Traenkle.Bailer)
        prms <- parameters$Traenkle.Bailer
      } else{
        prms <- default.parameters$Traenkle.Bailer
      }
      kRp.check.params(names(prms), valid.params, where="Traenkle.Bailer")

      if(num.prepositions == 0 && num.conjunctions == 0){
        warning("Traenkle.Bailer: No tokens tagged as preposition or conjunction found. Was the text properly tagged?", call.=FALSE)
      } else {}
      TrBa.pct.prep <- num.prepositions * 100 / num.words
      TrBa.pct.conj <- num.conjunctions * 100 / num.words

      if(!any(c("TB1", "TB2") %in% names(prms))){
        stop(simpleError("Traenkle.Bailer: You need to specify parameters for at least *one* of the two formulas!"))
      }

      if("TB1" %in% names(prms)){
        prms.TB1 <- prms$TB1
        valid.prms <- c("const", "awl", "asl", "prep")
        kRp.check.params(names(prms.TB1), valid.prms, where="Traenkle.Bailer$TB1")
        kRp.check.params(valid.prms, names(prms.TB1), where="Traenkle.Bailer$TB1", missing=TRUE)
        # TB1 = 224.6814 - (79.8304 * ln(awl + 1)) - (12.24032 * ln(asl + 1)) - (1.292857 * %prepos)
        Traenkle.Bailer1 <- prms.TB1[["const"]] - (log(avg.word.len + 1) * prms.TB1[["awl"]]) - (log(avg.sntc.len + 1) * prms.TB1[["asl"]]) - (TrBa.pct.prep * prms.TB1[["prep"]])
      } else {
        Traenkle.Bailer1 <- NA
      }

      if("TB2" %in% names(prms)){
        prms.TB2 <- prms$TB2
        valid.prms <- c("const", "awl", "prep", "conj")
        kRp.check.params(names(prms.TB2), valid.prms, where="Traenkle.Bailer$TB2")
        kRp.check.params(valid.prms, names(prms.TB2), where="Traenkle.Bailer$TB2", missing=TRUE)
        # TB2 = 234.1063 - (96.11069 * ln(awl + 1)) - (2.05444 * %prepos) - (1.02805 * %conjct)
        Traenkle.Bailer2 <- prms.TB2[["const"]] - (log(avg.word.len + 1) * prms.TB2[["awl"]]) - (TrBa.pct.prep * prms.TB2[["prep"]]) - (TrBa.pct.conj * prms.TB2[["conj"]])
      } else {
        Traenkle.Bailer2 <- NA
      }

      slot(all.results, "Traenkle.Bailer") <- list(flavour=flavour, pct.prep=TrBa.pct.prep, pct.conj=TrBa.pct.conj, TB1=Traenkle.Bailer1, TB2=Traenkle.Bailer2)
    }
  } else {}

  ## TRI -- Kuntzsch's Text-Redundanz-Index
# zit. nach Klein, H. (2002). Lesbarkeit und Verstaendlichkeit von Texten (Teil 2). Technische Dokumentation, 2002/07.
#    http://www.doku.net/ausgabe/200207.htm (2011-05-09)
  if("TRI" %in% index){
    flavour <- "default"
    valid.params <- c("syll", "word", "pnct", "frgn", "const")
    if("TRI" %in% names(parameters)){
      flavour <- check.flavour(parameters$TRI, default.parameters$TRI)
      prms <- parameters$TRI
    } else{
      prms <- default.parameters$TRI
    }
    kRp.check.params(names(prms), valid.params, where="TRI")
    kRp.check.params(valid.params, names(prms), where="TRI", missing=TRUE)
    num.one.syll <- num.words - long.words(prms[["syll"]])
    TRI <- (num.one.syll * prms[["word"]]) - (num.punct * prms[["pnct"]]) - (num.foreign * prms[["frgn"]]) - prms[["const"]]
    slot(all.results, "TRI") <- list(flavour=flavour, short=num.one.syll, punct=num.punct, foreign=num.foreign, TRI=TRI)
  } else {}

  ## Tuldava
  if("Tuldava" %in% index){
    flavour <- "default"
    valid.params <- c("syll", "word1", "word2", "sent")
    if("Tuldava" %in% names(parameters)){
      flavour <- check.flavour(parameters$Tuldava, default.parameters$Tuldava)
      prms <- parameters$Tuldava
    } else{
      prms <- default.parameters$Tuldava
    }
    kRp.check.params(names(prms), valid.params, where="Tuldava")
    kRp.check.params(valid.params, names(prms), where="Tuldava", missing=TRUE)
    Tuldava.score <- ((prms[["syll"]] * num.syll) / (prms[["word1"]] * num.words)) * log(((prms[["word2"]] * num.words) / (prms[["sent"]] * num.sentences)))
#     Tuldava.score <- avg.syll.word * log(avg.sntc.len)
    slot(all.results, "Tuldava") <- list(flavour=flavour, Tuldava=Tuldava.score)
  } else {}

  ## Wheeler-Smith
  if("Wheeler.Smith" %in% index){
    flavour <- "default"
    valid.params <- c("syll")
    grade.measure <- "Wheeler.Smith"
    if("Wheeler.Smith" %in% names(parameters)){
      flavour <- check.flavour(parameters$Wheeler.Smith, default.parameters$Wheeler.Smith)
      prms <- parameters$Wheeler.Smith
      grade.measure <- "Wheeler.Smith"
      if(identical(prms, "de")){
        flavour <- "de (Bamberger & Vanecek)"
        prms <- default.parameters$Wheeler.Smith
        grade.measure <- "Wheeler.Smith.de"
      } else {}
    } else {
      prms <- default.parameters$Wheeler.Smith
    }
    kRp.check.params(names(prms), valid.params, where="Wheeler.Smith")
    kRp.check.params(valid.params, names(prms), where="Wheeler.Smith", missing=TRUE)
    Wheeler.Smith.long.words <- 10 * long.words(prms[["syll"]] - 1) / num.words
    Wheeler.Smith.score <- avg.sntc.len * Wheeler.Smith.long.words
    Wheeler.Smith.grade <- get.grade.level(Wheeler.Smith.score, measure=grade.measure)
    slot(all.results, "Wheeler.Smith") <- list(flavour=flavour, score=Wheeler.Smith.score,
      grade=Wheeler.Smith.grade[["grade"]], grade.min=Wheeler.Smith.grade[["grade.min"]])
  } else {}
  # recursive calls for alternative shortcuts
  if("Wheeler.Smith.de" %in% index){
    slot(all.results, "Wheeler.Smith.de") <- slot(kRp.rdb.formulae(txt.freq, hyphen=hyphen, index=c("Wheeler.Smith"), parameters=list(Wheeler.Smith="de"),
      fileEncoding=fileEncoding, tagger=tagger, force.lang=force.lang, sentc.tag=sentc.tag,
      nonword.class=nonword.class, nonword.tag=nonword.tag, analyze.text=analyze.text, txt.features=txt.features, quiet=TRUE), "Wheeler.Smith")
  } else {}


#   if("" %in% names(parameters)){
#     prms <- parameters$
#     valid.params <- c()
#     kRp.check.params(names(prms), valid.params)
#     kRp.check.params(valid.params, names(prms), missing=TRUE)
#     slot(all.results, "<nm>") <- <frml>
#   } else {}

  ## for the time being, give a warning until all implementations have been validated
  needs.warning <- index %in% c("ARI.simple", "Coleman", "Danielson.Bryan",
      "Dickes.Steiwer", "ELF", "Flesch.de", "Flesch.fr",
      "Flesch.nl", "Fucks", "Harris.Jacobson", "nWS",
      "SMOG.C", "SMOG.de", "Strain", "Traenkle.Bailer", "TRI")
  if(!isTRUE(quiet) && any(needs.warning)){
    warning(paste0("Note: The implementations of these formulas are still subject to validation:\n  ",
    paste(index[needs.warning], collapse=", "),
    "\n  Use the results with caution, even if they seem plausible!"), call.=FALSE)
  } else {}
  return(all.results)
}
