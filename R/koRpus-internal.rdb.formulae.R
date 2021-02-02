# Copyright 2010-2021 Meik Michalke <meik.michalke@hhu.de>
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
# Fry Graph
# Raygor Estimate Graph
  # http://en.wikipedia.org/wiki/Raygor_Readability_Estimate
#  - harris-jacobson wide range (noch keine formel)
#  - dolch sight words suite (noch keine formel)
#  - mcalpine EFLAW
# devereaux (smith, s. kane)

#  - grade eqivalent zum DRP

rdb_indices <- matrix(
  c(
  # fast    sylls   params  validated
    TRUE,   FALSE,  TRUE,   TRUE,     # ARI
    FALSE,  FALSE,  FALSE,  TRUE,     # ARI.NRI
    FALSE,  FALSE,  FALSE,  FALSE,    # ARI.simple
    TRUE,   FALSE,  TRUE,   TRUE,     # Bormuth
    TRUE,   TRUE,   TRUE,   FALSE,    # Coleman
    TRUE,   FALSE,  TRUE,   TRUE,     # Coleman.Liau
    TRUE,   FALSE,  TRUE,   TRUE,     # Dale.Chall
    FALSE,  FALSE,  FALSE,  TRUE,     # Dale.Chall.old
    FALSE,  FALSE,  FALSE,  TRUE,     # Dale.Chall.PSK
    TRUE,   FALSE,  TRUE,   FALSE,    # Danielson.Bryan
    TRUE,   FALSE,  TRUE,   FALSE,    # Dickes.Steiwer
    TRUE,   FALSE,  FALSE,  TRUE,     # DRP
    TRUE,   TRUE,   TRUE,   FALSE,    # ELF
    TRUE,   TRUE,   TRUE,   TRUE,     # Farr.Jenkins.Paterson
    FALSE,  TRUE,   FALSE,  TRUE,     # Farr.Jenkins.Paterson.PSK
    TRUE,   TRUE,   TRUE,   TRUE,     # Flesch
    FALSE,  TRUE,   FALSE,  FALSE,    # Flesch.Brouwer
    FALSE,  TRUE,   FALSE,  FALSE,    # Flesch.de
    FALSE,  TRUE,   FALSE,  TRUE,     # Flesch.es
    FALSE,  TRUE,   FALSE,  FALSE,    # Flesch.fr
    TRUE,   TRUE,   TRUE,   TRUE,     # Flesch.Kincaid
    FALSE,  TRUE,   FALSE,  FALSE,    # Flesch.nl
    FALSE,  TRUE,   FALSE,  TRUE,     # Flesch.nl-b
    FALSE,  TRUE,   FALSE,  TRUE,     # Flesch.PSK
    FALSE,  TRUE,   FALSE,  TRUE,     # Flesch.Szigriszt
    FALSE,  TRUE,   TRUE,   TRUE,     # FOG
    FALSE,  TRUE,   FALSE,  TRUE,     # FOG.NRI
    FALSE,  TRUE,   FALSE,  TRUE,     # FOG.PSK
    TRUE,   TRUE,   TRUE,   TRUE,     # FORCAST
    FALSE,  TRUE,   FALSE,  TRUE,     # FORCAST.RGL
    TRUE,   TRUE,   FALSE,  FALSE,    # Fucks
    TRUE,   FALSE,  TRUE,   FALSE,    # Harris.Jacobson
    TRUE,   TRUE,   TRUE,   TRUE,     # Linsear.Write
    TRUE,   FALSE,  TRUE,   TRUE,     # LIX
    TRUE,   TRUE,   TRUE,   FALSE,    # nWS
    TRUE,   FALSE,  TRUE,   TRUE,     # RIX
    TRUE,   TRUE,   TRUE,   TRUE,     # SMOG
    FALSE,  TRUE,   FALSE,  FALSE,    # SMOG.C
    FALSE,  TRUE,   FALSE,  FALSE,    # SMOG.de
    FALSE,  TRUE,   FALSE,  TRUE,     # SMOG.simple
    TRUE,   FALSE,  TRUE,   TRUE,     # Spache
    FALSE,  FALSE,  FALSE,  TRUE,     # Spache.de
    FALSE,  FALSE,  FALSE,  TRUE,     # Spache.old
    TRUE,   TRUE,   TRUE,   FALSE,    # Strain
    TRUE,   FALSE,  TRUE,   FALSE,    # Traenkle.Bailer
    TRUE,   TRUE,   TRUE,   FALSE,    # TRI
    TRUE,   TRUE,   TRUE,   TRUE,     # Tuldava
    TRUE,   TRUE,   TRUE,   TRUE,     # Wheeler.Smith
    FALSE,  TRUE,   FALSE,  TRUE      # Wheeler.Smith.de
  ),
  ncol=4,
  byrow=TRUE,
  dimnames=list(
    c(
      "ARI", 
      "ARI.NRI", 
      "ARI.simple", 
      "Bormuth", 
      "Coleman", 
      "Coleman.Liau", 
      "Dale.Chall", 
      "Dale.Chall.old", 
      "Dale.Chall.PSK", 
      "Danielson.Bryan", 
      "Dickes.Steiwer", 
      "DRP", 
      "ELF", 
      "Farr.Jenkins.Paterson", 
      "Farr.Jenkins.Paterson.PSK", 
      "Flesch", 
      "Flesch.Brouwer", 
      "Flesch.de", 
      "Flesch.es", 
      "Flesch.fr", 
      "Flesch.Kincaid", 
      "Flesch.nl", 
      "Flesch.nl-b", 
      "Flesch.PSK", 
      "Flesch.Szigriszt", 
      "FOG", 
      "FOG.NRI", 
      "FOG.PSK", 
      "FORCAST", 
      "FORCAST.RGL", 
      "Fucks", 
      "Harris.Jacobson", 
      "Linsear.Write", 
      "LIX", 
      "nWS", 
      "RIX", 
      "SMOG", 
      "SMOG.C", 
      "SMOG.de", 
      "SMOG.simple", 
      "Spache", 
      "Spache.de", 
      "Spache.old", 
      "Strain", 
      "Traenkle.Bailer", 
      "TRI", 
      "Tuldava", 
      "Wheeler.Smith", 
      "Wheeler.Smith.de"
    ),
    c(
      "fast",
      "sylls",
      "params",
      "validated"
    )
  )
)


## function validate_parameters()
# - given: character string or list of parameter names
# - index: name of the index that all values of "given" qill be checked against
#     if "all" a basic check is done whether params for unknown indizes are given
validate_parameters <- function(given, index){
  if(identical(index, "all")){
    valid <- rdb_indices[,"params"]
    given <- names(given)
    index <- "parameters"
  } else {
    valid <- sapply(unlist(rdb_parameters(index=index)), function(x) TRUE)
    given <- names(unlist(given))
  }
  if(
    any(
      all(identical(index, "parameters"), isTRUE(all(valid[given]))),
      all(sum(valid[given], na.rm=TRUE) == length(given), length(valid) == length(given))
    )
  ){
    return(TRUE)
  } else {
    invalid_params <- given[!given %in% names(valid)]
    missing_params <- names(valid)[!names(valid) %in% given]
    check_location <- paste0(" in \"", index, "\"")
    if(length(missing_params)){
      stop(simpleError(paste0("Missing parameters", check_location, ": \"", paste(missing_params, collapse="\", \""), "\"")))
    } else {
      stop(simpleError(paste0("Invalid parameters", check_location, ": \"", paste(invalid_params, collapse="\", \""), "\"")))
    }
  }
} ## end function validate_parameters()


## function check_parameters()
# takes provided parameters (if any) and tries to figure out if they should be
# used as-is or need to be replaced because they are a shortcut string
# the function calls validate_parameters() and returns a list of two
# elements, "p" with the parameters to use and "f" with the name of the
# index flavour
#  - index: character string, name of the readability index
#  - given: a list of parameters to check
#  - short: a named list of character strings, where element names are shortcuts used
#      as parameters and the value is to be used as its flavour; if missing
#      the flavour will be "default", "custom", or identical to the shortcut
check_parameters <- function(
  index,
  given,
  flav_names=NULL
){
  if(length(given)){
    if(all(length(given) == 1, is.null(names(given)))){
      # should be a shortcut
      flavour <- given
      params <- rdb_parameters(index=index, flavour=given)
    } else {
      # should be custom parameters
      validate_parameters(given=given, index=index)
      flavour <- ifelse(
        identical(given, rdb_parameters(index=index, flavour="default")),
        "default",
        "custom"
      )
      params <- given
    }
  } else {
    # no parameters given, fall back to defaults
    flavour <- "default"
    params <- rdb_parameters(index=index, flavour=flavour)
  }
  # finally check for flavour name replacement
  if(length(flav_names[[flavour]])){
    flavour <- flav_names[[flavour]]
  }
  return(list(p=params, f=flavour))
} ## end function check_parameters()


## additional options:
# - analyze.text:
#     TRUE for kRp.rdb.formulae(), i.e. get values from text object examination,
#     FALSE to calculate with the values in txt.features
# - txt.features: a named list with key values needed to calculate
#' @importFrom sylly hyphenText hyphenText<-
kRp.rdb.formulae <- function(
  txt.file=NULL,
  hyphen=NULL,
  index=c(),
  parameters=list(),
  word.lists=list(),
  fileEncoding="UTF-8",
  sentc.tag="sentc",
  nonword.class="nonpunct",
  nonword.tag=c(),
  quiet=FALSE,
  keep.input=NULL,
  analyze.text=TRUE,
  txt.features=list(),
  as.feature=FALSE
){

  ## TODO: validation
  if(identical(index, "validation")){
    message(paste0("
  The following implementations have already been checked against various tools
  to validate the correctness of calculation. This doesn't mean they always came to identical
  results at once, since the accuracy of input data (like number of syllables or sentences)
  varies considerably. But if these differences were manually corrected, the results were similar/identical:
  - ARI                   [OUT, RDS, FRT, WDC]
    - NRI                 [RDS (labeled as \"simplified\")]
  - Bormuth Mean Cloze    [RDS]
  - Coleman-Liau          [OUT, RDS]
  - Dale-Chall            [RDS]
    - PSK                 [RDS]
    - Dale-Chall (1948)   [OKP]
  - DRP                   [RDS]
  - Farr-Jenkins-Paterson [RDS]
    - PSK                 [RDS]
  - Flesch                [OUT, RDS, TAL, LLB, JRT, WDC]
    - PSK                 [RDS]
    - Szigriszt (es)      [INF]
    - Flesch.es           [INF]
  - Flesch-Kincaid        [OUT, RDS, JRT, WDC]
  - FOG                   [GFI, RDS, OUT, JRT, WDC]
    - PSK                 [RDS]
  - FORCAST               [RDS]
  - Harris-Jacobson (HJ2) [RDS]
  - LIX                   [RDS]
  - Linsear Write         [FRT]
  - RIX                   [RDS]
  - SMOG                  [OUT, RDS, WDC]
  - Spache                [RDS]
  - Wheeler-Smith         [RDS, WHE]

  These measures produce plausible results, but need checking:
  - ARI simplified
  - Coleman Formulas (1-4)
  - Danielson-Bryan (1-2)
  - Dickes-Steiwer's Handformel
  - Easy Listening Formula
  - Flesch.de
  - Flesch.fr
  - Flesch.nl
    - some papers use 0.33 and other 0.93 for the average sentence length parameter!
  - Flesch.Brouwer (nl)
  - Fucks
  - Harris-Jacobson (1-5)
  - Neue Wiener Sachtextformeln (1-4)
  - SMOG Qu
  - SMOG C
  - Strain
  - Traenkle-Bailer
  
  These measures look bogus:
  - TRI

  Tools used:
  FRT: http://www.readabilityformulas.com/free-readability-formula-tests.php
  GFI: http://gunning-fog-index.com
  INF: INFLESZ v1.0, http://www.legibilidad.com/home/descargas.html
  JRT: http://juicystudio.com/services/readability.php
  LLB: http://www.leichtlesbar.ch
  OKP: http://www.lefthandlogic.com/htmdocs/tools/okapi/okapi.php
  OUT: http://www.online-utility.org/english/readability_test_and_improve.jsp
  RDS: Readability Studio, version 3.2.7.0 (14 jan 2011)
  TAL: http://www.textalyser.net
  WDC: http://wordscount.info/wc/jsp/clear/analyze_smog.jsp (original SMOG implementation)
  
  Other:
  WHE: example from original article by Wheeler & Smith"
    ))
    return(invisible(NULL))
  } else {}

  # see if just the default parameters should be returned:
  if(identical(parameters, "dput")){
    # rdb_parameters() is defined in koRpus-internal.rdb.params.grades.R
    return(rdb_parameters("dput"))
  } else {}

  # check for given magic numbers
  if(!is.list(parameters)){
    stop(simpleError("Missing accurate paramteter list!"))
  } else {
    # first complain if unknown parameters supplied
    validate_parameters(
      names(parameters),
      index="all"
    )
  }

  all.valid.indices <- rownames(rdb_indices)
  # activate all?
  if(identical(index, "all")){
    index <- rownames(rdb_indices)
  } else {}
  if(identical(index, "fast")){
    # this should be like the defaults but without FOG
    index <- rownames(rdb_indices)[rdb_indices[, "fast"]]
  } else {}

  need.sylls <- rownames(rdb_indices)[rdb_indices[, "sylls"]]

  # use for keep.input
  dropHyphen <- FALSE

  if(isTRUE(analyze.text)){
    #######################
    ## analyze.text=TRUE ##
    #######################

    lang <- language(txt.file)
    docID <- doc_id(txt.file)

    if(identical(nonword.class, "nonpunct")){
      nonword.class <- kRp.POS.tags(lang, tags=c("punct","sentc"), list.classes=TRUE)
    } else {}

    tagged.words.only <- filterByClass(txt.file, corp.rm.class=nonword.class, corp.rm.tag=nonword.tag, update.desc=NULL)
    if(is.null(describe(txt.file, doc_id=docID)[["all.words"]])){
      describe(txt.file, doc_id=docID)[["all.words"]] <- tagged.words.only[["token"]]
    } else {}
    txt.desc <- describe(txt.file, doc_id=docID)

    # check how to handle the hyphen parameter
    # first see if there's results to re-use
    if(all(hasFeature(txt.file, "hyphen"), is.null(hyphen))){
      hyphen <- corpusHyphen(txt.file)
    } else {}
    # we don't need hyphenation for certain formulas,
    # then we'll skip that step automatically
    if(any(index %in% need.sylls)){
      if(is.null(hyphen)){
        hyphen <- hyphen(txt.file, corp.rm.class=nonword.class, corp.rm.tag=nonword.tag, quiet=quiet)
      } else {
        stopifnot(inherits(hyphen, "kRp.hyphen"))
        dropHyphen <- TRUE
      }
      sylls.available <- TRUE
    } else {
      if(inherits(hyphen, "kRp.hyphen")){
        sylls.available <- TRUE
        dropHyphen <- TRUE
      } else {
        # we'll just create an empty object to not disturb the other functions
        hyphen <- new("kRp.hyphen")
        sylls.available <- FALSE
        dropHyphen <- TRUE
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
      num.syll <- describe(hyphen)[["num.syll"]]
      num.monosyll <- describe(hyphen)[["syll.distrib"]]["num", 1]
      pct.monosyll <- num.monosyll * 100 / num.words
      syll.distrib <- describe(hyphen)[["syll.distrib"]]
      syll.uniq.distrib <- describe(hyphen)[["syll.uniq.distrib"]]
      avg.syll.word <- describe(hyphen)[["avg.syll.word"]]
      syll.per100 <- describe(hyphen)[["syll.per100"]]
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
    DS.case.sens <- check_parameters(
      index="Dickes.Steiwer",
      given=parameters[["Dickes.Steiwer"]]
    )[["case.sens"]]
    num.TTR <- slot(TTR(txt.file, case.sens=DS.case.sens, quiet=TRUE), "TTR")
    # some word classes needed by one formula or the other
    num.conjunctions <- sum(taggedText(txt.file)$wclass %in% "conjunction", na.rm=TRUE)
    num.prepositions <- sum(taggedText(txt.file)$wclass %in% "preposition", na.rm=TRUE)
    num.pronouns <- sum(taggedText(txt.file)$wclass %in% "pronoun", na.rm=TRUE)
    num.foreign <- sum(taggedText(txt.file)$wclass %in% "foreign", na.rm=TRUE)
    only.kRp.tags <- all(taggedText(txt.file)[["tag"]] %in% kRp.POS.tags("kRp", list.tags=TRUE))
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
    only.kRp.tags <- FALSE
    if(is.null(txt.features$lang)){
      lang <- character()
    } else {
      lang <- txt.features$lang
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
    } else {
      dropHyphen <- TRUE
    }
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
  all.results <- kRp_readability(
    lang=lang,
    desc=desc,
    param=parameters
  )
  if(isTRUE(analyze.text)){
    if(isTRUE(keep.input)){
      feature(txt.file, "hyphen") <- hyphen
    } else if(is.null(keep.input)){
      if(!isTRUE(dropHyphen)){
        feature(txt.file, "hyphen") <- hyphen
      } else {}
    } else {}
  } else {}

  #####################################
  ## readability measures start here ##
  #####################################

  ## Automated Readability Index (ARI)
  if("ARI" %in% index){
    pf <- check_parameters(
      index="ARI",
      given=parameters[["ARI"]],
      flav_names=list(simple="simplified")
    )
    ARI.grade <- pf[["p"]][["asl"]] * avg.sntc.len + pf[["p"]][["awl"]] * avg.word.len - pf[["p"]][["const"]]
    if(identical(pf[["f"]], "simplified")){
      slot(all.results, "ARI") <- list(flavour=pf[["f"]], index=ARI.grade, grade=NA)
    } else {
      slot(all.results, "ARI") <- list(flavour=pf[["f"]], index=NA, grade=ARI.grade)
    }
  } else{}
  # recursive calls for alternative shortcuts
  if("ARI.NRI" %in% index){
     # setting hyphen to NULL here, because otherwise we could end up with pseudo hyphen objects crashing the function
     slot(all.results, "ARI.NRI") <- slot(kRp.rdb.formulae(txt.file, hyphen=NULL, index=c("ARI"), parameters=list(ARI="NRI"),
      sentc.tag=sentc.tag,
      nonword.class=nonword.class, nonword.tag=nonword.tag, analyze.text=analyze.text, txt.features=txt.features, quiet=TRUE, keep.input=FALSE), "ARI")
  } else {}
  if("ARI.simple" %in% index){
     # setting hyphen to NULL here, because otherwise we could end up with pseudo hyphen objects crashing the function
     slot(all.results, "ARI.simple") <- slot(kRp.rdb.formulae(txt.file, hyphen=NULL, index=c("ARI"), parameters=list(ARI="simple"),
      sentc.tag=sentc.tag,
      nonword.class=nonword.class, nonword.tag=nonword.tag, analyze.text=analyze.text, txt.features=txt.features, quiet=TRUE, keep.input=FALSE), "ARI")
  } else {}

  ## Bormuth Grade Level
  if("Bormuth" %in% index | "DRP" %in% index ){
    # check for word list and add it to params, otherwise skip this index
    if(!"Bormuth" %in% names(word.lists) | is.null(word.lists[["Bormuth"]])){
      warning("Bormuth: Missing word list, hence not calculated.", call.=FALSE)
    } else {
      bormuth.word.list <- read.word.list(word.lists[["Bormuth"]], encoding=fileEncoding)
      pf <- check_parameters(
        index="Bormuth",
        given=parameters[["Bormuth"]]
      )
      fam.words.all.txt <- difficult.words(txt.words.only, bormuth.word.list)
      fam.words.txt <- fam.words.all.txt[["num.listed"]]/num.words
      fam.words.nol <- fam.words.all.txt[["words.not.listed"]]
      bmc <- pf[["p"]][["meanc"]][["const"]] - (pf[["p"]][["meanc"]][["awl"]] * avg.word.len) + (pf[["p"]][["meanc"]][["afw"]] * fam.words.txt^3) -
        (pf[["p"]][["meanc"]][["asl1"]] * avg.sntc.len) + (pf[["p"]][["meanc"]][["asl2"]] * avg.sntc.len^2) - (pf[["p"]][["meanc"]][["asl3"]] * avg.sntc.len^3)

      cloze.crit <- pf[["p"]][["clz"]] / 100
      # here comes the rather long formula...
      bm.grade <- pf[["p"]][["grade"]][["const"]] + (pf[["p"]][["grade"]][["m1"]] * bmc) - (pf[["p"]][["grade"]][["m2"]] * bmc^2) + (pf[["p"]][["grade"]][["m3"]] * bmc^3) +
        (pf[["p"]][["grade"]][["c1"]] * cloze.crit) - (pf[["p"]][["grade"]][["c2"]] * cloze.crit^2) - (pf[["p"]][["grade"]][["c3"]] * cloze.crit^3) -
        (pf[["p"]][["grade"]][["mc1"]] * bmc * cloze.crit) + (pf[["p"]][["grade"]][["mc2"]] * (bmc * cloze.crit)^2) - (pf[["p"]][["grade"]][["mc3"]] * (bmc * cloze.crit)^3)
      # preserve hard words in the desc slot
      if(isTRUE(analyze.text)){
        slot(all.results, "desc")[["Bormuth.NOL"]] <- length(fam.words.nol)
      } else {
        slot(all.results, "desc")[["Bormuth.NOL"]] <- bormuth.word.list
      }
      slot(all.results, "Bormuth") <- list(flavour=pf[["f"]], word.list=bormuth.word.list, not.on.list=fam.words.nol, pct.fam=fam.words.txt * 100, mean.cloze=bmc * 100, grade=bm.grade)
    }
  } else{}

  ## Coleman Formulas
  if("Coleman" %in% index){
    # this formula needs proper POS tags; skip if missing
    if(only.kRp.tags){
      # this text was just tagged with tokenize() and misses important tags
      warning("Coleman: POS tags are not elaborate enough, can't count pronouns and prepositions. Formulae skipped.", call.=FALSE)
    } else {
      pf <- check_parameters(
        index="Coleman",
        given=parameters[["Coleman"]]
      )

      coleman.words   <- describe(hyphen)[["syll.distrib"]]["cum.sum", pf[["p"]][["syll"]]] * 100 / num.words
      coleman.sentc   <- sntc.per100
      coleman.pronoun <- num.pronouns * 100 / num.words
      coleman.prepos  <- num.prepositions * 100 / num.words

      Coleman1 <- (pf[["p"]][["clz1"]][["word"]] * coleman.words) - pf[["p"]][["clz1"]][["const"]]
      Coleman2 <- (pf[["p"]][["clz2"]][["word"]] * coleman.words) + (pf[["p"]][["clz2"]][["sntc"]] * coleman.sentc) - pf[["p"]][["clz2"]][["const"]]
      Coleman3 <- (pf[["p"]][["clz3"]][["word"]] * coleman.words) + (pf[["p"]][["clz3"]][["sntc"]] * coleman.sentc) + (pf[["p"]][["clz3"]][["pron"]] * coleman.pronoun) - pf[["p"]][["clz3"]][["const"]]
      Coleman4 <- (pf[["p"]][["clz4"]][["word"]] * coleman.words) + (pf[["p"]][["clz4"]][["sntc"]] * coleman.sentc) + (pf[["p"]][["clz4"]][["pron"]] * coleman.pronoun) - (pf[["p"]][["clz4"]][["prep"]] * coleman.prepos) - pf[["p"]][["clz4"]][["const"]]
      slot(all.results, "Coleman") <- list(flavour=pf[["f"]], num.pron=coleman.pronoun, num.prep=coleman.prepos, C1=Coleman1, C2=Coleman2, C3=Coleman3, C4=Coleman4)
    }
  } else {}

  ## Coleman-Liau
  if("Coleman.Liau" %in% index){
    pf <- check_parameters(
      index="Coleman.Liau",
      given=parameters[["Coleman.Liau"]]
    )
    co.li.ECP <- pf[["p"]][["ecp"]][["const"]] - (pf[["p"]][["ecp"]][["char"]] * lett.per100) + (pf[["p"]][["ecp"]][["sntc"]] * sntc.per100)
    co.li.grade <- (pf[["p"]][["grade"]][["ecp"]] * (co.li.ECP / 100)) + pf[["p"]][["grade"]][["const"]]
    co.li.short <- pf[["p"]][["short"]][["awl"]] * avg.word.len - pf[["p"]][["short"]][["spw"]] * sntc.per.word - pf[["p"]][["short"]][["const"]]
    slot(all.results, "Coleman.Liau") <- list(flavour=pf[["f"]], ECP=co.li.ECP, grade=co.li.grade, short=co.li.short)
  } else{}

  ## Dale-Chall
  ## TODO: remove names and numbers, they're not counted as unfamiliar!
  if("Dale.Chall" %in% index){
    # check for word list and add it to params, otherwise skip this index
    if(!"Dale.Chall" %in% names(word.lists) | is.null(word.lists[["Dale.Chall"]])){
      warning("Dale-Chall: Missing word list, hence not calculated.", call.=FALSE)
    } else {
      pf <- check_parameters(
        index="Dale.Chall",
        given=parameters[["Dale.Chall"]],
        flav_names=list(
          PSK="Powers-Sumner-Kearl",
          old="Dale-Chall (1948)",
          default="New Dale-Chall (1995)"
        )
      )
      dale.chall.word.list <- read.word.list(word.lists[["Dale.Chall"]], encoding=fileEncoding)
      diff.words.all.txt <- difficult.words(txt.words.only, dale.chall.word.list)
      diff.words.txt <- diff.words.all.txt[["pct.not.listed"]]
      diff.words.nol <- diff.words.all.txt[["words.not.listed"]]
      if(identical(pf[["f"]], "Powers-Sumner-Kearl")){
        dale.chall.raw <- pf[["p"]][["const"]] + (pf[["p"]][["asl"]] * avg.sntc.len) + (pf[["p"]][["dword"]] * diff.words.txt)
        dale.chall.grade <- get.grade.level(dale.chall.raw, measure="Dale.Chall.PSK")
      } else if(identical(pf[["f"]], "Dale-Chall (1948)")){
        dale.chall.raw <- (pf[["p"]][["dword"]] * diff.words.txt) + (pf[["p"]][["asl"]] * avg.sntc.len) + pf[["p"]][["const"]]
        dale.chall.grade <- get.grade.level(dale.chall.raw, measure="Dale.Chall")
      } else {
        dale.chall.raw <- pf[["p"]][["const"]] - (pf[["p"]][["dword"]] * diff.words.txt) - (pf[["p"]][["asl"]] * avg.sntc.len)
        dale.chall.grade <- get.grade.level(dale.chall.raw, measure="Dale.Chall.new")
      }
      # preserve hard words in the desc slot
      if(isTRUE(analyze.text)){
        slot(all.results, "desc")[["Dale.Chall.NOL"]] <- length(diff.words.nol)
      } else {
        slot(all.results, "desc")[["Dale.Chall.NOL"]] <- dale.chall.word.list
      }
      slot(all.results, "Dale.Chall") <- list(flavour=pf[["f"]], word.list=dale.chall.word.list, not.on.list=diff.words.nol, pct=diff.words.txt, raw=dale.chall.raw,
      grade=dale.chall.grade[["grade"]], grade.min=dale.chall.grade[["grade.min"]],
      age=dale.chall.grade[["age"]], age.min=dale.chall.grade[["age.min"]])
    }
  } else{}
  # recursive calls for alternative shortcuts
  if("Dale.Chall.PSK" %in% index){
    slot(all.results, "Dale.Chall.PSK") <- slot(kRp.rdb.formulae(txt.file, hyphen=hyphen, index=c("Dale.Chall"), parameters=list(Dale.Chall="PSK"),
      word.lists=list(Dale.Chall=word.lists[["Dale.Chall"]]),
      sentc.tag=sentc.tag,
      nonword.class=nonword.class, nonword.tag=nonword.tag, analyze.text=analyze.text, txt.features=txt.features, quiet=TRUE, keep.input=FALSE), "Dale.Chall")
  } else {}
  if("Dale.Chall.old" %in% index){
    slot(all.results, "Dale.Chall.old") <- slot(kRp.rdb.formulae(txt.file, hyphen=hyphen, index=c("Dale.Chall"), parameters=list(Dale.Chall="old"),
      word.lists=list(Dale.Chall=word.lists[["Dale.Chall"]]),
      sentc.tag=sentc.tag,
      nonword.class=nonword.class, nonword.tag=nonword.tag, analyze.text=analyze.text, txt.features=txt.features, quiet=TRUE, keep.input=FALSE), "Dale.Chall")
  } else {}

  ## Danielson-Bryan
  # since after tokenization we cannot really count
  # the blanks any more, we'll assume one blank between
  # every two words, hence blanks = num.words - 1
  if("Danielson.Bryan" %in% index){
    pf <- check_parameters(
      index="Danielson.Bryan",
      given=parameters[["Danielson.Bryan"]]
    )

    db.num.blanks <- num.words - 1
    db.avg.let.blnk <- num.all.chars / db.num.blanks
    db.avg.let.sntc <- num.all.chars / num.sentences

    DB1 <- (pf[["p"]][["db1"]][["cpb"]] * db.avg.let.blnk) + (pf[["p"]][["db1"]][["cps"]] * db.avg.let.sntc) - pf[["p"]][["db1"]][["const"]]
    DB2 <- pf[["p"]][["db2"]][["const"]] - (pf[["p"]][["db2"]][["cpb"]] * db.avg.let.blnk) - (pf[["p"]][["db2"]][["cps"]] * db.avg.let.sntc)
    DB2.grade <- get.grade.level(DB2, measure="Danielson.Bryan")

    slot(all.results, "Danielson.Bryan") <- list(flavour=pf[["f"]], avg.blank=db.avg.let.blnk, avg.sentc=db.avg.let.sntc,
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
    pf <- check_parameters(
      index="Dickes.Steiwer",
      given=parameters[["Dickes.Steiwer"]]
    )
    Dickes.Steiwer.score <- pf[["p"]][["const"]] - (log(avg.word.len + 1) * pf[["p"]][["awl"]]) - (log(avg.sntc.len + 1) * pf[["p"]][["asl"]]) - (num.TTR * pf[["p"]][["ttr"]])
    slot(all.results, "Dickes.Steiwer") <- list(flavour=pf[["f"]], TTR=num.TTR, Dickes.Steiwer=Dickes.Steiwer.score)
  } else {}

  ## Easy Listening Formula
  if("ELF" %in% index){
    pf <- check_parameters(
      index="ELF",
      given=parameters[["ELF"]]
    )
    ELF.exsyls <- long.words(pf[["p"]][["syll"]] - 1, hyphen=hyphen)
    ELF.score <- ELF.exsyls / num.sentences
    slot(all.results, "ELF") <- list(flavour=pf[["f"]], num.exsyls=ELF.exsyls, ELF=ELF.score)
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
    pf <- check_parameters(
      index="Farr.Jenkins.Paterson",
      given=parameters[["Farr.Jenkins.Paterson"]],
      flav_names=list(PSK="Powers-Sumner-Kearl")
    )
    fjp.RE <- pf[["p"]][["const"]] - (pf[["p"]][["asl"]] * avg.sntc.len) + (pf[["p"]][["monsy"]] * pct.monosyll)
    fjp.grade <- get.grade.level(fjp.RE, measure="Flesch")
    slot(all.results, "Farr.Jenkins.Paterson") <- list(flavour=pf[["f"]], FJP=fjp.RE, grade=fjp.grade[["grade"]], grade.min=fjp.grade[["grade.min"]])
  } else {}
  # recursive calls for alternative shortcuts
  if("Farr.Jenkins.Paterson.PSK" %in% index){
    slot(all.results, "Farr.Jenkins.Paterson.PSK") <- slot(kRp.rdb.formulae(txt.file, hyphen=hyphen, index=c("Farr.Jenkins.Paterson"), parameters=list(Farr.Jenkins.Paterson="PSK"),
      sentc.tag=sentc.tag,
      nonword.class=nonword.class, nonword.tag=nonword.tag, analyze.text=analyze.text, txt.features=txt.features, quiet=TRUE, keep.input=FALSE), "Farr.Jenkins.Paterson")
  } else {}

  ## Flesch Reading Ease
  if("Flesch" %in% index){
    pf <- check_parameters(
      index="Flesch",
      given=parameters[["Flesch"]],
      flav_names=list(
        default="en (Flesch)",
        en="en (Flesch)",
        de="de (Amstad)",
        es="es (Fernandez-Huerta)",
        "es-s"="es (Szigriszt)",
        nl="nl (Douma)",
        "nl-b"="nl (Brouwer)",
        fr="fr (Kandel-Moles)",
        PSK="Powers-Sumner-Kearl"
      )
    )
    Flesch.RE <- pf[["p"]][["const"]] - (pf[["p"]][["asl"]] * avg.sntc.len) - (pf[["p"]][["asw"]] * avg.syll.word)
    if(identical(pf[["f"]], "Powers-Sumner-Kearl")){
      slot(all.results, "Flesch") <- list(flavour=pf[["f"]], RE=NA, grade=Flesch.RE, grade.min=Flesch.RE, age=(Flesch.RE + 5))
    } else {
      Flesch.grade <- get.grade.level(Flesch.RE, measure="Flesch")
      slot(all.results, "Flesch") <- list(flavour=pf[["f"]], RE=Flesch.RE, grade=Flesch.grade[["grade"]], grade.min=Flesch.grade[["grade.min"]], age=NA)
    }
  } else {}
  # recursive calls for alternative shortcuts
  if("Flesch.PSK" %in% index){
    slot(all.results, "Flesch.PSK") <- slot(kRp.rdb.formulae(txt.file, hyphen=hyphen, index=c("Flesch"), parameters=list(Flesch="PSK"),
      sentc.tag=sentc.tag,
      nonword.class=nonword.class, nonword.tag=nonword.tag, analyze.text=analyze.text, txt.features=txt.features, quiet=TRUE, keep.input=FALSE), "Flesch")
  } else {}
  if("Flesch.de" %in% index){
    slot(all.results, "Flesch.de") <- slot(kRp.rdb.formulae(txt.file, hyphen=hyphen, index=c("Flesch"), parameters=list(Flesch="de"),
      sentc.tag=sentc.tag,
      nonword.class=nonword.class, nonword.tag=nonword.tag, analyze.text=analyze.text, txt.features=txt.features, quiet=TRUE, keep.input=FALSE), "Flesch")
  } else {}
  if("Flesch.es" %in% index){
    slot(all.results, "Flesch.es") <- slot(kRp.rdb.formulae(txt.file, hyphen=hyphen, index=c("Flesch"), parameters=list(Flesch="es"),
      sentc.tag=sentc.tag,
      nonword.class=nonword.class, nonword.tag=nonword.tag, analyze.text=analyze.text, txt.features=txt.features, quiet=TRUE, keep.input=FALSE), "Flesch")
  } else {}
  if("Flesch.Szigriszt" %in% index){
    # Flesch-Szigriszt (Indice de Legibilidad de Flesch-Szigriszt (IFSZ))
    # see http://www.legibilidad.com/home/acercade.html
    slot(all.results, "Flesch.Szigriszt") <- slot(kRp.rdb.formulae(txt.file, hyphen=hyphen, index=c("Flesch"), parameters=list(Flesch="es-s"),
      sentc.tag=sentc.tag,
      nonword.class=nonword.class, nonword.tag=nonword.tag, analyze.text=analyze.text, txt.features=txt.features, quiet=TRUE, keep.input=FALSE), "Flesch")
  } else {}
  if("Flesch.fr" %in% index){
    slot(all.results, "Flesch.fr") <- slot(kRp.rdb.formulae(txt.file, hyphen=hyphen, index=c("Flesch"), parameters=list(Flesch="fr"),
      sentc.tag=sentc.tag,
      nonword.class=nonword.class, nonword.tag=nonword.tag, analyze.text=analyze.text, txt.features=txt.features, quiet=TRUE, keep.input=FALSE), "Flesch")
  } else {}
  if("Flesch.nl" %in% index){
    slot(all.results, "Flesch.nl") <- slot(kRp.rdb.formulae(txt.file, hyphen=hyphen, index=c("Flesch"), parameters=list(Flesch="nl"),
      sentc.tag=sentc.tag,
      nonword.class=nonword.class, nonword.tag=nonword.tag, analyze.text=analyze.text, txt.features=txt.features, quiet=TRUE, keep.input=FALSE), "Flesch")
  } else {}
  if("Flesch.Brouwer" %in% index){
    slot(all.results, "Flesch.Brouwer") <- slot(kRp.rdb.formulae(txt.file, hyphen=hyphen, index=c("Flesch"), parameters=list(Flesch="nl-b"),
      sentc.tag=sentc.tag,
      nonword.class=nonword.class, nonword.tag=nonword.tag, analyze.text=analyze.text, txt.features=txt.features, quiet=TRUE, keep.input=FALSE), "Flesch")
  } else {}

  ## Flesch-Kincaid Grade Level
  if("Flesch.Kincaid" %in% index){
    pf <- check_parameters(
      index="Flesch.Kincaid",
      given=parameters[["Flesch.Kincaid"]]
    )
    Flesch.Kincaid.GL <- (pf[["p"]][["asl"]] * avg.sntc.len) + (pf[["p"]][["asw"]] * avg.syll.word) - pf[["p"]][["const"]]
    slot(all.results, "Flesch.Kincaid") <- list(flavour=pf[["f"]], grade=Flesch.Kincaid.GL, age=Flesch.Kincaid.GL+5)
  } else {}

  ## FORCAST
  if("FORCAST" %in% index){
    pf <- check_parameters(
      index="FORCAST",
      given=parameters[["FORCAST"]],
      flav_names=list(RGL="precise reading grade level")
    )
    FORCAST.monosyll <- describe(hyphen)[["syll.distrib"]]["cum.sum", pf[["p"]][["syll"]]] * 150 / num.words
    FORCAST.grade <- pf[["p"]][["const"]] - (FORCAST.monosyll * pf[["p"]][["mult"]])
    FORCAST.age <- FORCAST.grade + 5
    slot(all.results, "FORCAST") <- list(flavour=pf[["f"]], grade=FORCAST.grade, age=FORCAST.age)
  } else {}
  # recursive calls for alternative shortcuts
  if("FORCAST.RGL" %in% index){
    slot(all.results, "FORCAST.RGL") <- slot(kRp.rdb.formulae(txt.file, hyphen=hyphen, index=c("FORCAST"), parameters=list(FORCAST="RGL"),
      sentc.tag=sentc.tag,
      nonword.class=nonword.class, nonword.tag=nonword.tag, analyze.text=analyze.text, txt.features=txt.features, quiet=TRUE, keep.input=FALSE), "FORCAST")
  } else {}

  ## Fucks Stilcharakteristik
  if("Fucks" %in% index){
    Fucks.raw <- avg.syll.word * avg.sntc.len
    Fucks.grade <- sqrt(Fucks.raw)
    slot(all.results, "Fucks") <- list(Fucks=Fucks.raw, grade=Fucks.grade)
  } else {}

  ## Gunning FOG Index
  if("FOG" %in% index){
    pf <- check_parameters(
      index="FOG",
      given=parameters[["FOG"]],
      flav_names=list(
        PSK="Powers-Sumner-Kearl",
        NRI="New FOG (NRI)"
      )
    )

    FOG.hyphen <- hyphen

    # a list for dropped tokens
    FOG.dropped <- list(names=NA, combi=NA, verbs=NA)

    if(isTRUE(analyze.text)){
      # exclude certain words
      # proper nouns/names will completely be omitted
      FOG.names <- which(tagged.words.only[["wclass"]] == "name")
      # check for verbs ending in -es, -ed, or -ing (or anything else set in pf[["p"]][["suffix"]])
      # these endings must not be counted as syllables
      FOG.verbs <- which(tagged.words.only[["wclass"]] == "verb")
      FOG.verb.suffix <- paste0("(", paste(pf[["p"]][["suffix"]], collapse="|"), ")$")
      FOG.verbs <- FOG.verbs[grepl(FOG.verb.suffix, tagged.words.only[FOG.verbs,"token"])]
      # count one syllable less for these
      if(length(FOG.verbs) > 0){
        FOG.dropped[["verbs"]] <- tagged.words.only[FOG.verbs[hyphenText(FOG.hyphen)[FOG.verbs,"syll"] == pf[["p"]][["syll"]]],]
        hyphenText(FOG.hyphen)[FOG.verbs,"syll"] <- hyphenText(FOG.hyphen)[FOG.verbs,"syll"] - 1
      } else {}
      # syllables of combined words must be counted separately
      # \p{Pd} matches dashes, indicating hyphenated/combined words
      FOG.combi <- which(grepl("\\p{Pd}", tagged.words.only[["token"]], perl=TRUE))
      # next step: split into seperate parts, count syllables for each and
      # drop all if none of the parts is long enough on its own
      FOG.combi.dopped <- c()
      for(combi in FOG.combi){
        combi.to.check <- unlist(strsplit(tagged.words.only[combi, "token"], "\\p{Pd}", perl=TRUE))
        # quite liberal check here, just to ensure that we don't run into empty strings, e.g. when a single dash was tagged as a word
        if(all(any(nchar(combi.to.check, type="width") >= pf[["p"]][["syll"]]), all(nchar(combi.to.check, type="width") >= 1))){
          ## TODO: should this be cached or not?
          FOG.this.combi <- hyphen(combi.to.check, hyph.pattern=lang, quiet=TRUE)
          if(all(hyphenText(FOG.this.combi)[["syll"]] < pf[["p"]][["syll"]])){
            # don't count this as a hard word
            FOG.combi.dopped <- combi
          } else {}
        } else {
          FOG.combi.dopped <- combi
        }
      }

      FOG.num.syll <- hyphenText(FOG.hyphen)$syll
      FOG.all.dropped <- c()
      # disarm the found names
      if(length(FOG.names) > 0){
        FOG.dropped[["names"]] <- tagged.words.only[FOG.names,]
        FOG.all.dropped <- c(FOG.all.dropped, FOG.names)
      } else {}
      if(length(FOG.combi.dopped) > 0){
        FOG.dropped[["combi"]] <- tagged.words.only[FOG.combi.dopped,]
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

    if(identical(pf[["f"]], "Powers-Sumner-Kearl")){
      Gunning.FOG <- pf[["p"]][["const"]] + (pf[["p"]][["asl"]] * avg.sntc.len) + (pf[["p"]][["hword"]] * FOG.hard.words)
    } else if(identical(pf[["f"]], "New FOG (NRI)")){
      FOG.easy.words <- (num.words - FOG.hard.words) * 100 / num.words
      Gunning.FOG <- (((FOG.easy.words + (pf[["p"]][["hword"]] * FOG.hard.words)) / sntc.per100) - pf[["p"]][["const"]]) / pf[["p"]][["div"]]
    } else {
      if(isTRUE(analyze.text)){
        FOG.hard.words <- sum(FOG.num.syll > (pf[["p"]][["syll"]] - 1), na.rm=TRUE) * 100 / num.words
      } else {}
      Gunning.FOG <- (avg.sntc.len + FOG.hard.words) * pf[["p"]][["const"]]
    }
    # preserve hard words in the desc slot
    slot(all.results, "desc")[["FOG.hard.words"]] <- FOG.hard.words.absolute
    slot(all.results, "FOG") <- list(flavour=pf[["f"]], dropped=FOG.dropped, FOG.hard.words=FOG.hard.words.absolute, FOG=Gunning.FOG)
  } else {}
  # recursive calls for alternative shortcuts
  if("FOG.PSK" %in% index){
    slot(all.results, "FOG.PSK") <- slot(kRp.rdb.formulae(txt.file, hyphen=hyphen, index=c("FOG"), parameters=list(FOG="PSK"),
      sentc.tag=sentc.tag,
      nonword.class=nonword.class, nonword.tag=nonword.tag, analyze.text=analyze.text, txt.features=txt.features, quiet=TRUE, keep.input=FALSE), "FOG")
  } else {}
  if("FOG.NRI" %in% index){
    slot(all.results, "FOG.NRI") <- slot(kRp.rdb.formulae(txt.file, hyphen=hyphen, index=c("FOG"), parameters=list(FOG="NRI"),
      sentc.tag=sentc.tag,
      nonword.class=nonword.class, nonword.tag=nonword.tag, analyze.text=analyze.text, txt.features=txt.features, quiet=TRUE, keep.input=FALSE), "FOG")
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
      pf <- check_parameters(
        index="Harris.Jacobson",
        given=parameters[["Harris.Jacobson"]]
      )
      hj.word.list <- read.word.list(word.lists[["Harris.Jacobson"]], encoding=fileEncoding)
      hj.diff.words.all.txt <- difficult.words(txt.words.only, hj.word.list)
      hj.diff.words.txt <- hj.diff.words.all.txt[["pct.not.listed"]]
      hj.diff.words.nol <- hj.diff.words.all.txt[["words.not.listed"]]
      # percentage of long words
      hj.long.words <- (long.words(pf[["p"]][["char"]], txt.desc=txt.desc) * 100) / num.words

      # HJ1 <- 0.094 * V1 + 0.168 * V2 + 0.502
      Harris.Jacobson1 <- (pf[["p"]][["hj1"]][["dword"]] * hj.diff.words.txt) + (pf[["p"]][["hj1"]][["asl"]] * avg.sntc.len) + pf[["p"]][["hj1"]][["const"]]
      # HJ2 <- 0.140 * V1 + 0.153 * V2 + 0.560
      Harris.Jacobson2 <- (pf[["p"]][["hj2"]][["dword"]] * hj.diff.words.txt) + (pf[["p"]][["hj2"]][["asl"]] * avg.sntc.len) + pf[["p"]][["hj2"]][["const"]]
      # HJ3 <- 0.158 * V2 + 0.055 * V3 + 0.355
      Harris.Jacobson3 <- (pf[["p"]][["hj3"]][["asl"]] * avg.sntc.len) + (pf[["p"]][["hj3"]][["lword"]] * hj.long.words) + pf[["p"]][["hj3"]][["const"]]
      # HJ4 <- 0.070 * V1 + 0.125 * V2 + 0.037 * V3 + 0.497 ## best validity, formula of choice
      Harris.Jacobson4 <- (pf[["p"]][["hj4"]][["dword"]] * hj.diff.words.txt) + (pf[["p"]][["hj4"]][["asl"]] * avg.sntc.len) + (pf[["p"]][["hj4"]][["lword"]] * hj.long.words) + pf[["p"]][["hj4"]][["const"]]
      # HJ5 <- 0.118 * V1 + 0.134 * V2 + 0.032 * V3 + 0.424 ## > 3. grade
      Harris.Jacobson5 <- (pf[["p"]][["hj5"]][["dword"]] * hj.diff.words.txt) + (pf[["p"]][["hj5"]][["asl"]] * avg.sntc.len) + (pf[["p"]][["hj5"]][["lword"]] * hj.long.words) + pf[["p"]][["hj5"]][["const"]]

      # preserve hard words in the desc slot
      if(isTRUE(analyze.text)){
        slot(all.results, "desc")[["Harris.Jacobson.NOL"]] <- length(hj.diff.words.nol)
      } else {
        slot(all.results, "desc")[["Harris.Jacobson.NOL"]] <- hj.word.list
      }
      slot(all.results, "Harris.Jacobson") <- list(flavour=pf[["f"]],
        word.list=hj.word.list, not.on.list=hj.diff.words.nol, pct=hj.diff.words.txt,
        pct.long=hj.long.words,
        HJ1=Harris.Jacobson1, HJ2=Harris.Jacobson2, HJ3=Harris.Jacobson3, HJ4=Harris.Jacobson4, HJ5=Harris.Jacobson5)
    }
  } else {}

  ## Laesbarhetsindex (LIX)
  if("LIX" %in% index){
    pf <- check_parameters(
      index="LIX",
      given=parameters[["LIX"]]
    )
    LIX.long.words <- long.words(pf[["p"]][["char"]], txt.desc=txt.desc)
    LIX.idx <- (num.words / num.sentences) + ((LIX.long.words * pf[["p"]][["const"]]) / num.words)
    LIX.rating <- get.grade.level(LIX.idx, measure="LIX")
    LIX.grade <- get.grade.level(LIX.idx, measure="LIX.grade")
    slot(all.results, "LIX") <- list(flavour=pf[["f"]], index=LIX.idx, rating=LIX.rating[["grade"]], grade=LIX.grade[["grade"]], grade.min=LIX.grade[["grade.min"]])
  } else {}

  ## Linsear Write
  if("Linsear.Write" %in% index){
    pf <- check_parameters(
      index="Linsear.Write",
      given=parameters[["Linsear.Write"]]
    )
    hard.words <- (long.words(pf[["p"]][["long.syll"]] - 1, hyphen=hyphen) * 100) / num.words
    easy.words <- 100 - ((long.words(pf[["p"]][["short.syll"]], hyphen=hyphen) * 100) / num.words)
    Linsear.Write.raw <- (easy.words + (hard.words * 3)) / sntc.per100
    if(Linsear.Write.raw > pf[["p"]][["thrs"]]){
      Linsear.Write.RM <- Linsear.Write.raw / 2
    } else {
      Linsear.Write.RM <- (Linsear.Write.raw - 2) / 2
    }
    slot(all.results, "Linsear.Write") <- list(flavour=pf[["f"]], easy.words=easy.words, hard.words=hard.words, raw=Linsear.Write.raw, grade=Linsear.Write.RM)
  } else {}

  ## Neue Wiener Sachtextformeln
  if("nWS" %in% index){
    pf <- check_parameters(
      index="nWS",
      given=parameters[["nWS"]]
    )

    wien.MS <- long.words(pf[["p"]][["ms.syll"]] - 1, hyphen=hyphen) / num.words
    wien.SL <- avg.sntc.len
    wien.IW <- long.words(pf[["p"]][["iw.char"]], txt.desc=txt.desc) / num.words
    wien.ES <- describe(hyphen)[["syll.distrib"]]["cum.sum", pf[["p"]][["es.syll"]]] / num.words

    nWS1 <- (pf[["p"]][["nws1"]][["ms"]] * wien.MS) + (pf[["p"]][["nws1"]][["sl"]] * wien.SL) + (pf[["p"]][["nws1"]][["iw"]] * wien.IW) - (pf[["p"]][["nws1"]][["es"]] * wien.ES) - pf[["p"]][["nws1"]][["const"]]
    nWS2 <- (pf[["p"]][["nws2"]][["ms"]] * wien.MS) + (pf[["p"]][["nws2"]][["sl"]] * wien.SL) + (pf[["p"]][["nws2"]][["iw"]] * wien.IW) - pf[["p"]][["nws2"]][["const"]]

    nWS3 <- (pf[["p"]][["nws3"]][["ms"]] * wien.MS) + (pf[["p"]][["nws3"]][["sl"]] * wien.SL) - pf[["p"]][["nws3"]][["const"]]
    nWS4 <- (pf[["p"]][["nws4"]][["sl"]] * wien.SL) + (pf[["p"]][["nws4"]][["ms"]] * wien.MS) - pf[["p"]][["nws4"]][["const"]]

    slot(all.results, "Wiener.STF") <- list(flavour=pf[["f"]], nWS1=nWS1, nWS2=nWS2, nWS3=nWS3, nWS4=nWS4)
  } else {}

  ## RIX
  if("RIX" %in% index){
    pf <- check_parameters(
      index="RIX",
      given=parameters[["RIX"]]
    )
    RIX.long.words <- long.words(pf[["p"]][["char"]], txt.desc=txt.desc)
    RIX.idx <- RIX.long.words / num.sentences
    RIX.grade <- get.grade.level(RIX.idx, measure="RIX")
    slot(all.results, "RIX") <- list(flavour=pf[["f"]], index=RIX.idx, grade=RIX.grade[["grade"]], grade.min=RIX.grade[["grade.min"]])
  } else {}

  ## Simple Measure of Gobbledygook (SMOG)
  if("SMOG" %in% index){
    pf <- check_parameters(
      index="SMOG",
      given=parameters[["SMOG"]],
      flav_names=list(
        de="de (\"Qu\", Bamberger-Vanecek)",
        C="Fomula C",
        simple="simplified"
      )
    )
    SMOG.grade <- pf[["p"]][["sqrt"]] * sqrt((pf[["p"]][["fact"]] * (long.words(pf[["p"]][["syll"]] - 1, hyphen=hyphen) / num.sentences)) + pf[["p"]][["sqrt.const"]]) + pf[["p"]][["const"]]
    slot(all.results, "SMOG") <- list(flavour=pf[["f"]], grade=SMOG.grade, age=SMOG.grade + 5)
  } else {}
  # recursive calls for alternative shortcuts
  if("SMOG.de" %in% index){
    slot(all.results, "SMOG.de") <- slot(kRp.rdb.formulae(txt.file, hyphen=hyphen, index=c("SMOG"), parameters=list(SMOG="de"),
      sentc.tag=sentc.tag,
      nonword.class=nonword.class, nonword.tag=nonword.tag, analyze.text=analyze.text, txt.features=txt.features, quiet=TRUE, keep.input=FALSE), "SMOG")
  } else {}
  if("SMOG.C" %in% index){
    slot(all.results, "SMOG.C") <- slot(kRp.rdb.formulae(txt.file, hyphen=hyphen, index=c("SMOG"), parameters=list(SMOG="C"),
      sentc.tag=sentc.tag,
      nonword.class=nonword.class, nonword.tag=nonword.tag, analyze.text=analyze.text, txt.features=txt.features, quiet=TRUE, keep.input=FALSE), "SMOG")
  } else {}
  if("SMOG.simple" %in% index){
    slot(all.results, "SMOG.simple") <- slot(kRp.rdb.formulae(txt.file, hyphen=hyphen, index=c("SMOG"), parameters=list(SMOG="simple"),
      sentc.tag=sentc.tag,
      nonword.class=nonword.class, nonword.tag=nonword.tag, analyze.text=analyze.text, txt.features=txt.features, quiet=TRUE, keep.input=FALSE), "SMOG")
  } else {}

  ## Spache
  if("Spache" %in% index){
    # check for word list and add it to params, otherwise skip this index
    if(!"Spache" %in% names(word.lists) | is.null(word.lists[["Spache"]])){
      warning("Spache: Missing word list, hence not calculated.", call.=FALSE)
    } else {
      pf <- check_parameters(
        index="Spache",
        given=parameters[["Spache"]],
        flav_names=list(
          old="First formula (1953)"
        )
      )
      spache.word.list <- read.word.list(word.lists[["Spache"]], encoding=fileEncoding)
      diff.words.all.txt <- difficult.words(txt.words.only, spache.word.list, only.once=TRUE)
      diff.words.txt <- diff.words.all.txt[["pct.not.listed"]]
      diff.words.nol <- diff.words.all.txt[["words.not.listed"]]
      Spache.grade <- pf[["p"]][["asl"]] * avg.sntc.len + pf[["p"]][["dword"]] * diff.words.txt + pf[["p"]][["const"]]
      # preserve hard words in the desc slot
      if(isTRUE(analyze.text)){
        slot(all.results, "desc")[["Spache.NOL"]] <- length(diff.words.nol)
      } else {
        slot(all.results, "desc")[["Spache.NOL"]] <- spache.word.list
      }
      slot(all.results, "Spache") <- list(flavour=pf[["f"]], word.list=spache.word.list, not.on.list=diff.words.nol, pct=diff.words.txt, grade=Spache.grade)
    }
  } else {}
  # recursive calls for alternative shortcuts
  if("Spache.old" %in% index){
    slot(all.results, "Spache.old") <- slot(kRp.rdb.formulae(txt.file, hyphen=hyphen, index=c("Spache"), parameters=list(Spache="old"),
      word.lists=list(Spache=word.lists[["Spache"]]),
      sentc.tag=sentc.tag,
      nonword.class=nonword.class, nonword.tag=nonword.tag, analyze.text=analyze.text, txt.features=txt.features, quiet=TRUE, keep.input=FALSE), "Spache")
  } else {}

  ## Strain Index
  # http://strainindex.wordpress.com/2007/09/25/hello-world/
  if("Strain" %in% index){
    pf <- check_parameters(
      index="Strain",
      given=parameters[["Strain"]]
    )
    # syllables per 3 sentences divided by 10
    Strain.id <- (num.syll / (num.sentences / pf[["p"]][["sent"]])) / pf[["p"]][["const"]]
    slot(all.results, "Strain") <- list(flavour=pf[["f"]], index=Strain.id)
  } else {}

  ## Traenkle-Bailer
  # new Dickes-Steiwer for german texts
  if("Traenkle.Bailer" %in% index){
    # this formula needs proper POS tags; skip if missing
    if(only.kRp.tags){
      # this text was just tagged with tokenize() and misses important tags
      warning("Traenkle.Bailer: POS tags are not elaborate enough, can't count prepositions and conjuctions. Formulae skipped.", call.=FALSE)
    } else {
      pf <- check_parameters(
        index="Traenkle.Bailer",
        given=parameters[["Traenkle.Bailer"]]
      )

      if(all(num.prepositions == 0, num.conjunctions == 0)){
        warning("Traenkle.Bailer: No tokens tagged as preposition or conjunction found. Was the text properly tagged?", call.=FALSE)
      } else {}
      TrBa.pct.prep <- num.prepositions * 100 / num.words
      TrBa.pct.conj <- num.conjunctions * 100 / num.words

      # TB1 = 224.6814 - (79.8304 * ln(awl + 1)) - (12.24032 * ln(asl + 1)) - (1.292857 * %prepos)
      Traenkle.Bailer1 <- pf[["p"]][["TB1"]][["const"]] - (log(avg.word.len + 1) * pf[["p"]][["TB1"]][["awl"]]) - (log(avg.sntc.len + 1) * pf[["p"]][["TB1"]][["asl"]]) - (TrBa.pct.prep * pf[["p"]][["TB1"]][["prep"]])
      # TB2 = 234.1063 - (96.11069 * ln(awl + 1)) - (2.05444 * %prepos) - (1.02805 * %conjct)
      Traenkle.Bailer2 <- pf[["p"]][["TB2"]][["const"]] - (log(avg.word.len + 1) * pf[["p"]][["TB2"]][["awl"]]) - (TrBa.pct.prep * pf[["p"]][["TB2"]][["prep"]]) - (TrBa.pct.conj * pf[["p"]][["TB2"]][["conj"]])

      slot(all.results, "Traenkle.Bailer") <- list(flavour=pf[["f"]], pct.prep=TrBa.pct.prep, pct.conj=TrBa.pct.conj, TB1=Traenkle.Bailer1, TB2=Traenkle.Bailer2)
    }
  } else {}

  ## TRI -- Kuntzsch's Text-Redundanz-Index
# zit. nach Klein, H. (2002). Lesbarkeit und Verstaendlichkeit von Texten (Teil 2). Technische Dokumentation, 2002/07.
#    http://www.doku.net/ausgabe/200207.htm (2011-05-09)
  if("TRI" %in% index){
    pf <- check_parameters(
      index="TRI",
      given=parameters[["TRI"]]
    )
    num.one.syll <- num.words - long.words(pf[["p"]][["syll"]], hyphen=hyphen)
    TRI <- (num.one.syll * pf[["p"]][["word"]]) - (num.punct * pf[["p"]][["pnct"]]) - (num.foreign * pf[["p"]][["frgn"]]) - pf[["p"]][["const"]]
    slot(all.results, "TRI") <- list(flavour=pf[["f"]], short=num.one.syll, punct=num.punct, foreign=num.foreign, TRI=TRI)
  } else {}

  ## Tuldava
  if("Tuldava" %in% index){
    pf <- check_parameters(
      index="Tuldava",
      given=parameters[["Tuldava"]]
    )
    Tuldava.score <- ((pf[["p"]][["syll"]] * num.syll) / (pf[["p"]][["word1"]] * num.words)) * log(((pf[["p"]][["word2"]] * num.words) / (pf[["p"]][["sent"]] * num.sentences)))
#     Tuldava.score <- avg.syll.word * log(avg.sntc.len)
    slot(all.results, "Tuldava") <- list(flavour=pf[["f"]], Tuldava=Tuldava.score)
  } else {}

  ## Wheeler-Smith
  if("Wheeler.Smith" %in% index){
    pf <- check_parameters(
      index="Wheeler.Smith",
      given=parameters[["Wheeler.Smith"]],
      flav_names=list(
        de="de (Bamberger & Vanecek)"
      )
    )
    if(identical(pf[["f"]], "de (Bamberger & Vanecek)")){
      grade.measure <- "Wheeler.Smith.de"
    } else {
      grade.measure <- "Wheeler.Smith"
    }
    Wheeler.Smith.long.words <- 10 * long.words(pf[["p"]][["syll"]] - 1, hyphen=hyphen) / num.words
    Wheeler.Smith.score <- avg.sntc.len * Wheeler.Smith.long.words
    Wheeler.Smith.grade <- get.grade.level(Wheeler.Smith.score, measure=grade.measure)
    slot(all.results, "Wheeler.Smith") <- list(flavour=pf[["f"]], score=Wheeler.Smith.score,
      grade=Wheeler.Smith.grade[["grade"]], grade.min=Wheeler.Smith.grade[["grade.min"]])
  } else {}
  # recursive calls for alternative shortcuts
  if("Wheeler.Smith.de" %in% index){
    slot(all.results, "Wheeler.Smith.de") <- slot(kRp.rdb.formulae(txt.file, hyphen=hyphen, index=c("Wheeler.Smith"), parameters=list(Wheeler.Smith="de"),
      sentc.tag=sentc.tag,
      nonword.class=nonword.class, nonword.tag=nonword.tag, analyze.text=analyze.text, txt.features=txt.features, quiet=TRUE, keep.input=FALSE), "Wheeler.Smith")
  } else {}


#   if("" %in% names(parameters)){
#     pf <- check_parameters(
#       index="",
#       given=parameters[[""]],
#       flav_names=list(
#         =""
#       )
#     )
#     slot(all.results, "<nm>") <- <frml>
#   } else {}

  ## for the time being, give a warning until all implementations have been validated
  needs.warning <- index %in% rownames(rdb_indices)[!rdb_indices[, "validated"]]
  if(all(!isTRUE(quiet), any(needs.warning))){
    warning(paste0("Note: The implementations of these formulas are still subject to validation:\n  ",
    paste(index[needs.warning], collapse=", "),
    "\n  Use the results with caution, even if they seem plausible!",
    "\n  See readability(index=\"validation\") for more details."), call.=FALSE)
  } else {}

  if(isTRUE(as.feature)){
    corpusReadability(txt.file) <- all.results
    return(txt.file)
  } else {
    return(all.results)
  }
}

############################
## internal helper functions

## function read.word.list()
# this function tries to get the actual word vector out of a given
# word list, be it a vector, matrix, data.frame or file name
read.word.list <- function(word.list, encoding="UTF-8"){
  if(all(length(word.list) == 1, is.numeric(word.list))){
      # we got a number, seems to be no word list, but already the result
      return(word.list)
    } else {}
  if(((is.data.frame(word.list) | is.matrix(word.list)) & isTRUE(ncol(word.list) == 1)) |
    (is.vector(word.list) & length(word.list) > 1)){
    local.word.list <- as.character(unlist(word.list))
  } else if(check.file(word.list, mode="exist")){
    wl.file.con <- file(word.list, open="r", encoding=encoding)
    local.word.list <- as.character(unlist(readLines(wl.file.con)))
    close(wl.file.con)
  }
  return(local.word.list)
} ## function read.word.list()

## function long.words()
long.words <- function(min.num, txt.desc=NULL, hyphen=NULL){
  if(!is.null(txt.desc)){
    # will return the number of words with at least min.num characters!
    if(min.num %in% colnames(txt.desc[["lttr.distrib"]])){
      # can be that column name and numer are different, e.g. no words with three letters
      # therefore, we transform the number into character
      result <- txt.desc[["lttr.distrib"]]["cum.inv", as.character(min.num)]
    } else {
      result <- 0
    }
  } else if(!is.null(hyphen)){
    if(min.num %in% colnames(describe(hyphen)[["syll.distrib"]])){
      result <- describe(hyphen)[["syll.distrib"]]["cum.inv", as.character(min.num)]
    } else {
      result <- 0
    }
  } else {
    stop(simpleError("internal bug: long.words() called without data!"))
  }
  return(result)
} ## end function long.words()


## function difficult.words()
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
} ## end function difficult.words()
