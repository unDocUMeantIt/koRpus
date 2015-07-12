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


# these internal functions do the calculations of lexical diversity,
# called by lex.div() and its wrappers

#' @import stats
#' @import utils

# this internal function does the real lexical diversity calculations,
# so it's mostly called by lex.div()

#########################################################
## if this signature changes, check lex.div() as well! ##
#########################################################

kRp.lex.div.formulae <- function(txt, segment=100, factor.size=0.72, min.tokens=9, rand.sample=42, window=100,
    case.sens=FALSE, lemmatize=FALSE, detailed=FALSE,
    measure=c("TTR","MSTTR","MATTR","C","R","CTTR","U","S","K","Maas","HD-D","MTLD","MTLD-MA"),
    char=c("TTR","MATTR","C","R","CTTR","U","S","K","Maas","HD-D","MTLD","MTLD-MA"),
    char.steps=5, log.base=10,
    force.lang=NULL,
    keep.tokens=FALSE,
    corp.rm.class="nonpunct",
    corp.rm.tag=c(), quiet=FALSE){

## TODO:
# - Tuldava (1997): T = LogLog(tokens)/LogLog((tokens/types)+A)^5
#   (was ist A?)
# - Zipf's Z
# - check MTLD charactersitics -- can't this be simplified to save a lot of time?

  ## TODO: validation
  # the following implementations have already been checked against various tools
  # to validate the correctness of calculation. this doesn't mean they always came to identical
  # results at once, since the accuracy of input data (like number of tokens) might vary.
  # but if these differences were manually corrected, the results were similar/identical:
  # - C                    [AYG]
  # - CTTR                 [AYG]
  # - HD-D                 [JMC]
  # - Maas                 [MAS]
  # - MSTTR                [AYG]
  # - MTLD                 [JMC]
  # - R                    [AYG]
  # - TTR                  [TAL]
  # - U                    [AYG]
  # - MTLD-MA              [JMC]
  # 
  # these measures produce plausible results, but need checking:
  # - MATTR
  # - S
  # - K
  # 
  # tools used:
  # AYG: http://aihaiyang.com/synlex/lexical
  # TAL: http://www.textalyser.net
  # 
  # other:
  # JMC: re-calculations by jarvis & mccarthy (thanks!!!)
  # MAS: example data in the original paper by Maas

  if(!is.numeric(factor.size) | factor.size > 1 | factor.size < 0){
    stop(simpleError(paste("Invalid factor size value (must be 0 < factor.size < 1):",factor.size)))
  } else {}
  if(!is.numeric(segment) | segment < 1){
    stop(simpleError(paste("Invalid segment value (must be > 0):",factor.size)))
  } else {}
  # check for optional measures
  if(!any(measure %in% c("TTR","MSTTR","MATTR","C","R","CTTR","U","S","K","Maas","HD-D","MTLD","MTLD-MA")) &
    !any(char %in% c("TTR","MATTR","C","R","CTTR","U","S","K","Maas","HD-D","MTLD","MTLD-MA"))){
      stop(simpleError(paste("You didn't specify at least one valid measure or characteristic!")))
  } else {}

  # get class kRp.tagged from txt object
  # the internal function tag.kRp.txt() will return the object unchanged if it
  # is already tagged, so it's safe to call it with the lang set here
  tagged.text <- tag.kRp.txt(txt, lang=force.lang, objects.only=TRUE)
  # set the language definition
  lang <- language.setting(tagged.text, force.lang)
  if(!isTRUE(quiet)){
    cat("Language: \"",lang,"\"\n", sep="")
  } else {}

  if(identical(corp.rm.class, "nonpunct")){
    corp.rm.class <- kRp.POS.tags(lang, tags=c("punct","sentc"), list.classes=TRUE)
  } else {}

  # calling internal function tagged.txt.rm.classes()
  txt.all.clean <- tagged.txt.rm.classes(tagged.text@TT.res,
      # "lemma" only affects the results if "as.vector=TRUE"
      # so lemmatizing will be done by type.freq() below
      lemma=FALSE, lang=lang,
      corp.rm.class=corp.rm.class,
      corp.rm.tag=corp.rm.tag,
      as.vector=FALSE)
  if(isTRUE(lemmatize)){
    txt.all.tokens <- txt.all.clean[,"lemma"]
  } else {
    txt.all.tokens <- txt.all.clean[,"token"]
  }
  txt.type.freq <- type.freq(txt.all.clean, case.sens=case.sens, lemma=FALSE)
  txt.lemma.freq <- type.freq(txt.all.clean, case.sens=case.sens, lemma=TRUE, fail.if.no.lemmas=lemmatize)
  txt.all.types <- txt.type.freq[,"type"]
  txt.all.lemmas <- txt.lemma.freq[,"type"]
  if(!isTRUE(case.sens)){
    txt.all.tokens <- tolower(txt.all.tokens)
  } else {}
  num.all.tokens <- length(txt.all.tokens)
  num.all.types <- length(txt.all.types)
  num.all.lemmas <- length(txt.all.lemmas)
  num.type.or.lemma <- ifelse(isTRUE(lemmatize), num.all.lemmas, num.all.types)
  # some sanity checks
  if(num.all.tokens < 100){
    warning("Text is relatively short (<100 tokens), results are probably not reliable!", call.=FALSE)
  } else {}

  if(!is.null(char)){
    if(num.all.tokens < char.steps){
      warning("Text is shorter than step size, adjusting \"char.steps\" to 1!", call.=FALSE)
      char.steps <- 1
    } else {}
    # global value for steps if char=c(something)
    num.all.steps <- num.all.tokens %/% char.steps
  } else {}


  # initialize result object
  lex.div.results <- new("kRp.TTR")


  ###################################
  ## diversity measures start here ##
  ###################################


  ## calculate TTR
  if("TTR" %in% measure){
    lex.div.results@TTR <- ttr.calc(num.tokens=num.all.tokens, num.types=num.type.or.lemma, type="TTR")
  } else {}

  ## calculate Herdan's C: log(types) / log(tokens)
  if("C" %in% measure){
    lex.div.results@C.ld <- ttr.calc(num.tokens=num.all.tokens, num.types=num.type.or.lemma, type="C", log.base=log.base)
  } else {}

  ## calculate Guiraud's R: types / sqrt(tokens)
  if("R" %in% measure){
    lex.div.results@R.ld <- ttr.calc(num.tokens=num.all.tokens, num.types=num.type.or.lemma, type="R")
  } else {}

  ## calculate Carroll's CTTR: types / 2*sqrt(tokens)
  if("CTTR" %in% measure){
    lex.div.results@CTTR <- ttr.calc(num.tokens=num.all.tokens, num.types=num.type.or.lemma, type="CTTR")
  } else {}

  ## calculate Uber Index U: (log(tokens))^2 / (log(tokens) - log(types)) 
  if("U" %in% measure){
    lex.div.results@U.ld <- ttr.calc(num.tokens=num.all.tokens, num.types=num.type.or.lemma, type="U", log.base=log.base)
  } else {}

  ## calculate Summer's S: LogLog(types) / LogLog(tokens)
  if("S" %in% measure){
    lex.div.results@S.ld <- ttr.calc(num.tokens=num.all.tokens, num.types=num.type.or.lemma, type="S", log.base=log.base)
  } else {}

  ## calculate Maas' a and lgV0 indices
  if("Maas" %in% measure){
    lex.div.results@Maas <- ttr.calc(num.tokens=num.all.tokens, num.types=num.type.or.lemma, type="Maas", log.base=log.base)
    lex.div.results@lgV0 <- lgV0.calc(num.tokens=num.all.tokens, num.types=num.type.or.lemma, x=0)
    lex.div.results@lgeV0 <- lgV0.calc(num.tokens=num.all.tokens, num.types=num.type.or.lemma, x=0, log.base=exp(1))
    # calculate relative lexical growth, using first half of the text and full text
    Maas.txt.half <- txt.all.tokens[1:(num.all.tokens %/% 2)]
    Maas.num.tokens.half <- length(Maas.txt.half)
    Maas.num.types.half <- length(unique(Maas.txt.half))
    lex.div.results@Maas.grw <- lex.growth(N1=Maas.num.tokens.half, V1=Maas.num.types.half, N2=num.all.tokens, V2=num.type.or.lemma)
  } else {}

  ## calculate MSTTR
  if("MSTTR" %in% measure){
    lex.div.results@MSTTR <- MSTTR.calc(txt.tokens=txt.all.tokens, segment=segment, num.tokens=num.all.tokens)
  } else {}

  ## calculate MATTR
  # needed also for characteristics
  if(any(c("MATTR", "MATTR.char") %in% measure)){
    lex.div.results@MATTR <- MATTR.calc(txt.tokens=txt.all.tokens, window=window, num.tokens=num.all.tokens)
  } else {}

  ## Yule's K, frequency correction
  if("K" %in% measure){
    lex.div.results@K.ld <- k.calc(txt.all.tokens)
  } else {}

  ## calculate HD-D
  if("HD-D" %in% measure){
    lex.div.results@HDD <- hdd.calc(txt.all.tokens, drawn=rand.sample)
  } else {}

  ## calculate MTLD
  if("MTLD" %in% measure){
    lex.div.results@MTLD <- MTLD.calc(txt.all.tokens, factor.size=factor.size, num.tokens=num.all.tokens, detailed=detailed)
  } else {}

  ## calculate MTLD-MA
  if("MTLD-MA" %in% measure){
    # characteristics need detailed results, so discard another setting if present to speed up things
    # the alternative would be to calculate this twice, so it's a no-brainer
    # to comply with user preferences, we'll drop the detailed stuff again, see MTLDMA.char section!
    if("MTLD-MA" %in% char && !isTRUE(detailed)){
      detailed.mtldma <- TRUE
    } else {
      detailed.mtldma <- detailed
    }
    lex.div.results@MTLDMA <- MTLDMA.calc(txt.all.tokens, factor.size=factor.size, num.tokens=num.all.tokens, min.tokens=min.tokens,
      detailed=detailed.mtldma, quiet=quiet)
  } else {}

  ## calculate TTR, C, R, CTTR, U, S and Maas characteristics
  # set up the base function
  ttr.calc.chars <- function(txt.tokens, type="TTR", log.base=log.base){
    if(!isTRUE(quiet)){
      message(paste0(type, ".char: Calculate ",type," values"))
      # give some feedback, so we know the machine didn't just freeze...
      prgBar <- txtProgressBar(min=0, max=num.all.steps, style=3)
    } else {}
    char.results <- t(sapply(1:num.all.steps, function(x){
          curr.token <- x * char.steps
          if(!isTRUE(quiet)){
            # update progress bar
            setTxtProgressBar(prgBar, x)
          } else {}
          char.temp <- c(token=curr.token, value=ttr.calc(txt.tokens=txt.all.tokens[1:curr.token], type=type, log.base=log.base))
          return(char.temp)
        }
      ))
    if(!isTRUE(quiet)){
      # close prograss bar
      close(prgBar)
    } else {}
    return(char.results)
  }

  # do the actual calculations
  if("TTR" %in% char){
    lex.div.results@TTR.char <- ttr.calc.chars(txt.all.tokens, type="TTR")
  } else {}

  if("C" %in% char){
    lex.div.results@C.char <- ttr.calc.chars(txt.all.tokens, type="C", log.base=log.base)
  } else {}

  if("R" %in% char){
    lex.div.results@R.char <- ttr.calc.chars(txt.all.tokens, type="R")
  } else {}

  if("CTTR" %in% char){
    lex.div.results@CTTR.char <- ttr.calc.chars(txt.all.tokens, type="CTTR")
  } else {}

  if("U" %in% char){
    lex.div.results@U.char <- ttr.calc.chars(txt.all.tokens, type="U", log.base=log.base)
  } else {}

  if("S" %in% char){
    lex.div.results@S.char <- ttr.calc.chars(txt.all.tokens, type="S", log.base=log.base)
  } else {}

  if("MATTR" %in% char && num.all.tokens > window){
    # mattr.all.TTR should be available in lex.div.results@MATTR$TTR.win
    # characteristics are just the progressing mean of these TTRs
    mattr.all.TTR <- slot(lex.div.results, "MATTR")$TTR.win
    mattr.num.TTRs <- length(mattr.all.TTR)
    mattr.num.all.steps <- mattr.num.TTRs %/% char.steps
    lex.div.results@MATTR.char <- t(sapply(1:mattr.num.all.steps, function(x){
        mattr.curr.token <- x * char.steps
        mattr.char.temp <- c(token=mattr.curr.token + window, value=mean(mattr.all.TTR[1:mattr.curr.token]))
        return(mattr.char.temp)
      }))
  } else {}

  if("Maas" %in% char){
    lex.div.results@Maas.char <- ttr.calc.chars(txt.all.tokens, type="Maas", log.base=log.base)
    maas.lgV.chars <- function(base){
      if(!isTRUE(quiet)){
        # give some feedback, so we know the machine didn't just freeze...
        prgBar <- txtProgressBar(min=0, max=num.all.steps, style=3)
      } else {}
      lgV0.char.res <- t(sapply(1:num.all.steps, function(x){
            curr.token <- x * char.steps
            if(!isTRUE(quiet)){
              # update progress bar
              setTxtProgressBar(prgBar, x)
            } else {}
            lgV0.char.temp <- c(token=curr.token, value=lgV0.calc(txt.all.tokens[1:curr.token], x=0, log.base=base))
            return(lgV0.char.temp)
          }
        ))
      if(!isTRUE(quiet)){
        # close prograss bar
        close(prgBar)
      } else {}
      return(lgV0.char.res)
    }
    if(!isTRUE(quiet)){
      message("lgV0.char: Calculate lgV0 values")
    } else {}
    lex.div.results@lgV0.char <- maas.lgV.chars(base=10)
    if(!isTRUE(quiet)){
      message("lgeV0.char: Calculate lgeV0 values")
    } else {}
    lex.div.results@lgeV0.char <- maas.lgV.chars(base=exp(1))
  } else {}

  ## calculate K characteristics
  if("K" %in% char){
    if(!isTRUE(quiet)){
      message("K.char: Calculate K values")
      # just some feedback, so we know the machine didn't just freeze...
      prgBar <- txtProgressBar(min=0, max=num.all.steps, style=3)
    } else {}
    lex.div.results@K.char <- t(sapply(1:num.all.steps, function(x){
          curr.token <- x * char.steps
          if(!isTRUE(quiet)){
            # update progress bar
            setTxtProgressBar(prgBar, x)
          } else {}
          k.char.temp <- c(token=curr.token, value=k.calc(txt.all.tokens[1:curr.token]))
          return(k.char.temp)
        }
      ))
    if(!isTRUE(quiet)){
      # close prograss bar
      close(prgBar)
    } else {}
  } else {}

  ## calculate HD-D characteristics
  if("HD-D" %in% char){
    if(!isTRUE(quiet)){
      message("HDD.char: Calculate HD-D values")
      # just some feedback, so we know the machine didn't just freeze...
      prgBar <- txtProgressBar(min=0, max=num.all.steps, style=3)
    } else {}
    lex.div.results@HDD.char <- t(sapply(1:num.all.steps, function(x){
          curr.token <- x * char.steps
          if(!isTRUE(quiet)){
            # update progress bar
            setTxtProgressBar(prgBar, x)
          } else {}
          hdd.value <- hdd.calc(txt.all.tokens[1:curr.token], drawn=rand.sample)[["HDD"]]
          hdd.char.temp <- c(token=curr.token, value=hdd.value)
          return(hdd.char.temp)
        }
      ))
    if(!isTRUE(quiet)){
      # close prograss bar
      close(prgBar)
    } else {}
  } else {}

  ## calculate MTLD characteristics
  if("MTLD" %in% char){
    if(!isTRUE(quiet)){
      message("MTLD.char: Calculate MTLD values")
      # just some feedback, so we know the machine didn't just freeze...
      prgBar <- txtProgressBar(min=0, max=num.all.steps, style=3)
    } else {}
    if(isTRUE(is.na(lex.div.results@MTLD[["all.forw"]]))){
      mtld.char.forw <- mtld.sub.calc(txt.tokens=txt.all.tokens, factor.size=factor.size,
        num.tokens=num.all.tokens, detailed=TRUE)[["MTLD.all"]]
    } else {
      # if MTLD results have already been calculated, recycle the "all.forw" data
      mtld.char.forw <- lex.div.results@MTLD[["all.forw"]]
    }
    lex.div.results@MTLD.char <- t(sapply(1:num.all.steps, function(x){
          curr.token <- x * char.steps
          if(!isTRUE(quiet)){
            # update progress bar
            setTxtProgressBar(prgBar, x)
          } else {}
          mtld.back.value <- mtld.sub.nodata(txt.tokens=rev(txt.all.tokens[1:curr.token]), factor.size=factor.size,
            partial=TRUE, stopAtFirstFactor=FALSE, detailed=FALSE)[["factors"]]
          mtld.forw.value <- mtld.char.forw[which(mtld.char.forw[["end"]] == curr.token), "factors"]
          mtld.char.mean <- mean(c(mtld.forw.value, mtld.back.value))
          # uncomment to debug:
          # print(paste0("token: ", curr.token, "(", txt.all.tokens[curr.token],") forw: ", mtld.forw.value, "back: ", mtld.back.value, " -- MTLD: ", mtld.char.mean))
          mtld.char.value <- curr.token / mtld.char.mean
          mtld.char.temp <- c(token=curr.token, value=mtld.char.value)
          return(mtld.char.temp)
        }
      ))
    if(!isTRUE(quiet)){
      # close prograss bar
      close(prgBar)
    } else {}
  } else {}

  ## calculate MTLD-MA characteristics
  if("MTLD-MA" %in% char){
    # this needs the detailed results, already taken care of by calculating MTLDMA in the first place
    all.factorEnds <- sapply(lex.div.results@MTLDMA[["all"]], function(x) max(x[["end"]]))
    all.factorLengths <- lex.div.results@MTLDMA[["lengths"]][["all"]]
    # if the text is too short, no usable results can be expected
    if(!is.null(all.factorLengths) && length(all.factorEnds) > 0){
      if(!isTRUE(quiet)){
        message("MTLDMA.char: Calculate MTLD-MA values")
        # just some feedback, so we know the machine didn't just freeze...
        prgBar <- txtProgressBar(min=0, max=num.all.steps, style=3)
      } else {}
      lex.div.results@MTLDMA.char <- t(sapply(1:num.all.steps, function(x){
            curr.token <- x * char.steps
            if(!isTRUE(quiet)){
              # update progress bar
              setTxtProgressBar(prgBar, x)
            } else {}
            # well, of course a factor can't be complete if there's less text than the very first factor
            if(curr.token < all.factorEnds[1]){
              mtldma.value <- NA
            } else {
              # see at which point a next full factor would need more text than we have
              lastValidIndex <- min(which(all.factorEnds > curr.token)) - 1
              relevantFactorLenghts <- all.factorLengths[1:lastValidIndex]
              mtldma.value <- mean(relevantFactorLenghts[relevantFactorLenghts > min.tokens])
            }
            # uncomment to debug:
            # print(paste0("token: ", curr.token, "(", txt.all.tokens[curr.token],") -- MTLD-MA: ", mtldma.value, "lastValidIndex: ", lastValidIndex))
            mtldma.char.temp <- c(token=curr.token, value=mtldma.value)
            return(mtldma.char.temp)
          }
        ))
      if(!isTRUE(quiet)){
        # close prograss bar
        close(prgBar)
      } else {}
    } else {
      warning("MTLDMA.char: Not even one factor found, skipping!", call.=FALSE)
    }
    if(!isTRUE(detailed)){
      # in case the user actually didn't want details, remove them from the result
      # anything else would seem confusing
      lex.div.results@MTLDMA[["all"]] <- NA
    } else {}
  }

  lex.div.results@param <- list(segment=segment, factor.size=factor.size, min.tokens=min.tokens,
    rand.sample=rand.sample, window=window, case.sens=case.sens, lemmatize=lemmatize, log.base=log.base)

  # keep raw text material only if explicitly told so
  if(isTRUE(keep.tokens)){
    lex.div.results@tt <- list(tokens=txt.all.tokens, types=txt.type.freq, lemmas=txt.lemma.freq, num.tokens=num.all.tokens, num.types=num.all.types, num.lemmas=num.all.lemmas)
  } else {
    lex.div.results@tt <- list(tokens=character(), types=character(), lemmas=character(), num.tokens=num.all.tokens, num.types=num.all.types, num.lemmas=num.all.lemmas)
  }

  ## for the time being, give a warning until all implementations have been validated
  needs.warning <- measure %in% c("MATTR","S","K")
  if(!isTRUE(quiet) && any(needs.warning)){
    warning(paste0("Note: The implementations of these formulas are still subject to validation:\n  ",
    paste(measure[needs.warning], collapse=", "),
    "\n  Use the results with caution, even if they seem plausible!"), call.=FALSE)
  } else {}
  return(lex.div.results)
}


## function ttr.calc()
# this helper function will be used for nearly all TTR calculations
ttr.calc <- function(txt.tokens=NULL, txt.types=NULL, num.tokens=NULL, num.types=NULL, type="TTR", log.base=10){
  if(is.null(c(txt.tokens, txt.types, num.tokens, num.types))){
    stop(simpleError("Internal function ttr.calc() called without any data!"))
  } else {
    if(is.null(num.tokens)){
      stopifnot(!is.null(txt.tokens))
      num.tokens <-  length(txt.tokens)
    } else {}
    if(is.null(num.types)){
      if(is.null(txt.types)){
        stopifnot(!is.null(txt.tokens))
        txt.types <- unique(txt.tokens)
      } else {}
      num.types <- length(txt.types)
    } else {}
  }
  if(identical(type, "TTR")){
    result <- num.types / num.tokens
  } else {}
  if(identical(type, "C")){
    result <- log(num.types, base=log.base) / log(num.tokens, base=log.base)
  } else {}
  if(identical(type, "R")){
    result <- num.types / sqrt(num.tokens)
  } else {}
  if(identical(type, "CTTR")){
    result <- num.types / sqrt(2 * num.tokens)
  } else {}
  if(identical(type, "U")){
    result <- log(num.tokens, base=log.base)^2 / (log(num.tokens, base=log.base) - log(num.types, base=log.base))
  } else {}
  if(identical(type, "S")){
    result <-  log(log(num.types, base=log.base), base=log.base) / log(log(num.tokens, base=log.base), base=log.base)
  } else {}
  if(identical(type, "Maas")){
    result <- sqrt((log(num.tokens, base=log.base) - log(num.types, base=log.base)) / log(num.tokens, base=log.base)^2)
  } else {}
  return(result)
} ## end function ttr.calc()


## function k.calc()
# function to calculate Yule's K
k.calc <- function(txt.tokens){
  N <- length(txt.tokens)
  txt.types <- unique(txt.tokens)
  # first analize types for their frequencies
  type.freqs <- sapply(txt.types, function(x){
      return(sum(match(txt.tokens, x), na.rm=TRUE))
    })
  # now count the frequencies of frequencies
  freq.freqs <- t(sapply(unique(type.freqs), function(x){
      frq.frq <- sum(match(type.freqs, x), na.rm=TRUE)
      return(c(X=x, fx=frq.frq))
    }))
  # calculate the sum of all fx * X^2
  freq.sum <- sum(freq.freqs[, "X"]^2 * freq.freqs[, "fx"])

  # fill in the whole equasion
  result <- 1e4 * (freq.sum - N) / N^2

  return(result)
} ## end function k.calc()


## function hdd.calc()
# function to calculate HD-D
hdd.calc <- function(hdd.tokens, drawn){
  hdd.types <- unique(hdd.tokens)
  num.hdd.tokens <- length(hdd.tokens)
  num.hdd.types <- length(hdd.types)
  # get probability from hypergeometric distribution for each type
  hdd.type.probs <- sapply(hdd.types, function(x){
      num.types.in.tokens <- sum(match(hdd.tokens, x), na.rm=TRUE)
      num.nontypes <- num.hdd.tokens - num.types.in.tokens
      # probability of having the type at least once is the inverse of not drawing it at all
      # if 'drawn' is larger than number of tokens, set probability to 1
      if(drawn > num.hdd.tokens){
        type.prob <- 1
      } else {
        type.prob <- 1 - dhyper(0, num.types.in.tokens, num.nontypes, drawn)
      }
      return(type.prob)
    })
  summary.probs <- summary(hdd.type.probs)
  sd.probs <- sd(hdd.type.probs)
  hdd.result <- sum(hdd.type.probs)
  hdd.ATTR <- hdd.result / drawn
  results <- list(HDD=hdd.result, ATTR=hdd.ATTR, type.probs=sort(hdd.type.probs, decreasing=TRUE), summary=summary.probs, sd=sd.probs)
  return(results)
} ## end function hdd.calc()


## function MSTTR.calc()
# calculate MSTTR
MSTTR.calc <- function(txt.tokens, segment, num.tokens=NULL){
  if(is.null(num.tokens)){
    num.tokens <- length(txt.tokens)
  } else {}
  if(num.tokens < segment){
    warning(paste0("MSTTR: Skipped calculation, segment size is ", segment, ", but the text has only ", num.tokens, " tokens!"), call.=FALSE)
    return(list(MSTTR=NA, TTR.seg=NA, dropped=NA, sd=NA))
  } else {}
  full.iter <- num.tokens %/% segment
  msttr.dropped <- num.tokens %% segment
  # iterate over all tokens
  msttr.iters <- sapply(0:(full.iter-1), function(x){
                    segm.start <- (x*segment)+1
                    segm.end <- (x*segment)+segment
                    ttr <- ttr.calc(txt.tokens=txt.tokens[segm.start:segm.end])
                    return(ttr)
                  })
  msttr.res <- mean(msttr.iters)
  msttr.sd <- sd(msttr.iters)
  results <- list(MSTTR=msttr.res, TTR.seg=msttr.iters, dropped=msttr.dropped, sd=msttr.sd)
  return(results)
} ## function MSTTR.calc()


## function list.add.type()
# used in lex.div() for MATTR and MTLDMA
list.add.type <- function(this.token, type.list){
  # is this a new type?
  if(is.null(type.list[[this.token]])){
    return(1)
  } else {
    return(type.list[[this.token]] + 1)
  }
} ## end function list.add.type()


## function list.drop.type()
# used in lex.div() for MATTR
list.drop.type <- function(this.token, type.list){
  # is this type in the list in the first place?
  if(is.null(type.list[[this.token]])){
    stop(simpleError(paste0("list.drop.type: '", this.token, "' cannot be found in the list of types, this is odd!")))
  } else {}
  # is this the last type?
  if(type.list[[this.token]] > 1){
    return(type.list[[this.token]] - 1)
  } else {
    # remove from list
    return(NULL)
  }
} ## end function list.drop.type()


## function MATTR.calc()
# calculate MATTR
MATTR.calc <- function(txt.tokens, window, num.tokens=NULL){
  if(is.null(num.tokens)){
    num.tokens <- length(txt.tokens)
  } else {}
  # check if there are less tokens than window size
  if(num.tokens <= window){
    warning(paste0("MATTR: Skipped calculation, window size is ", window, ", but the text has only ", num.tokens, " tokens!"), call.=FALSE)
    return(list(MATTR=NA, TTR.win=NA, sd=NA))
  } else {}

  mattr.list <- list()
  # take the first n tokens
  mattr.win.tokens <- txt.tokens[1:window]
  # fill the initial type list
  for (this.token in mattr.win.tokens){
    mattr.list[[this.token]] <- list.add.type(this.token=this.token, type.list=mattr.list)
  }

  # initialize vector with TTRs
  mattr.all.TTR <- c(length(mattr.list) / window)
  # give the entry the name of the last token
  names(mattr.all.TTR) <- txt.tokens[window]

  # now move through the remaining text.
  # at each step, drop the first token and add the next unused one
  nextToken <- window + 1
  while (nextToken <= num.tokens) {
    # remove the first token
    mattr.list[[txt.tokens[nextToken - window]]] <- list.drop.type(this.token=txt.tokens[nextToken - window], type.list=mattr.list)
    # add the next one
    mattr.list[[txt.tokens[nextToken]]] <- list.add.type(this.token=txt.tokens[nextToken], type.list=mattr.list)
    # calculate new TTR
    mattr.all.TTR <- c(mattr.all.TTR, length(mattr.list) / window)
    names(mattr.all.TTR)[length(mattr.all.TTR)] <- txt.tokens[nextToken]
    # prepare for next round
    nextToken <- nextToken + 1
  }

  results <- list(MATTR=mean(mattr.all.TTR), TTR.win=mattr.all.TTR, sd=sd(mattr.all.TTR))
  return(results)
} ## end function MATTR.calc()


## function lgV0.calc()
# function to calculate Maas' lgV0 index
## TODO: estimate geschwurbel index x for correction
lgV0.calc <- function(txt.tokens=NULL, txt.types=NULL, num.tokens=NULL, num.types=NULL, x=0, log.base=10) {
  if(is.null(c(txt.tokens, txt.types, num.tokens, num.types))){
    stop(simpleError("Internal function lgV0.calc() called without any data!"))
  } else {
    if(is.null(num.tokens)){
      stopifnot(!is.null(txt.tokens))
      num.tokens <- length(txt.tokens)
    } else {}
    if(is.null(num.types)){
      if(is.null(txt.types)){
        stopifnot(!is.null(txt.tokens))
        txt.types <- unique(txt.tokens)
      } else {}
      num.types <- length(txt.types)
    } else {}
  }
  maas.index <- log(num.types, base=log.base) / sqrt(1 - (log(num.types, base=log.base) / (log(num.tokens, base=log.base) + x))^2)
  return(maas.index)
} ## end function lgV0.calc()


## function lex.growth()
# calculates the relative growth of lexical richness in one text while it progresses
# N1/V1 and N2/V2 are therefore tokens/types of the same text, but of different proportions
# Vs is the growth factor ( Vs * 100 = number of new types per 100 new tokens)
# see Maas (1972, p. 87, full ref. in lex.div())
lex.growth <- function(N1, V1, N2, V2, log.base=10){
  # yeah, this isn't the most elegant solution, but it works for the moment...
  a <- seq(0,1,0.001)
  geschw <- t(sapply(a, function(this.a){
      upper <- suppressWarnings(lgV0.calc(num.tokens=N1, num.types=V1, x=log(this.a, base=log.base)))
      lower <- suppressWarnings(lgV0.calc(num.tokens=N2, num.types=V2, x=log(this.a, base=log.base)))
      diffx <- upper - lower
      return(c(upper=upper, lower=lower, diff=diffx, a=this.a))
    }))
  # find the minimum
  got.min <- which.min(abs(geschw[,"diff"]))
  min.a <- a[got.min]
  min.lgV0 <- round(mean(geschw[got.min, "upper"], geschw[got.min, "lower"]), digits=3)

  # lexical growth
  V.grow <- (log(V2, base=log.base) / log(min.a * N2, base=log.base))^3 * (V2 / N2)
  return(c(a=min.a, lgV0=min.lgV0, Vs=V.grow))
} ## end function lex.growth()


## function mtld.sub.nodata()
# by default, this function doesn't return as detailed information, but just basic results
# and should therefore work much faster
# partial: should partial factors be computed? (TRUE for MTLD, FALSE for MTLD-MA)
# stopAtFirstFactor: TRUE for MTLD-MA, FALSE for MTLD
# detailed: keep full factorization details in results (makes things really slow)
# offset: MTLD-MA uses a moving window, therefore the token numbers in the examined sample
#   probably don't match those in the text; this can be corrected by "offset"
mtld.sub.nodata <- function(txt.tokens, factor.size, num.tokens=NULL,
  partial=FALSE, stopAtFirstFactor=TRUE, detailed=FALSE, offset=0){
  if(is.null(num.tokens)){
    num.tokens <- length(txt.tokens)
  } else {}
  mtld.factors <- 0
  mtld.factor.lengths <- c()
  mtld.ttr <- 1
  mtld.start <- 1
  mtld.words <- 2

  if(isTRUE(detailed)){
    # this data frame will store *all* results down the way
    mtld.all.results <- data.frame(start=NA, end=NA, token=rep("", num.tokens), TTR=NA, factors=NA, stringsAsFactors=FALSE)
  } else {
    mtld.all.results <- NA
  }

  while ((mtld.start + 1) < num.tokens){
    while (mtld.ttr >= factor.size && mtld.words <= num.tokens){
      mtld.ttr <- ttr.calc(txt.tokens[mtld.start:mtld.words])
      if(isTRUE(detailed)){
        if(mtld.ttr < factor.size){
          mtld.sub.factor <- mtld.factors + 1
        } else {
          mtld.sub.factor <- mtld.factors + ((1 - mtld.ttr) / (1 - factor.size))
        }
        mtld.all.results[mtld.words, c("start","end","TTR","factors")] <- c(
          start=mtld.start,
          end=mtld.words,
          TTR=mtld.ttr,
          factors=mtld.sub.factor)
      } else {}
      mtld.words <- mtld.words + 1
    }
    if(mtld.ttr < factor.size){
      # found a full factor
      mtld.factors <- mtld.factors + 1
      mtld.factor.lengths <- c(mtld.factor.lengths, mtld.words - mtld.start)
      if(isTRUE(stopAtFirstFactor)){
        break
      } else {
        mtld.ttr <- 1
        mtld.start <- mtld.words
        mtld.words <- mtld.words + 1
      }
    } else {
      # must be the end of the text
      if(isTRUE(partial)){
        # calculate partial last factor
        mtld.factors <- mtld.factors + ((1 - mtld.ttr) / (1 - factor.size))
      } else {}
      break
    }
  }

  if(isTRUE(partial)){
    mtld.factor.lengths.all <- c(mtld.factor.lengths, mtld.words - mtld.start)
  } else {
    mtld.factor.lengths.all <- mtld.factor.lengths
  }

  if(isTRUE(detailed)){
    # tokens are still missing
    mtld.all.results <- na.omit(mtld.all.results)
    mtld.all.results[,"token"] <- txt.tokens[mtld.all.results[,"end"]]
  } else {}

  # respect offset values
  if(isTRUE(detailed) && offset > 0){
    mtld.all.results[,c("start","end")] <- mtld.all.results[,c("start","end")] + offset
  } else {}

  # if there wasn't at least one full factor... happens if the text is too short
  if(length(mtld.factor.lengths) == 0){
    mtld.factor.lengths <- 0
  } else {}
  mtld.results <- list(
    MTLD.all=mtld.all.results,
    factors=mtld.factors,
    lengths=mtld.factor.lengths.all,
    lengths.complete=mtld.factor.lengths)
  return(mtld.results)
} ## end function mtld.sub.nodata()


## function mtld.sub.calc()
# this is just a wrapper for mtld.sub.nodata()
mtld.sub.calc <- function(txt.tokens, factor.size, num.tokens=NULL, detailed=FALSE){
  return(mtld.sub.nodata(txt.tokens=txt.tokens, factor.size=factor.size, num.tokens=num.tokens,
    partial=TRUE, stopAtFirstFactor=FALSE, detailed=detailed, offset=0))
} ## function mtld.sub.calc()


## function MTLD.calc()
# calculate MTLD
# to get only the backwards data (used for characteristics), set "back.only=TRUE)"
MTLD.calc <- function(txt.tokens, factor.size, num.tokens=NULL, back.only=FALSE, detailed=FALSE){
  if(is.null(num.tokens)){
    num.tokens <- length(txt.tokens)
  } else {}

  if(isTRUE(back.only)){
    mtld.results <- mtld.sub.calc(rev(txt.tokens), factor.size=factor.size, detailed=TRUE)[["MTLD.all"]]
  } else {
    mtld.res.forw <- mtld.sub.calc(txt.tokens, factor.size=factor.size, detailed=detailed)
    mtld.res.back <- mtld.sub.calc(rev(txt.tokens), factor.size=factor.size, detailed=detailed)
    mtld.res.mean <- mean(c(mtld.res.forw[["factors"]], mtld.res.back[["factors"]]))
    mtld.len.mean <- mean(c(mtld.res.forw[["lengths"]], mtld.res.back[["lengths"]]))
    mtld.len.sd <- sd(c(mtld.res.forw[["lengths"]], mtld.res.back[["lengths"]]))
    mtld.len.mean.cmp <- mean(c(mtld.res.forw[["lengths.complete"]], mtld.res.back[["lengths.complete"]]))
    mtld.len.sd.cmp <- sd(c(mtld.res.forw[["lengths.complete"]], mtld.res.back[["lengths.complete"]]))
    # this is the final MTLD value
    mtld.res.value <- num.tokens / mtld.res.mean
    mtld.results <- list(
      MTLD=mtld.res.value,
      all.forw=mtld.res.forw[["MTLD.all"]],
      all.back=mtld.res.back[["MTLD.all"]],
      factors=c(forw=mtld.res.forw[["factors"]], mean=mtld.res.mean, back=mtld.res.back[["factors"]]),
      lengths=list(
        forw=mtld.res.forw[["lengths"]],
        forw.compl=mtld.res.forw[["lengths.complete"]],
        mean=mtld.len.mean,
        mean.compl=mtld.len.mean.cmp,
        sd=mtld.len.sd,
        sd.compl=mtld.len.sd.cmp,
        back=mtld.res.back[["lengths"]],
        back.compl=mtld.res.back[["lengths.complete"]]
      )
    )
  }
  return(mtld.results)
} ## end function MTLD.calc()


## function MTLDMA.calc()
MTLDMA.calc <- function(txt.tokens, factor.size, num.tokens=NULL, min.tokens=9, detailed=FALSE, quiet=TRUE){
  if(is.null(num.tokens)){
    num.tokens <- length(txt.tokens)
  } else {}

  if(!isTRUE(quiet)){
    message("MTLDMA: Calculate MTLD-MA values")
    # just some feedback, so we know the machine didn't just freeze...
    prgBar <- txtProgressBar(min=0, max=num.tokens, style=3)
  } else {}

  # we'll simply run mtld.sub.calc() in a loop with changing start tokens
  # and then average the factor results
  nextToken <- 1
  mtldma.list <- list()
  while (nextToken < num.tokens){
    if(!isTRUE(quiet)){
      # update progress bar
      setTxtProgressBar(prgBar, nextToken)
    } else {}
    start.with <- nextToken
    num.tokens.sub <- num.tokens - start.with + 1
    mtldma.fct.tokens <- txt.tokens[start.with:num.tokens]
    sub.calc.result <- mtld.sub.nodata(txt.tokens=mtldma.fct.tokens, factor.size=factor.size,
      stopAtFirstFactor=TRUE, detailed=detailed, offset=(start.with - 1))
    if(sub.calc.result[["factors"]] < 1){
      # we're supposed to quit if we reached the end of the text without a full factor
      break
    } else {}
    mtldma.list[[start.with]] <- sub.calc.result
    nextToken <- nextToken + 1
  }
  if(!isTRUE(quiet)){
    # ew're done -- update and close progress bar
    setTxtProgressBar(prgBar, num.tokens)
    close(prgBar)
  } else {}
  mtldma.len.compl <- unlist(sapply(mtldma.list, function(x) {x[["lengths.complete"]]}))
  # do some checks if the text was long enough to produce at least one valid factor
  if(!is.null(mtldma.len.compl)){
    mtldma.len.compl.use <- mtldma.len.compl[mtldma.len.compl > min.tokens]
    mtldma.len.mean <- mean(mtldma.len.compl)
    mtldma.len.sd <- sd(mtldma.len.compl)
    # this is the final MTLD value
    mtldma.res.value <- mean(mtldma.len.compl.use)
    mtldma.res.sd <- sd(mtldma.len.compl.use)
  } else {
    mtldma.len.mean <- mtldma.len.sd <- mtldma.res.sd <- 0
    mtldma.res.value <- NA
  }
  if(isTRUE(detailed)){
    all.res <- lapply(mtldma.list, function(x) {x[["MTLD.all"]]})
  } else {
    all.res <- NA
  }
  mtldma.results <- list(
    MTLDMA=mtldma.res.value,
    sd=mtldma.res.sd,
    all=all.res,
    lengths=list(
      all=mtldma.len.compl,
      mean=mtldma.len.mean,
      sd=mtldma.len.sd
    )
  )

  return(mtldma.results)
} ## end function MTLDMA.calc()
