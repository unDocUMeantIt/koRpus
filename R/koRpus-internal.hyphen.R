# Copyright 2010-2016 Meik Michalke <meik.michalke@hhu.de>
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

## function explode.letters()
# all possible values of .word.:
# 6 x 1: . w o r d .
#        1 2 3 4 5 6
# 5 x 2: .w wo or rd d.
#        12 23 34 45 56
# 4 x 3: .wo wor ord rd.
#        123 234 345 456
# 3 x 4: .wor word ord.
#        1234 2345 3456
# 2 x 5: .word word.
#        12345 23456
# 1 x 6: .word.
#        123456
#
explode.letters <- function(max.word.length=hyph.max.word.length){
  result <- lapply(
    1:max.word.length,
    function(wl){
      lapply(
        1:wl,
        function(sl){
          lapply(
            1:(wl-sl+1),
            function(x){
              x:(x+sl-1)
            }
          )
        }
      )
    }
  )
  return(result)
} ## end function explode.letters()

# generate internal object with all possible patterns of subcharacters
# for hyphenation, to speed up the process
all.patterns <- explode.letters()


## function explode.word()
# using the provided patterns, split an actual word into its subpatterns
# - min.pattern/max.pattern: set the minimum and maximum length of available
#   patterns, makes no sense to split further in the first place
explode.word <- function(word, min.pattern=2L, max.pattern=5L){
  word.length <- nchar(word)
  if(word.length > hyph.max.word.length){
    stop(
      simpleError(
        paste0(
          "Found a word with more than ", hyph.max.word.length, " characters:\n  ",
          word, "\n",
          "This was not expected and is not covered by the defaults, please inform the package author(s)!"
        )
      )
    )
  } else {}
  if(word.length <= min.pattern){
    result <- data.frame(frag=word, on=1L, off=min(word.length, min.pattern))
  } else {
    result.list <- sapply(
      unlist(all.patterns[[word.length]][min.pattern:min(word.length, max.pattern)], recursive=FALSE),
      function(lttrs){
        return(
          # already include a dummy 'match' row
          c(
            substr(word, lttrs[1], max(lttrs)), # frag
            lttrs[1],                           # on
            max(lttrs),                         # off
            NA                                  # match
          )
        )
      },
      USE.NAMES=FALSE
    )
    result <- matrix(unlist(result.list), nrow=4L, dimnames=list(c("frag","on","off","match"),NULL))
  }
  return(result)
} ## function explode.word()


## function get.hyph.cache()
get.hyph.cache <- function(lang){
  # don't try anything while cache is locked
  locked <- mget("hyphenCacheLock", envir=as.environment(.koRpus.env), ifnotfound=list(hyphenCacheLock=FALSE))[["hyphenCacheLock"]]
  while(isTRUE(locked)){
    Sys.sleep(0.5)
    locked <- mget("hyphenCacheLock", envir=as.environment(.koRpus.env), ifnotfound=list(hyphenCacheLock=FALSE))[["hyphenCacheLock"]]
  }
  # simply get cache from current koRpus environment
  # returns NULL if none exists
  return(mget("hyphenCache", envir=as.environment(.koRpus.env), ifnotfound=list(NULL))[["hyphenCache"]][[lang]])
}
## end function get.hyph.cache()


## function check.hyph.cache()
# called by hyphen(), returns either the cached entry, or NULL
# - missing: if TRUE, all elements of token are looked up at once, and
#   a vector of all missing tokens is returned, or NULL
# - multiple: if TRUE, token must be a character vector, result is a data.frame
check.hyph.cache <- function(lang, token, cache=get.hyph.cache(lang=lang), missing=FALSE, multiple=FALSE){
  result <- NULL
  if(is.null(cache)){
    # no cache, no hit...
    if(isTRUE(missing)){
      result <- token
    } else {}
  } else {
    if(isTRUE(missing)){
      missing.tokens <- token[!token %in% names(cache)]
      if(length(missing.tokens) > 0){
        result <- missing.tokens
      } else {}
    } else if(isTRUE(multiple)){
      # update fields with data from cache if available
      result <- as.data.frame(t(
        sapply(token,
          function(tk){
            inCache <- cache[[tk]]
            if(is.null(inCache)){
              return(c(syll=1, word=tk, token=tk))
            } else {
              return(c(syll=inCache[["syll"]], word=inCache[["word"]], token=tk))
            }
          },
          USE.NAMES=FALSE
        )
      ), stringsAsFactors=FALSE)
      result[["syll"]] <- as.numeric(result[["syll"]])
    } else {
      # check if this word was hyphenated before;
      # will be NULL if not found
      result <- cache[[token]]
    }
  }
  return(result)
} ## end function check.hyph.cache()


## function set.hyph.cache()
# writes (probably new) cache data back to the environment
# - append: a named list of new data (name is the token, value another named list
#           containing "syll" and "word", respectively)
set.hyph.cache <- function(lang, append=NULL, cache=get.hyph.cache(lang=lang)){
  # don't try anything while cache is locked
  locked <- mget("hyphenCacheLock", envir=as.environment(.koRpus.env), ifnotfound=list(hyphenCacheLock=FALSE))[["hyphenCacheLock"]]
  while(isTRUE(locked)){
    Sys.sleep(0.2)
    locked <- mget("hyphenCacheLock", envir=as.environment(.koRpus.env), ifnotfound=list(hyphenCacheLock=FALSE))[["hyphenCacheLock"]]
  }
  # now *we* lock the cache
  assign("hyphenCacheLock", TRUE, envir=as.environment(.koRpus.env), inherits=TRUE)
  all.kRp.env.hyph <- mget("hyphenCache", envir=as.environment(.koRpus.env), ifnotfound=list(NULL))[["hyphenCache"]]
  if(is.null(all.kRp.env.hyph)){
    all.kRp.env.hyph <- new.env()
  } else {}
  # append result to cache
  if(!is.null(append)){
    # could be there is no cache yet
    if(is.null(cache)){
      cache <- new.env()
    } else {}
    # using arbitrary character stuff for names might fail
    try(
      cache <- as.environment(modifyList(as.list(cache), append))
    )
  } else {
    if(is.null(cache)){
      # hm, if both is null, don't do anything
      assign("hyphenCacheLock", FALSE, envir=as.environment(.koRpus.env), inherits=TRUE)
      return(invisible(NULL))
    } else {}
  }

  all.kRp.env.hyph[[lang]] <- cache
  assign("hyphenCache", all.kRp.env.hyph, envir=as.environment(.koRpus.env))
  # unlock cache
  assign("hyphenCacheLock", FALSE, envir=as.environment(.koRpus.env), inherits=TRUE)
  return(invisible(NULL))
} ## end function set.hyph.cache()


## function read.hyph.cache.file()
# reads a dumped chace file, if "file" is not NULL and does exist
read.hyph.cache.file <- function(lang, file=get.kRp.env(hyph.cache.file=TRUE, errorIfUnset=FALSE), quiet=FALSE){
  if(is.null(file)){
    return(invisible(NULL))
  } else {}

  cache.file.path <- normalizePath(file, mustWork=FALSE)
  if(!file.exists(cache.file.path)){
    # if the file is not there yet, create one
    write.hyph.cache.file(lang=lang, quiet=quiet)
  } else {}
  # only reload the file if it changed or wasn't loaded at all yet
  cacheFileInfo.new <- file.info(cache.file.path)
  cacheFileInfo.old <- mget("hyphenCacheFile", envir=as.environment(.koRpus.env), ifnotfound=list(NULL))[["hyphenCacheFile"]]
  if(identical(cacheFileInfo.new, cacheFileInfo.old[[lang]])){
    # file doesn't seem to have changed
    return(invisible(NULL))
  } else if(is.null(cacheFileInfo.old)){
    # this must be the first time we try to read the file
    cacheFileInfo.old <- list()
  } else {}

  # set koRpus.hyph.cache to NULL to suppress R CMD check warning
  koRpus.hyph.cache <- NULL
  load(cache.file.path)
  # data will be checked by set.hyph.cache(), so no need to worry here
  # but the loaded data must contain an environment named "koRpus.hyph.cache"
  if(is.null(koRpus.hyph.cache)){
    stop(simpleError("The cache file you provided does not contain koRpus-ready hyphenation data!"))
  } else {
    # cache format changed with 0.10-2, make sure we're good
    if(is.data.frame(koRpus.hyph.cache)){
      warning("Cache file format has changed, trying conversion. If you run into errors, delete your old cache files!", call.=FALSE)
      koRpus.hyph.cache <- setNames(
        object=lapply(
          seq_along(koRpus.hyph.cache[["token"]]),
          function(n){
            return(
              list(
                syll=as.numeric(koRpus.hyph.cache[n,"syll"]),
                word=as.character(koRpus.hyph.cache[n,"word"])
              )
            )
          }
        ),
        nm=koRpus.hyph.cache[["token"]]
      )
    } else {}
  }
  # set new file data to prevent from reloading if unchanged
  cacheFileInfo.old[[lang]] <- cacheFileInfo.new
  assign("hyphenCacheFile", cacheFileInfo.old, envir=as.environment(.koRpus.env))

  # write loaded data to environment
  set.hyph.cache(lang=lang, append=koRpus.hyph.cache, cache=get.hyph.cache(lang=lang))

  return(invisible(NULL))
} ## end function read.hyph.cache.file()


## function write.hyph.cache.file()
# dumps cache data into a file, if "file" is not NULL. if it doesn't exist, it will be created
write.hyph.cache.file <- function(lang, file=get.kRp.env(hyph.cache.file=TRUE, errorIfUnset=FALSE), quiet=FALSE){
  if(is.null(file)){
    return(invisible(NULL))
  } else {}

  cache.file.path <- normalizePath(file, mustWork=FALSE)
  if(!file.exists(cache.file.path)){
    if(!isTRUE(quiet)){
      message(paste0("Cache file does not exist and will be created:\n  ", cache.file.path))
    } else {}
  } else {}

  koRpus.hyph.cache <- get.hyph.cache(lang=lang)
  save(koRpus.hyph.cache, file=cache.file.path)

  return(invisible(NULL))
} ## end function write.hyph.cache.file()


## function hyphen.word()
# this helper function is being called by kRp.hyphen.calc(), see below
hyphen.word <- function(
    word,
    hyph.pattern=NULL,
    min.length=4L,
    rm.hyph=TRUE,
    min.pattern=2L,
    max.pattern=5L,
    as.cache=FALSE
  ){
    # consider min length of word?
    if(nchar(word) < min.length){
      if(isTRUE(as.cache)){
        return(list(syll=1, word=word))
      } else {
        return(c(syll=1, word=word))
      }
    } else {}
    word.orig <- word
    ## remove hyphens in word
    if(isTRUE(rm.hyph)){
      word <- gsub("-", "", word)
    } else {}
    # non-letters like leading dots confuse the algorithm. we'll remove any non-alphabetic character
    word <- gsub("[^\\p{L}]+", "", word, perl=TRUE)
    # if this removed all of the token, guess we're finished
    if (identical(word, "")){
      if(isTRUE(as.cache)){
        return(list(syll=1, word=word.orig))
      } else {
        return(c(syll=1, word=word.orig))
      }
    } else {}
    ## convert to lowercase
    word <- tolower(word)
    ## transform "word" to ".word."
    word.dotted <- paste0(".", word, ".")
    word.length <- nchar(word.dotted)

    # create word fragments ".wo", ".wor"... "rd."
    matched.patterns <- explode.word(word.dotted, min.pattern=min.pattern, max.pattern=max.pattern)
    # find all matching patterns of the word fragments
    matched.patterns["match",] <- sapply(matched.patterns["frag",],
      function(f){
        in.patterns <- slot(hyph.pattern, "pattern")[[f]]
        return(ifelse(is.null(in.patterns), NA, in.patterns[["nums"]]))
      },
      USE.NAMES=FALSE
    )
    # now let's add the found matches and find the maximum
    matched.pat.index <- !is.na(matched.patterns["match",])
    if(sum(matched.pat.index) > 0){
      pattern.matrix <- sapply(which(matched.pat.index), function(got.match){
          word.on <- max(1, (as.numeric(matched.patterns["on",got.match]) - 1))
          word.off <- as.numeric(matched.patterns["off",got.match])
          match.num.code <- unlist(strsplit(matched.patterns["match",got.match], split=""))
          results <- c(rep(0, word.on - 1), match.num.code, rep(0, word.length - word.off))
        },
        USE.NAMES=FALSE
      )
      # this is the vector with the max values for the dotted word
      pattern.max <- as.numeric(apply(pattern.matrix, 1, max))

      # we'll never hyphenate before a word...
      pattern.max <- pattern.max[-c(1, length(pattern.max))]
      # ... never after the first or before the last letter
      pattern.max[c(1,length(pattern.max)-1,length(pattern.max))] <- 0

      # filter odd positions (count syllables)
      possible.hyphs <- (pattern.max %% 2) != 0
      syllables <- sum(possible.hyphs) + 1
      # recreate word with hyphens
      add.hyphen <- which(possible.hyphs)
      if(isTRUE(rm.hyph)){
        hyph.word <- unlist(strsplit(gsub("-", "", word.orig), split=""))
      } else {
        hyph.word <- unlist(strsplit(word.orig, split=""))
      }
      for (letter in add.hyphen) {
        hyph.word[letter] <- paste0(hyph.word[letter], "-")
      }
      hyph.word <- paste(hyph.word, collapse="")
      # in cases where previous hyphenations were already removed and here returned,
      # don't return double them up
      hyph.word <- gsub("-+", "-", hyph.word)
      if(isTRUE(as.cache)){
        hyph.result <- list(syll=syllables, word=hyph.word)
      } else {
        hyph.result <- c(syll=syllables, word=hyph.word)
      }
    } else {
      ## no hyphenation
      if(isTRUE(as.cache)){
        hyph.result <- list(syll=1, word=word.orig)
      } else {
        hyph.result <- c(syll=1, word=word.orig)
      }
    }
    # this will return *three* elements if as.cache is TRUE
    return(hyph.result)
} ## end function hyphen.word()


# this internal function does the real hyphenations,
# so it's mostly called by hyphen()

########################################################
## if this signature changes, check hyphen() as well! ##
########################################################

# min.length is set to 4 because we'll never hyphenate after the first of before the last letter, so
# words with three letters or less cannot be hyphenated
kRp.hyphen.calc <- function(words, hyph.pattern=NULL, min.length=4L, rm.hyph=TRUE,
  quiet=FALSE, cache=TRUE, lang=NULL){

  stopifnot(is.character(words))

  # to avoid needless NOTEs from R CMD check
  token <- NULL

  # check for hyphenation pattern.
  if(is.null(hyph.pattern)){
    if(!is.null(lang)){
      # this way the text object defines pattern language
      hyph.pattern <- load.hyph.pattern(lang)
    } else {
      stop(simpleError("No language definition available. Set \"hyph.pattern\"!"))
    }
  } else {
    if(!inherits(hyph.pattern, "kRp.hyph.pat")){
      # the internal function load.hyph.pattern() will return what we need
      hyph.pattern <- load.hyph.pattern(hyph.pattern)
    } else {
      # optimize the pattern object
      hyph.pattern <- optimize.hyph.pattern(hyph.pattern)
    }
    # the other way: take language from hyph.pattern
    # overwrites lang in tagged.text
    lang <- slot(hyph.pattern, "lang")
  }
  if(isTRUE(cache)){
    # check if cached hyphenation data has been set with set.kRp.env().
    # if so, the data will directly be coped to koRpus' environment
    read.hyph.cache.file(lang=lang, file=get.kRp.env(hyph.cache.file=TRUE, errorIfUnset=FALSE), quiet=quiet)
    # set a variable to check if we changed the data at all, to later skip the writing back part if possible
    writeBackCache <- new.env()
    assign("changed", FALSE, envir=writeBackCache)
  } else {
    writeBackCache <- NULL
  }

  if(!isTRUE(quiet)){
    # feed back the hypenation we're using
    message(paste0("Hyphenation (language: ", lang, ")"))
  } else {}

  # min-lenth and max-length of patterns
  min.pat <- slot(hyph.pattern, "min.pat")
  max.pat <- slot(hyph.pattern, "max.pat")

  ## main loop
  # build a vector with all possible word fragments
  # check for matches of the fragment vector in the pattern db
  uniqueWords <- unique(words)

  if(!isTRUE(quiet)){
    # counter to get some feedback
    .iter.counter <- new.env()
    assign("counter", 1, envir=.iter.counter)
  } else {}

  if(isTRUE(cache)){
    # the fastest way to fill the cache is to first check what types are missing,
    # hyphenate them and append them to cache in *one* go and *then* fetch results
    # for the actual hyphenation all from cache
    typesMissingInCache  <- check.hyph.cache(lang=lang, token=uniqueWords, missing=TRUE)
    if(!is.null(typesMissingInCache)){
      # throw out uncachable tokens right away
      typesMissingInCache <- typesMissingInCache[nchar(typesMissingInCache) >= min.length]
      if(length(typesMissingInCache) > 0){
        if(!isTRUE(quiet)){
          # give some feedback, so we know the machine didn't just freeze...
          prgBar <- txtProgressBar(min=0, max=length(typesMissingInCache), style=3)
        } else {}
        typesMissingHyphenated <- setNames(
          object=lapply(
            typesMissingInCache,
            function(nw){
              if(!isTRUE(quiet)){
                # update prograss bar
                iteration.counter <- get("counter", envir=.iter.counter)
                setTxtProgressBar(prgBar, iteration.counter)
                assign("counter", iteration.counter + 1, envir=.iter.counter)
              } else {}
              return(hyphen.word(
                nw,
                hyph.pattern=hyph.pattern,
                min.length=min.length,
                rm.hyph=rm.hyph,
                min.pattern=min.pat,
                max.pattern=max.pat,
                as.cache=TRUE
              ))
            }
          ),
          nm=typesMissingInCache
        )
        typesMissingHyphenated[["syll"]] <- as.numeric(typesMissingHyphenated[["syll"]])
        set.hyph.cache(lang=lang, append=typesMissingHyphenated)
        assign("changed", TRUE, envir=writeBackCache)
        if(!isTRUE(quiet)){
          # close prograss bar
          close(prgBar)
        } else {}
      } else {}
    } else {}
    # fetch results from cache in one go
    hyph.df <- check.hyph.cache(lang=lang, token=words, multiple=TRUE)
  } else {
    if(!isTRUE(quiet)){
      # give some feedback, so we know the machine didn't just freeze...
      prgBar <- txtProgressBar(min=0, max=length(uniqueWords), style=3)
    } else {}
    # initialize result data.frame
    hyph.df <- data.frame(
      syll=1,
      word="",
      token=as.character(words),
      stringsAsFactors=FALSE
    )
    for (nw in uniqueWords){
      if(!isTRUE(quiet)){
        # update prograss bar
        iteration.counter <- get("counter", envir=.iter.counter)
        setTxtProgressBar(prgBar, iteration.counter)
        assign("counter", iteration.counter + 1, envir=.iter.counter)
      } else {}
      hyphenate.results <- hyphen.word(
        nw,
        hyph.pattern=hyph.pattern,
        min.length=min.length,
        rm.hyph=rm.hyph,
        min.pattern=min.pat,
        max.pattern=max.pat,
        as.cache=FALSE
      )[c("syll","word")]
      hyph.df[hyph.df[["token"]] == nw, "syll"] <- as.numeric(hyphenate.results["syll"])
      hyph.df[hyph.df[["token"]] == nw, "word"] <- hyphenate.results["word"]
    }
    if(!isTRUE(quiet)){
      # close prograss bar
      close(prgBar)
    } else {}
  }

  ## compute descriptive statistics
  num.syll <- sum(hyph.df$syll, na.rm=TRUE)
  syll.distrib <- value.distribs(hyph.df$syll)
  syll.uniq.distrib <- value.distribs(unique(hyph.df)$syll)
  avg.syll.word <- mean(hyph.df$syll, na.rm=TRUE)
  syll.per100 <- avg.syll.word * 100

  desc.stat.res <- list(
    num.syll=num.syll,
    syll.distrib=syll.distrib,
    syll.uniq.distrib=syll.uniq.distrib,
    avg.syll.word=avg.syll.word,
    syll.per100=syll.per100
  )

  if(isTRUE(cache) && isTRUE(get("changed", envir=writeBackCache))){
    # check if cached hyphenation data has been set with set.kRp.env().
    # if so and if we added to it here, the current data will be written back to that file
    write.hyph.cache.file(lang=lang, file=get.kRp.env(hyph.cache.file=TRUE, errorIfUnset=FALSE), quiet=quiet)
  } else {}

  results <- new("kRp.hyphen", lang=lang, desc=desc.stat.res, hyphen=hyph.df[c("syll","word")])

  return(results)
} ## end function kRp.hyphen.calc()


## function optimize.hyph.pattern()
# replaces the pattern matrix with a hashed environment
optimize.hyph.pattern <- function(hyph.pat){
  pattern.matrix <- slot(hyph.pat, "pattern")
  pattern.list <- setNames(
    object=lapply(
      seq_along(pattern.matrix[,"char"]),
      function(n){
        return(
          list(
            # nums must remain character to keep leading zeroes!
            nums=as.character(pattern.matrix[n,"nums"]),
            orig=as.character(pattern.matrix[n,"orig"])
          )
        )
      }
    ),
    nm=pattern.matrix[,"char"]
  )
  # "kRp.hyph.pat.env" in an internal class, defined in 01_class_07_kRp.hyph.pat.R
  new.hyph.pat <- new("kRp.hyph.pat.env",
    lang=slot(hyph.pat, "lang"),
    min.pat=min(nchar(pattern.matrix[,"char"])),
    max.pat=max(nchar(pattern.matrix[,"char"])),
    pattern=as.environment(pattern.list)
  )
  return(new.hyph.pat)
} ## end function optimize.hyph.pattern()


## function load.hyph.pattern()
load.hyph.pattern <- function(lang){
  # to avoid needless NOTEs from R CMD check
  hyph.pat <- NULL

  lang <- is.supported.lang(lang, support="hyphen")
  # check for additional package information, in case we're
  # importing hyphen patterns from a third party package
  lang.names <- names(lang) %in% "package"
  if(length(lang) > 1 & any(lang.names)){
    hyph.package <- lang[["package"]]
    lang <- lang[!lang.names][[1]]
  } else {
    hyph.package <- "koRpus"
  }
  # well populate the internal environment with optimized patterns
  if(!exists(paste0("hyph.", lang), envir=as.environment(.koRpus.env), inherits=FALSE)){
    # we'll load the hyphen pattern, get it here and check its format
    # this way packages can carry both old and new pattern formats
    data(list=paste0("hyph.", lang), package=hyph.package, envir=as.environment(.koRpus.env))
    hyph.pat.optim <- get(paste0("hyph.", lang), envir=as.environment(.koRpus.env))
    if(!inherits(hyph.pat.optim, "kRp.hyph.pat.env")){
      # optimization is only needed for packages with old pattern objects
      # this should not be an issue, as it's quite fast and happens only once a pattern set is loaded
      hyph.pat.optim <- optimize.hyph.pattern(hyph.pat.optim)
      # replace hyph.XX with optimized object
      assign(paste0("hyph.", lang), hyph.pat.optim, envir=as.environment(.koRpus.env))
    } else {}
  } else {
    hyph.pat.optim <- get(paste0("hyph.", lang), envir=as.environment(.koRpus.env))
  }
  # return optimized patterns
  return(hyph.pat.optim)
} ## end function load.hyph.pattern()
