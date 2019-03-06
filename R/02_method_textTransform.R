# Copyright 2010-2019 Meik Michalke <meik.michalke@hhu.de>
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


#' Letter case transformation
#' 
#' Transforms text in koRpus objects token by token.
#'
#' This method is mainly intended to produce text material for experiments.
#'
#' @param txt An object of class \code{\link[koRpus:kRp.txt.trans-class]{kRp.txt.trans}}, \code{\link[koRpus:kRp.tagged-class]{kRp.tagged}},
#'    \code{\link[koRpus:kRp.txt.freq-class]{kRp.txt.freq}} or \code{\link[koRpus:kRp.analysis-class]{kRp.analysis}}.
#' @param scheme One of the following character strings:
#' \itemize{
#'   \item {\code{"minor"}} {Start each word with a lowercase letter.}
#'   \item {\code{"all.minor"}} {Forces all letters into lowercase.}
#'   \item {\code{"major"}} {Start each word with a uppercase letter.}
#'   \item {\code{"all.major"}} {Forces all letters into uppercase.}
#'   \item {\code{"random"}} {Randomly start words with uppercase or lowercase letters.}
#'   \item {\code{"de.norm"}} {German norm: All names, nouns and sentence beginnings start with an uppercase letter,
#'      anything else with a lowercase letter.}
#'   \item {\code{"de.inv"}} {Inversion of \code{"de.norm"}.}
#'   \item {\code{"eu.norm"}} {Usual European cases: Only names and sentence beginnings start with an uppercase letter,
#'      anything else with a lowercase letter.}
#'   \item {\code{"eu.inv"}} {Inversion of \code{"eu.norm"}.}
#'  }
#' @param p Numeric value between 0 and 1. Defines the probability for upper case letters (relevant only
#'    if \code{scheme="random"}).
#' @param paste Logical, see value section.
#' @param ... Only used for the method generic.
#' @return By default an object of class \code{\link[koRpus:kRp.txt.trans-class]{kRp.txt.trans}} is returned. If \code{paste=TRUE}, returns
#'    an atomic character vector (via \code{\link[koRpus:pasteText]{pasteText}}).
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords misc
#' @import methods
#' @docType methods
#' @export
#' @rdname textTransform-methods
#' @examples
#' \dontrun{
#' tagged.text.obj <- freq.analysis("/some/text.txt", corp.freq=my.LCC.data)
#' textTransform(tagged.text.obj, scheme="random", paste=TRUE)
#' }
setGeneric("textTransform", function(txt, ...){standardGeneric("textTransform")})

#' @export
#' @docType methods
#' @rdname textTransform-methods
#' @aliases textTransform,kRp.taggedText-method
#' @include 01_class_01_kRp.tagged.R
#' @include 01_class_03_kRp.txt.freq.R
#' @include 01_class_04_kRp.txt.trans.R
#' @include 01_class_05_kRp.analysis.R
#' @include 01_class_80_kRp.taggedText_union.R
#' @include koRpus-internal.R
setMethod("textTransform",
  # "kRp.taggedText" is a ClassUnion defined in koRpus-internal.R
  signature(txt="kRp.taggedText"),
  function(txt, scheme, p=0.5, paste=FALSE){

    txt.df <- txt.orig <- taggedText(txt)

    if(identical(scheme, "minor")){
      # change first letter to lower case
      txt.df[["token"]] <- text.1st.letter(txt.df[["token"]], "lower")
    } else if(identical(scheme, "all.minor")){
      # change all to lower case
      txt.df[["token"]] <- tolower(txt.df[["token"]])
    } else if(identical(scheme, "major")){
      # change first letter to upper case
      txt.df[["token"]] <- text.1st.letter(txt.df[["token"]], "upper")
    } else if(identical(scheme, "all.major")){
      # change all to upper case
      txt.df[["token"]] <- toupper(txt.df[["token"]])
    } else if(identical(scheme, "random")){
      # randomly begin with upper or lower case letters
      # p defines the probability of upper case
      num.words <- nrow(txt.df)
      num.upper <- round(num.words * p)
      upper.select <- 1:num.words %in% sample(1:num.words, num.upper)
      txt.df[upper.select,"token"] <- text.1st.letter(txt.df[upper.select,"token"], "upper")
      txt.df[!upper.select,"token"] <- text.1st.letter(txt.df[!upper.select,"token"], "lower")
    } else if(scheme %in% c("de.norm", "de.inv", "eu.norm", "eu.inv")){
      # beginning of sentences must begin in upper case
      # we'll define "beginning" as anything after a fullstop
      fullstop.classes <- kRp.POS.tags(language(txt), list.classes=TRUE, tags="sentc")
      sentc.ends <- txt.df[["wclass"]] %in% fullstop.classes
        # usually the semicolon indicates a new sentence, but you don't begin in uppercase afterwards
        sentc.ends[which(txt.df[["token"]] %in% ";")] <- FALSE
      sentc.begins <- c(TRUE, sentc.ends[-length(sentc.ends)])
        # we must correct for cases where the sentence starts with punctuation, like double quotes
        punctuation.classes <- kRp.POS.tags(language(txt), list.classes=TRUE, tags="punct")
        sntc.punct <- txt.df[["wclass"]] %in% punctuation.classes
        # which cases need to be taken care of?
        start.with.punct <- which(sntc.punct & sentc.begins)
        for (txt.index in start.with.punct){
          # move the TRUE one up
          sentc.begins[txt.index] <- FALSE
          if(txt.index < length(sentc.begins)){
            sentc.begins[txt.index + 1] <- TRUE
          } else {}
        }
      if(scheme %in% c("de.norm", "de.inv")){
        # find nouns an names
        nouns <- txt.df[["wclass"]] %in% c("noun", "name")
      } else {
        # find proper nouns
        nouns <- txt.df[["wclass"]] %in% "name"
      }
      all.to.upper <- nouns | sentc.begins
      if(scheme %in% c("de.norm", "eu.norm")){
        # write all nouns, names and sentence beginnings starting with upper case
        txt.df[all.to.upper,"token"] <- text.1st.letter(txt.df[all.to.upper,"token"], "upper")
        txt.df[!all.to.upper,"token"] <- text.1st.letter(txt.df[!all.to.upper,"token"], "lower")
      } else {
        # full inversion of "de.norm"
        txt.df[!all.to.upper,"token"] <- text.1st.letter(txt.df[!all.to.upper,"token"], "upper")
        txt.df[all.to.upper,"token"] <- text.1st.letter(txt.df[all.to.upper,"token"], "lower")
      }
    } else {
      stop(simpleError("Unknown scheme specified!"))
    }

    if(isTRUE(paste)){
      results <- pasteText(txt.df)
    } else {
      results <- txt_trans_diff(obj=txt, TT.res.new=txt.df, transfmt=scheme)
    }

    return(results)
  }
)


#' Deprecated functions
#' 
#' These functions will be removed soon and should no longer ne used.
#' @rdname koRpus-deprecated
#' @name koRpus-deprecated
#' @export
kRp.text.transform <- function(...){
  .Deprecated(new="textTransform")
  textTransform(...)
}


## function txt_trans_diff()
# helper function to calculate the diff data and combine results in
# proper kRp.txt.trans object
# - obj: tagged text object (class kRp.taggedText)
# - TT.res.new: the transformed TT.res data frame
# returns an object of kRp.txt.trans
#' @include 01_class_04_kRp.txt.trans.R
txt_trans_diff <- function(obj, TT.res.new, transfmt="unknown"){
  TT.res.orig <- old.new.comp <- taggedText(obj)
  lang <- language(obj)

  no_punct <- tagged.txt.rm.classes(TT.res.orig, lang=lang, corp.rm.class="nonpunct", corp.rm.tag=c(), boolean=TRUE)
  all_letters <- TT.res.orig[["lttr"]]

  # keep an already present "token.orig" if present
  tokens.orig <- txt_trans_revert_orig(TT.res=TT.res.orig)[["token"]]
  tokens.trans     <- TT.res.new[["token"]]

  tokens.equal     <- tokens.orig == tokens.trans
  letters.diff     <- rep(0, length(tokens.equal))
  letters.diff[!tokens.equal] <- sapply(
    # only do this for tokens which are actually different from the originalText
    which(!tokens.equal),
    function(thisToken){
      letters.orig <- unlist(strsplit(tokens.orig[thisToken], ""))
      letters.trans <- unlist(strsplit(tokens.trans[thisToken], ""))
      # transformations like clozeDelete might change the number of
      # characters, so for a safe comparison, we'll discard all
      # additional characters of the longer token
      relevant_length <- min(length(letters.orig), length(letters.trans))
      result <- sum(letters.orig[1:relevant_length] != letters.trans[1:relevant_length])
      return(result)
    }
  )

  tokens.orig.np   <- tokens.orig[no_punct]
  tokens.trans.np  <- tokens.trans[no_punct]
  tokens.equal.np  <- tokens.orig.np == tokens.trans.np
  letters.diff.np  <- letters.diff[no_punct]
  all_letters.np   <- all_letters[no_punct]

  diff.pct.words.all <- 100 * sum(!tokens.equal) / length(tokens.equal)
  diff.pct.words     <- 100 * sum(!tokens.equal.np) / length(tokens.equal.np)
  diff.pct.lett.all  <- 100 * sum(letters.diff) / sum(all_letters)
  diff.pct.lett      <- 100 * sum(letters.diff.np) / sum(all_letters.np)

  old.new.comp[["token"]] <- tokens.trans
  old.new.comp[!tokens.equal,"token.orig"] <- tokens.orig[!tokens.equal]
  old.new.comp[["equal"]] <- tokens.equal
  old.new.comp[["lttr.diff"]] <- letters.diff

  if(inherits(obj, "kRp.txt.trans")){
    transfmt <- c(diffText(obj)[["transfmt"]], transfmt)
  } else {}

  results <- kRp_txt_trans(
    lang=lang,
    TT.res=old.new.comp,
    diff=list(
      all.tokens=diff.pct.words.all,
      words=diff.pct.words,
      all.chars=diff.pct.lett.all,
      letters=diff.pct.lett,
      transfmt=transfmt
    )
  )
  
  return(results)
} ## end function kRp_txt_trans_diff()


## function txt_trans_revert_orig()
# checks a TT.res object for previous tranformations and returns the original tokens
txt_trans_revert_orig <- function(TT.res){
  cols <- colnames(TT.res)
  if(all(c("equal", "token.orig") %in% cols)){
    TT.res[!TT.res[["equal"]],"token"] <- TT.res[!TT.res[["equal"]],"token.orig"]
    return(TT.res[,cols[!cols %in% c("token.orig","equal","lttr.diff")]])
  } else {
    return(TT.res)
  }
} ## end function txt_trans_revert_orig()
