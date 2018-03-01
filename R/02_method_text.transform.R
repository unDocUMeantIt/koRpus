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


#' Letter case transformation
#' 
#' Transforms text in koRpus objects token by token.
#'
#' This function is mainly intended to produce text material for experiments.
#'
#' @param txt An object of class \code{\link[koRpus]{kRp.txt.trans-class}}, \code{\link[koRpus]{kRp.tagged-class}},
#'    \code{\link[koRpus]{kRp.txt.freq-class}} or \code{\link[koRpus]{kRp.analysis-class}}.
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
#' @return By default an object of class \code{\link[koRpus]{kRp.txt.trans-class}} is returned. If \code{paste=TRUE}, returns
#'    an atomic character vector (via \code{\link[koRpus:kRp.text.paste]{kRp.text.paste}}).
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
setGeneric("textTransform", function(txt, scheme, p=0.5, paste=FALSE){standardGeneric("textTransform")})

#' @export
#' @docType methods
#' @rdname textTransform-methods
#' @aliases textTransform,kRp.taggedText-method
#' @include 01_class_01_kRp.tagged.R
#' @include 01_class_03_kRp.txt.freq.R
#' @include 01_class_04_kRp.txt.trans.R
#' @include 01_class_05_kRp.analysis.R
#' @include koRpus-internal.R
setMethod("textTransform",
  # "kRp.taggedText" is a ClassUnion defined in koRpus-internal.R
  signature(txt="kRp.taggedText"),
  function(txt, scheme, p=0.5, paste=FALSE){

    # get class kRp.tagged from txt object
    # the internal function tag.kRp.txt() will return the object unchanged if it
    # is already tagged
    txt.orig <- tag.kRp.txt(txt, objects.only=TRUE)

    # local copy for alterations
    txt <- txt.orig

    if(identical(scheme, "minor")){
      # change first letter to lower case
      txt[["token"]] <- text.1st.letter(txt[["token"]], "lower")
    } else if(identical(scheme, "all.minor")){
      # change all to lower case
      txt[["token"]] <- tolower(txt[["token"]])
    } else if(identical(scheme, "major")){
      # change first letter to upper case
      txt[["token"]] <- text.1st.letter(txt[["token"]], "upper")
    } else if(identical(scheme, "all.major")){
      # change all to upper case
      txt[["token"]] <- toupper(txt[["token"]])
    } else if(identical(scheme, "random")){
      # randomly begin with upper or lower case letters
      # p defines the probability of upper case
      num.words <- nrow(taggedText(txt))
      num.upper <- round(num.words * p)
      upper.select <- 1:num.words %in% sample(1:num.words, num.upper)
      txt[upper.select,"token"] <- text.1st.letter(txt[upper.select,"token"], "upper")
      txt[!upper.select,"token"] <- text.1st.letter(txt[!upper.select,"token"], "lower")
    } else if(scheme %in% c("de.norm", "de.inv", "eu.norm", "eu.inv")){
      # beginning of sentences must begin in upper case
      # we'll define "beginning" as anything after a fullstop
      fullstop.classes <- kRp.POS.tags(language(txt), list.classes=TRUE, tags="sentc")
      sentc.ends <- txt[["wclass"]] %in% fullstop.classes
        # usually the semicolon indicates a new sentence, but you don't begin in uppercase afterwards
        sentc.ends[which(txt[["token"]] %in% ";")] <- FALSE
      sentc.begins <- c(TRUE, sentc.ends[-length(sentc.ends)])
        # we must correct for cases where the sentence starts with punctuation, like double quotes
        punctuation.classes <- kRp.POS.tags(language(txt), list.classes=TRUE, tags="punct")
        sntc.punct <- txt[["wclass"]] %in% punctuation.classes
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
        nouns <- txt[["wclass"]] %in% c("noun", "name")
      } else {
        # find proper nouns
        nouns <- txt[["wclass"]] %in% "name"
      }
      all.to.upper <- nouns | sentc.begins
      if(scheme %in% c("de.norm", "eu.norm")){
        # write all nouns, names and sentence beginnings starting with upper case
        txt[all.to.upper,"token"] <- text.1st.letter(txt[all.to.upper,"token"], "upper")
        txt[!all.to.upper,"token"] <- text.1st.letter(txt[!all.to.upper,"token"], "lower")
      } else {
        # full inversion of "de.norm"
        txt[!all.to.upper,"token"] <- text.1st.letter(txt[!all.to.upper,"token"], "upper")
        txt[all.to.upper,"token"] <- text.1st.letter(txt[all.to.upper,"token"], "lower")
      }
    } else {
      stop(simpleError("Unknown scheme specified!"))
    }

    if(isTRUE(paste)){
      results <- kRp.text.paste(txt)
    } else {
        tokens.orig      <- txt.orig[["token"]]
        tokens.orig.np   <- tagged.txt.rm.classes(taggedText(txt.orig), lang=txt.orig@lang, corp.rm.class="nonpunct", corp.rm.tag=c())
        tokens.trans     <- txt[["token"]]
        tokens.trans.np  <- tagged.txt.rm.classes(taggedText(txt), lang=language(txt), corp.rm.class="nonpunct", corp.rm.tag=c())

        letters.orig     <- unlist(strsplit(tokens.orig, ""))
        letters.orig.np  <- unlist(strsplit(tokens.orig.np, ""))
        letters.trans    <- unlist(strsplit(tokens.trans, ""))
        letters.trans.np <- unlist(strsplit(tokens.trans.np, ""))

        tokens.equal     <- tokens.orig == tokens.trans
        tokens.equal.np  <- tokens.orig.np == tokens.trans.np
        letters.equal    <- letters.orig == letters.trans
        letters.equal.np <- letters.orig.np == letters.trans.np

        diff.pct.words.all <- 100 * sum(!tokens.equal) / length(tokens.equal)
        diff.pct.words     <- 100 * sum(!tokens.equal.np) / length(tokens.equal.np)
        diff.pct.lett.all  <- 100 * sum(!letters.equal) / length(letters.equal)
        diff.pct.lett      <- 100 * sum(!letters.equal.np) / length(letters.equal.np)

        old.new.comp <- data.frame(
                    token=tokens.trans,
                    tag=txt[["tag"]],
                    lemma=txt[["lemma"]],
                    lttr=txt[["lttr"]],
                    wclass=txt[["wclass"]],
                    desc=txt[["desc"]],
                    stop=txt[["stop"]],
                    stem=txt[["stem"]],
                    token.orig=tokens.orig,
                    equal=tokens.equal,
                    stringsAsFactors=FALSE)
        results <- kRp_txt_trans(
                  lang=language(txt),
                  TT.res=old.new.comp,
                  diff=list(all.tokens=diff.pct.words.all, words=diff.pct.words, all.chars=diff.pct.lett.all, letters=diff.pct.lett))
        message(paste0("Difference between objects\n    Words: ", round(diff.pct.words, digits=2), "%\n  Letters: ", round(diff.pct.lett, digits=2),"%"))
    }

    return(results)
  }
)


#' Deprecated functions
#' 
#' These functions will be removed soon and should no longer ne used.
#' @rdname koRpus-deprecated
#' @name koRpus-deprecated
#' @param ... Parameters to be passed to the replacement of the function
#' @export
kRp.text.transform <- function(...){
  .Deprecated(new="textTransform")
  textTransform(...)
}
