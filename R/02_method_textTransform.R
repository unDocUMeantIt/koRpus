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
#' @section Function: You can dynamically calculate the replacement value for the \code{"normalize"} scheme by setting \code{method="function"} and
#' providing a function object as \code{f}. The function you provide must support the following arguments:
#' \itemize{
#'   \item {\code{tokens}} {The original tokens slot of the \code{txt} object (see \code{\link[koRpus:taggedText]{taggedText}}).}
#'   \item {\code{match}} {A logical vector, indicating for each row of \code{tokens} whether it's a query match or not.}
#' }
#' You can then use these arguments in your function body to calculate the replacement, e.g. \code{tokens[match,"token"]} to get all relevant tokens.
#' The return value of the function will be used as the replacement for all matched tokens. You probably want to make sure it's a character vecor
#' of length one or of the same length as all matches.
#'
#' @param txt An object of class \code{\link[koRpus:kRp.text-class]{kRp.text}}.
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
#'   \item {\code{"normalize"}} {Replace all tokens matching \code{query} in column \code{var} according to \code{method} (see below).}
#'  }
#' @param p Numeric value between 0 and 1. Defines the probability for upper case letters (relevant only
#'    if \code{scheme="random"}).
#' @param paste Logical, see value section.
#' @param var A character string naming a variable in the object (i.e., colname). See \code{\link[koRpus:query]{query}} for details.
#'    Relevant only if \code{scheme="normalize"}.
#' @param query A character vector (for words), regular expression, or single number naming values to be matched in the variable.
#'    See \code{\link[koRpus:query]{query}} for details. Relevant only if \code{scheme="normalize"}.
#' @param method One of the following character strings:
#'    \itemize{
#'      \item {\code{"shortest"}} {Replace all matches with the shortest value found.}
#'      \item {\code{"longest"}} {Replace all matches with the longest value found.}
#'      \item {\code{"replace"}} {Replace all matches with the token given via \code{replacement}.}
#'      \item {\code{"function"}} {Replace all matches with the result of the function provided by \code{f} (see section Function for details).}
#'    }
#'    In case of \code{"shortest"} and \code{"longest"}, if multiple values of the same length are found, the (first) most prevalent one is being used.
#'    The actual replacement value is documented in the \code{diff} slot of the object, as a list called \code{transfmt.normalize}.
#'    Relevant only if \code{scheme="normalize"}.
#' @param replacement Character string defining the exact token to replace all query matches with.
#'    Relevant only if \code{scheme="normalize"} and \code{method="replace"}.
#' @param f A function to calculate the replacement for all query matches.
#'    Relevant only if \code{scheme="normalize"} and \code{method="function"}.
#' @param ... Parameters passed to \code{\link[koRpus:query]{query}} to find matching tokens. Relevant only if \code{scheme="normalize"}.
#' @return By default an object of class \code{\link[koRpus:kRp.text-class]{kRp.text}} with the added feature \code{diff} is returned.
#'    If \code{paste=TRUE}, returns an atomic character vector (via \code{\link[koRpus:pasteText]{pasteText}}).
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords misc
#' @import methods
#' @docType methods
#' @export
#' @rdname textTransform-methods
#' @examples
#' \dontrun{
#' tagged.text.obj <- freq.analysis(
#'   tagged.text.obj,
#'   corp.freq=my.LCC.data,
#'   as.feature=TRUE
#' )
#' textTransform(tagged.text.obj, scheme="random", paste=TRUE)
#' }
setGeneric("textTransform", function(txt, ...){standardGeneric("textTransform")})

#' @export
#' @docType methods
#' @rdname textTransform-methods
#' @aliases textTransform,kRp.text-method
#' @include 01_class_01_kRp.text.R
#' @include koRpus-internal.R
setMethod("textTransform",
  signature(txt="kRp.text"),
  function(txt, scheme, p=0.5, paste=FALSE, var="wclass", query="fullstop", method="replace", replacement=".", f=NA, ...){

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
    } else if(identical(scheme, "normalize")){
      matched_tokens_idx <- query(txt.df, var=var, query=query, ...)[["idx"]]
      relevant_tokens <- txt.df[["idx"]] %in% matched_tokens_idx
      if(identical(method, "replace")){
        txt.df[relevant_tokens, "token"] <- replacement
      } else if(method %in% c("shortest", "longest", "function")){
        matched_tokens <- unique(txt.df[relevant_tokens, c("token","lttr")])
        num_tokens <- table(txt.df[relevant_tokens, "token"])
        matched_tokens[["num"]] <- num_tokens[matched_tokens[["token"]]]
        if(identical(method, "shortest")){
          shortest_matches <- matched_tokens[matched_tokens[["lttr"]] %in% min(matched_tokens[["lttr"]]),]
          # select the most prevalent token of all remaining matches
          txt.df[relevant_tokens, "token"] <- replacement <- shortest_matches[match(max(shortest_matches[["num"]]), shortest_matches[["num"]]), "token"]
        } else if(identical(method, "longest")){
          longest_matches <- matched_tokens[matched_tokens[["lttr"]] %in% max(matched_tokens[["lttr"]]),]
          txt.df[relevant_tokens, "token"] <- replacement <- longest_matches[match(max(longest_matches[["num"]]), longest_matches[["num"]]), "token"]
        } else if(identical(method, "function")){
          if(is.function(f)){
            txt.df[relevant_tokens, "token"] <- replacement <- f(tokens=txt.df, match=relevant_tokens)
          } else {
            stop(simpleError("\"f\" must be a function!"))
          }
        }
      } else {
        stop(simpleError("Unknown normalization method specified!"))
      }
    } else {
      stop(simpleError("Unknown scheme specified!"))
    }

    if(isTRUE(paste)){
      results <- pasteText(txt.df)
    } else {
      results <- txt_trans_diff(
        obj=txt,
        tokens.new=txt.df,
        transfmt=scheme,
        normalize=list(
          method=method,
          var=var,
          query=query,
          replacement=replacement,
          f=f,
          ...
        ),
        check_missing_letters=scheme %in% "normalize"
      )
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
# proper kRp.text object with added feature "diff"
# - obj: tagged text object (class kRp.text)
# - tokens.new: the transformed tokens data frame
# - transfmt: the name of the transformation
# - normalize: arguments given for the normalization
# - check_missing_letters: transformations like "normalize" can replace tokens
#     with shorter ones. this option adds missing letters as changes to the original tokens
# returns an object of kRp.text
txt_trans_diff <- function(obj, tokens.new, transfmt="unknown", normalize=list(), check_missing_letters=FALSE){
  lang <- language(obj)
  doc_id <- describe(obj)[["doc_id"]]
  old.new.comp <- taggedText(obj)
  obj_has_diff <- hasFeature(obj, "diff")
  if(obj_has_diff){
    diffObj        <- diffText(obj)
  } else {}

  no_punct <- tagged.txt.rm.classes(old.new.comp, lang=lang, corp.rm.class="nonpunct", corp.rm.tag=c(), boolean=TRUE)
  all_letters <- old.new.comp[["lttr"]]

  # keep an already present "token.orig" if present
  tokens.orig      <- txt_trans_revert_orig(tokens=old.new.comp)[["token"]]
  tokens.trans     <- tokens.new[["token"]]

  tokens.equal     <- tokens.orig == tokens.trans
  # the above shows the global differences between original and current tokens
  # if there was more than one transformation to this object, we also want to
  # document the tokens that have changed only in this transformational step
  if(obj_has_diff){
    tokens.last    <- old.new.comp[["token"]]
    trans.equal    <- tokens.last == tokens.trans
    transfmt.normalize <- diffObj[["transfmt.normalize"]]
    if(is.data.frame(diffObj[["transfmt.equal"]])){
      transfmt.equal <- cbind(diffObj[["transfmt.equal"]], trans.equal)
    } else {
      # initialize the data.frame
      transfmt.equal <- data.frame(tokens.equal, trans.equal)
    }
    colnames(transfmt.equal) <- c(diffObj[["transfmt"]], transfmt)
  } else {
    transfmt.normalize <- list()
    # skip this if this is the only transformation, because it would be redundant
    transfmt.equal <- NULL
  }
  if("normalize" %in% transfmt){
    transfmt.normalize[[length(transfmt.normalize) + 1]] <- normalize
  } else {}

  letters.diff     <- rep(0, length(tokens.equal))
  letters.diff[!tokens.equal] <- sapply(
    # only do this for tokens which are actually different from the originalText
    which(!tokens.equal),
    function(thisToken){
      letters.orig  <- unlist(strsplit(tokens.orig[thisToken], ""))
      letters.trans <- unlist(strsplit(tokens.trans[thisToken], ""))
      # transformations like clozeDelete might change the number of
      # characters, so for a safe comparison, we'll discard all
      # additional characters of the longer token
      relevant_length <- min(length(letters.orig), length(letters.trans))
      result <- sum(letters.orig[1:relevant_length] != letters.trans[1:relevant_length])
      if(isTRUE(check_missing_letters)){
        # however, transformations lie "normalize" might replace tokens
        # with much shorter ones, so for a full comparison, let's add the
        # missing letters as changes
        result <- result + (max(length(letters.orig), length(letters.trans)) - relevant_length)
      } else {}
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

  if(obj_has_diff){
    transfmt <- c(diffObj[["transfmt"]], transfmt)
  } else {}

  taggedText(obj) <- old.new.comp
  diffText(obj) <- list(
      all.tokens=diff.pct.words.all,
      words=diff.pct.words,
      all.chars=diff.pct.lett.all,
      letters=diff.pct.lett,
      transfmt=transfmt,
      transfmt.equal=transfmt.equal,
      transfmt.normalize=transfmt.normalize
    )
  describe(obj) <- basic.tagged.descriptives(
    obj,
    lang=lang,
    txt.vector=old.new.comp[["token"]],
    doc_id=doc_id
  )

  return(obj)
} ## end function txt_trans_diff()


## function txt_trans_revert_orig()
# checks a tokens object for previous tranformations and returns the original tokens
txt_trans_revert_orig <- function(tokens){
  cols <- colnames(tokens)
  if(all(c("equal", "token.orig") %in% cols)){
    tokens[!tokens[["equal"]],"token"] <- tokens[!tokens[["equal"]],"token.orig"]
    return(tokens[,cols[!cols %in% c("token.orig","equal","lttr.diff")]])
  } else {
    return(tokens)
  }
} ## end function txt_trans_revert_orig()
