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

#' S4 Class kRp.corp.freq
#'
#' This class is used for objects that are returned by \code{\link[koRpus:read.corp.LCC]{read.corp.LCC}} and \code{\link[koRpus:read.corp.celex]{read.corp.celex}}.
#'
#' The slot \code{meta} simply contains all information from the "meta.txt" of the LCC[1] data and remains
#' empty for data from a Celex[2] DB.
#'
#' @slot meta Metadata on the corpora (see details).
#' @slot words Absolute word frequencies. It has at least the following columns:
#'    \describe{
#'      \item{\code{num}:}{Some word ID from the DB, integer}
#'      \item{\code{word}:}{The word itself}
#'      \item{\code{lemma}:}{The lemma of the word}
#'      \item{\code{tag}:}{A part-of-speech tag}
#'      \item{\code{wclass}:}{The word class}
#'      \item{\code{lttr}:}{The number of characters}
#'      \item{\code{freq}:}{The frequency of that word in the corpus DB}
#'      \item{\code{pct}:}{Percentage of appearance in DB}
#'      \item{\code{pmio}:}{Appearance per million words in DB}
#'      \item{\code{log10}:}{Base 10 logarithm of word frequency}
#'      \item{\code{rank.avg}:}{Rank in corpus data, \code{\link{rank}} ties method "average"}
#'      \item{\code{rank.min}:}{Rank in corpus data, \code{\link{rank}} ties method "min"}
#'      \item{\code{rank.rel.avg}:}{Relative rank, i.e. percentile of \code{"rank.avg"}}
#'      \item{\code{rank.rel.min}:}{Relative rank, i.e. percentile of \code{"rank.min"}}
#'      \item{\code{inDocs}:}{The absolute number of documents in the corpus containing the word}
#'      \item{\code{idf}:}{The inverse document frequency}
#'    }
#'    The slot might have additional columns, depending on the input material.
#' @slot desc Descriptive information. It contains six numbers from the \code{meta} information, for convenient accessibility:
#'    \describe{
#'      \item{\code{tokens}:}{Number of running word forms}
#'      \item{\code{types}:}{Number of distinct word forms}
#'      \item{\code{words.p.sntc}:}{Average sentence length in words}
#'      \item{\code{chars.p.sntc}:}{Average sentence length in characters}
#'      \item{\code{chars.p.wform}:}{Average word form length}
#'      \item{\code{chars.p.word}:}{Average running word length}
#'    }
#'    The slot might have additional columns, depending on the input material.
#' @slot bigrams A data.frame listing all tokens that co-occurred next to each other in the corpus:
#'    \describe{
#'      \item{\code{token1}:}{The first token}
#'      \item{\code{token2}:}{The second token that appeared right next to the first}
#'      \item{\code{freq}:}{How often the co-occurrance was present}
#'      \item{\code{sig}:}{Log-likelihood significance of the co-occurrende}
#'    }
#' @slot cooccur Similar to \code{bigrams}, but listing co-occurrences anywhere in one sentence:
#'    \describe{
#'      \item{\code{token1}:}{The first token}
#'      \item{\code{token2}:}{The second token that appeared in the same sentence}
#'      \item{\code{freq}:}{How often the co-occurrance was present}
#'      \item{\code{sig}:}{Log-likelihood significance of the co-occurrende}
#'    }
#' @slot caseSens A single logical value, whether the frequency statistics were calculated case sensitive
#'    or not.
#' @name kRp.corp.freq,-class
#' @aliases kRp.corp.freq,-class kRp.corp.freq-class
#' @import methods
#' @keywords classes
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @references
#' [1] \url{http://corpora.informatik.uni-leipzig.de/download.html}
#' [2] \url{http://celex.mpi.nl}
#' @export kRp_corp_freq
#' @exportClass kRp.corp.freq
#' @rdname kRp.corp.freq-class

kRp_corp_freq <- setClass("kRp.corp.freq",
    representation=representation(
      meta="data.frame",
      words="data.frame",
      desc="data.frame",
      bigrams="data.frame",
      cooccur="data.frame",
      caseSens="logical"
    ),
    prototype(
      meta=data.frame(
          meta=NA,
          value=NA
        ),
      words=data.frame(
          num=NA,
          word=NA,
          lemma=NA,
          tag=NA,
          wclass=NA,
          lttr=NA,
          freq=NA,
          pct=NA,
          pmio=NA,
          log10=NA,
          rank.avg=NA,
          rank.min=NA,
          rank.rel.avg=NA,
          rank.rel.min=NA,
          inDocs=NA,
          idf=NA
        ),
      desc=data.frame(
          tokens=NA,
          types=NA,
          words.p.sntc=NA,
          chars.p.sntc=NA,
          chars.p.wform=NA,
          chars.p.word=NA
        ),
      bigrams=data.frame(
          token1=NA,
          token2=NA,
          freq=NA,
          sig=NA
        ),
      cooccur=data.frame(
          token1=NA,
          token2=NA,
          freq=NA,
          sig=NA
        ),
      caseSens=FALSE
    )
)

setValidity("kRp.corp.freq", function(object){
    meta <- slot(object, "meta")
    words <- slot(object, "words")
    desc <- slot(object, "desc")
    bigrams <- slot(object, "bigrams")
    cooccur <- slot(object, "cooccur")

    meta.names <- dimnames(meta)[[2]]
    words.names <- dimnames(words)[[2]]
    desc.names <- dimnames(desc)[[2]]
    bigrams.names <- dimnames(bigrams)[[2]]
    cooccur.names <- dimnames(cooccur)[[2]]

  if(!identical(meta.names, c("meta", "value"))){
    stop(simpleError("Invalid object: Wrong column names in slot \"meta\"."))
  } else {}
  if(!identical(words.names, c(
    "num", "word", "lemma", "tag", "wclass", "lttr", "freq", "pct", "pmio", "log10",
    "rank.avg", "rank.min", "rank.rel.avg", "rank.rel.min", "inDocs", "idf"))){
    stop(simpleError("Invalid object: Wrong column names in slot \"words\"."))
  } else {}
  if(!identical(desc.names, c("tokens", "types", "words.p.sntc", "chars.p.sntc", "chars.p.wform", "chars.p.word"))){
    stop(simpleError("Invalid object: Wrong column names in slot \"desc\"."))
  } else {}
  if(!identical(bigrams.names, c("token1", "token2", "freq", "sig"))){
    stop(simpleError("Invalid object: Wrong column names in slot \"bigrams\"."))
  } else {}
  if(!identical(cooccur.names, c("token1", "token2", "freq", "sig"))){
    stop(simpleError("Invalid object: Wrong column names in slot \"cooccur\"."))
  } else {}
  
  return(TRUE)
})
