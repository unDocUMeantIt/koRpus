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

# these internal functions do the real corpus import,
# so they're mostly called by read.corp.custom()

##################################################################
## if this signature changes, check read.corp.custom() as well! ##
##################################################################

## function read_corp_custom_calc()
# this is the *actual* helper function that is called by methods
# - corpus: a taggedText object
# - dtm: a document term matrix from 'corpus'
# - caseSens: logical defining case sensitivity, must match 'dtm'
# - log.base: base for the logarithm
read_corp_custom_calc <- function(
  corpus,
  dtm,
  caseSens=TRUE,
  log.base=10
){
  dtm <- as.matrix(dtm)
  # sort matrix
  dtm <- dtm[, order(colnames(dtm)), drop=FALSE]
  term_freq_global <- colSums(dtm)
  dtm <- dtm[, order(term_freq_global, decreasing=TRUE), drop=FALSE]
  all_tokens <- colnames(dtm)
  in_docs <- colSums(dtm > 0)

  # add tag, lemma and wclass
  extra_cols <- unique(taggedText(corpus)[,c("token","tag", "lemma", "wclass"), drop=FALSE])
  if(!isTRUE(caseSens)){
    extra_cols[["token"]] <- unique(tolower(extra_cols[["token"]]))
  } else {}
  if(any(!extra_cols[["token"]] %in% all_tokens, !all_tokens %in% extra_cols[["token"]])){
    stop(simpleError("The tokens in the text corpus do not match the document term matrix -- messed up case sensitivity?"))
  } else {}
  # only leave one entry per token, take the one with the most frequent wclass
  # it's a bit bogus, since identical tokens can be of different meaning, but
  # there's not so much we can do about it here
  duplicated_tokens <- duplicated(extra_cols[["token"]])
  if(any(duplicated_tokens)){
    all_token_wclass <- table(taggedText(corpus)[,c("token", "wclass"), drop=FALSE])
    unclear_tokens <- unique(extra_cols[duplicated_tokens, "token"])
    token_index <- sapply(
      unclear_tokens,
      function(this_token){
        # the most frequent word class for this token
        tic_max_wclass <- names(which.max(all_token_wclass[this_token, ]))
        # return the first row where both token and wclass match
        return(which(extra_cols[["token"]] %in% this_token & extra_cols[["wclass"]] %in% tic_max_wclass)[1])
      }
    )
    keep_rows <- !extra_cols[["token"]] %in% unclear_tokens
    keep_rows[token_index] <- TRUE
    extra_cols <- extra_cols[keep_rows,]
  } else {}
  token_order <- match(all_tokens, extra_cols[["token"]])

  token_df <- data.frame(
    num=1:ncol(dtm),
    word=all_tokens,
    tag=extra_cols[token_order, "tag"], #rep("", ncol(dtm)), 
    lemma=extra_cols[token_order, "lemma"], #rep("", ncol(dtm)), # 
    wclass=extra_cols[token_order, "wclass"], # rep("", ncol(dtm)), #
    freq=colSums(dtm),
    inDocs=in_docs,
    idf=log(nrow(dtm) / in_docs, base=log.base),
    row.names=1:ncol(dtm),
    stringsAsFactors=FALSE
  )

  # descriptive statistics
  dscrpt.meta <- data.frame(
    tokens=nrow(taggedText(corpus)),
    types=ncol(dtm),
    words.p.sntc=NA,
    chars.p.sntc=NA,
    chars.p.wform=NA,
    chars.p.word=NA
  )

  result <- create.corp.freq.object(
    matrix.freq=token_df,
    num.running.words=dscrpt.meta[["tokens"]],
    df.meta=data.frame(meta=character(), value=character()),
    df.dscrpt.meta=dscrpt.meta
  )

  return(result)
} ## end function read_corp_custom_calc()
