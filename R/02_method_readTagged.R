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


#' Import already tagged texts
#'
#' This method can be used on text files or matrices containing already tagged text material, e.g. the results of
#' TreeTagger[1].
#'
#' Note that the value of \code{lang} must match a valid language supported by \code{\link[koRpus:kRp.POS.tags]{kRp.POS.tags}}.
#' It will also get stored in the resulting object and might be used by other functions at a later point.
#'
#' @param file Either a matrix, a connection or a character vector. If the latter, that must be a valid path to a file,
#'    containing the previously analyzed text. If it is a matrix, it must contain three columns named "token", "tag", and "lemma",
#'    and except for these three columns all others are ignored.
#' @param lang A character string naming the language of the analyzed corpus. See \code{\link[koRpus:kRp.POS.tags]{kRp.POS.tags}}
#'    for all supported languages.
#'    If set to \code{"kRp.env"} this is got from \code{\link[koRpus:get.kRp.env]{get.kRp.env}}.
#' @param encoding A character string defining the character encoding of the input file, like  \code{"Latin1"} or \code{"UTF-8"}.
#' @param tagger The software which was used to tokenize and tag the text. Currently, "TreeTagger" and "manual" are the only
#'    supported values. If "manual", you must also adjust the values of \code{mtx_cols} to define the columns to be imported.
#' @param apply.sentc.end Logical, whethter the tokens defined in \code{sentc.end} should be searched and set to a sentence ending tag.
#'    You could call this a compatibility mode to make sure you get the results you would get if you called
#'    \code{\link[koRpus:treetag]{treetag}} on the original file.
#'    If set to \code{FALSE}, the tags will be imported as they are.
#' @param sentc.end A character vector with tokens indicating a sentence ending. This adds to given results, it doesn't replace them.
#' @param stopwords A character vector to be used for stopword detection. Comparison is done in lower case. You can also simply set 
#'    \code{stopwords=tm::stopwords("en")} to use the english stopwords provided by the \code{tm} package.
#' @param stemmer A function or method to perform stemming. For instance, you can set \code{stemmer=Snowball::SnowballStemmer} if you
#'    have the \code{Snowball} package installed (or \code{SnowballC::wordStem}). As of now, you cannot provide further arguments to
#'    this function.
#' @param rm.sgml Logical, whether SGML tags should be ignored and removed from output.
#' @param doc_id Character string, optional identifier of the particular document. Will be added to the \code{desc} slot.
#' @param add.desc Logical. If \code{TRUE}, the tag description (column \code{"desc"} of the data.frame) will be added directly
#'    to the resulting object. If set to \code{"kRp.env"} this is fetched from \code{\link[koRpus:get.kRp.env]{get.kRp.env}}. Only needed if \code{tag=TRUE}.
#' @param ... Additional options, currently unused.
#' @return An object of class \code{\link[koRpus:kRp.tagged-class]{kRp.tagged}}. If \code{debug=TRUE}, prints internal variable settings and
#'    attempts to return the original output if the TreeTagger system call in a matrix.
#' @keywords misc
#' @seealso \code{\link[koRpus:treetag]{treetag}},
#'    \code{\link[koRpus:freq.analysis]{freq.analysis}},
#'    \code{\link[koRpus:get.kRp.env]{get.kRp.env}},
#'    \code{\link[koRpus:kRp.tagged-class]{kRp.tagged}}
#' @references
#' Schmid, H. (1994). Probabilistic part-of-speec tagging using decision trees. In
#'    \emph{International Conference on New Methods in Language Processing}, Manchester, UK, 44--49.
#'
#' [1] \url{http://www.ims.uni-stuttgart.de/projekte/corplex/TreeTagger/DecisionTreeTagger.html}
#' @export
#' @import methods
#' @docType methods
#' @rdname readTagged-methods
#' @examples
#' \dontrun{
#'   # call method on a connection
#'   text_con <- file("~/my.data/tagged_speech.txt", "r")
#'   tagged_results <- readTagged(text_con, lang="en")
#'   close(text_con)
#'
#'   # call it on the file directly
#'   tagged_results <- readTagged("~/my.data/tagged_speech.txt", lang="en")
#'   
#'   # import the results of RDRPOSTagger, using the "manual" tagger feature
#'   sample_text <- c("Dies ist ein kurzes Beispiel. Es ergibt wenig Sinn.")
#'   tagger <- RDRPOSTagger::rdr_model(language="German", annotation="POS")
#'   tagged_rdr <- RDRPOSTagger::rdr_pos(tagger, x=sample_text)
#'   tagged_results <- readTagged(
#'     tagged_rdr,
#'     lang="de",
#'     tagger="manual",
#'     mtx_cols=c(token="token", tag="pos", lemma=NA)
#'   )
#' }
setGeneric("readTagged", function(file, ...){standardGeneric("readTagged")})


#' @param mtx_cols Character vector with exactly three elements named "token", "tag", and "lemma",
#'    the values of which must match the respective column names of the matrix provided via \code{file}.
#'    It is possible to set \code{lemma=NA} if the tagged results only provide token and tag.
#'    This argument is ignored unless \code{tagger="manual"} and data is provided as either a matrix or data frame.
#' @export
#' @docType methods
#' @rdname readTagged-methods
#' @aliases readTagged,matrix-method
setMethod("readTagged",
  signature(file="matrix"),
  function(
    file,
    lang="kRp.env",
    tagger="TreeTagger",
    apply.sentc.end=TRUE,
    sentc.end=c(".","!","?",";",":"),
    stopwords=NULL,
    stemmer=NULL,
    rm.sgml=TRUE,
    doc_id=NA,
    add.desc="kRp.env",
    mtx_cols=c(token="token", tag="tag", lemma="lemma")
  ){
    results <- kRp_read_tagged(
      mtrx=file,
      lang=lang,
      tagger=tagger,
      apply.sentc.end=apply.sentc.end,
      sentc.end=sentc.end,
      stopwords=stopwords,
      stemmer=stemmer,
      doc_id=doc_id,
      add.desc=add.desc,
      mtx_cols=mtx_cols
    )
    return(results)
  }
)


#' @export
#' @docType methods
#' @rdname readTagged-methods
#' @aliases readTagged,data.frame-method
setMethod("readTagged",
  signature(file="data.frame"),
  function(
    file,
    lang="kRp.env",
    tagger="TreeTagger",
    apply.sentc.end=TRUE,
    sentc.end=c(".","!","?",";",":"),
    stopwords=NULL,
    stemmer=NULL,
    rm.sgml=TRUE,
    doc_id=NA,
    add.desc="kRp.env",
    mtx_cols=c(token="token", tag="tag", lemma="lemma")
  ){
    results <- readTagged(
      file=as.matrix(file),
      lang=lang,
      tagger=tagger,
      apply.sentc.end=apply.sentc.end,
      sentc.end=sentc.end,
      stopwords=stopwords,
      stemmer=stemmer,
      rm.sgml=rm.sgml,
      doc_id=doc_id,
      add.desc=add.desc,
      mtx_cols=mtx_cols
    )
    return(results)
  }
)


#' @export
#' @docType methods
#' @rdname readTagged-methods
#' @aliases readTagged,kRp.connection-method
#' @include 01_class_81_kRp.connection_union.R
setMethod("readTagged",
  signature(file="kRp.connection"),
  function(
    file,
    lang="kRp.env",
    encoding="unknown",
    tagger="TreeTagger",
    apply.sentc.end=TRUE,
    sentc.end=c(".","!","?",";",":"),
    stopwords=NULL,
    stemmer=NULL,
    rm.sgml=TRUE,
    doc_id=NA,
    add.desc="kRp.env"
  ){
    ## read the file
    tagged.text <- readLines(file, encoding=encoding)
    tagged.text <- enc2utf8(tagged.text)

    ## workaround
    # in seldom cases TreeTagger seems to return duplicate tab stops
    # we'll try to correct for that here
    tagged.text <- gsub("\t\t", "\t", tagged.text)
    
    if(isTRUE(rm.sgml)){
      tagged.text <- tagged.text[grep("^[^<]", tagged.text)]
    } else {}
    tagged.mtrx <- matrix(
      unlist(strsplit(tagged.text, "\t")),
      ncol=3,
      byrow=TRUE,
      dimnames=list(c(),c("token","tag","lemma"))
    )

    results <- readTagged(
      file=tagged.mtrx,
      lang=lang,
      tagger=tagger,
      apply.sentc.end=apply.sentc.end,
      sentc.end=sentc.end,
      stopwords=stopwords,
      stemmer=stemmer,
      doc_id=doc_id,
      add.desc=add.desc,
      mtx_cols=c(token="token", tag="tag", lemma="lemma")
    )

    return(results)
  }
)


#' @export
#' @docType methods
#' @rdname readTagged-methods
#' @aliases readTagged,character-method
setMethod("readTagged",
  signature(file="character"),
  function(
    file,
    lang="kRp.env",
    encoding=getOption("encoding"),
    tagger="TreeTagger",
    apply.sentc.end=TRUE,
    sentc.end=c(".","!","?",";",":"),
    stopwords=NULL,
    stemmer=NULL,
    rm.sgml=TRUE,
    doc_id=NA,
    add.desc="kRp.env"
  ){
    check.file(normalizePath(file), mode="exist")
    # turn into a connection and call another methods
    file_con <- file(description=file, open="r", encoding=encoding)
    results <- readTagged(
      file=file_con,
      lang=lang,
      encoding=encoding,
      tagger=tagger,
      apply.sentc.end=apply.sentc.end,
      sentc.end=sentc.end,
      stopwords=stopwords,
      stemmer=stemmer,
      rm.sgml=rm.sgml,
      doc_id=doc_id,
      add.desc=add.desc
    )
    close(file_con)
    return(results)
  }
)

#' @rdname koRpus-deprecated
#' @name koRpus-deprecated
#' @export
read.tagged <- function(...){
  .Deprecated(new="readTagged")
  readTagged(...)
}


# the internal workhorse
# mtrx: a matrix with columns "token", "tag", and "lemma"
kRp_read_tagged <- function(
  mtrx,
  lang="kRp.env",
  tagger="TreeTagger",
  apply.sentc.end=TRUE,
  sentc.end=c(".","!","?",";",":"),
  stopwords=NULL,
  stemmer=NULL,
  doc_id=NA,
  add.desc="kRp.env",
  mtx_cols=c(token="token", tag="tag", lemma="lemma")
){
  if(identical(lang, "kRp.env")){
    lang <- get.kRp.env(lang=TRUE)
  } else {}

  relv_cols <- get_cols <- c("token","tag","lemma")
  
  if(identical(tagger, "manual")){
    # check if we need to fix colnames first
    if(any(!relv_cols %in% names(mtx_cols))){
      stop(simpleError("If you want to use manual tagger settings, you must provide column names for \"token\", \"tag\", and \"lemma\"!"))
    } else {}
    lemma_na <- FALSE
    for (thisCol in relv_cols){
      if(is.na(mtx_cols[thisCol])){
        if(thisCol %in% "lemma"){
          # remove this NA column from relevant columns
          get_cols <- get_cols[!get_cols %in% thisCol]
          lemma_na <- TRUE
        } else {
          stop(simpleError(paste0("The value for column \"", thisCol, "\" must not be NA!")))
        }
      }
      get_cols[get_cols %in% thisCol] <- mtx_cols[thisCol]
    }
    if(any(!get_cols %in% colnames(mtrx))){
      stop(simpleError("The columns you defined via mtx_cols do not match the columns of the given matrix!"))
    } else {}
    tagged.mtrx <- as.matrix(mtrx[,get_cols])
    if(isTRUE(lemma_na)){
      colnames(tagged.mtrx) <- c("token","tag")
      tagged.mtrx <- cbind(tagged.mtrx, "lemma"=NA)
    } else {
      colnames(tagged.mtrx) <- relv_cols
    }
  } else {
    # mtrx should be a matrix-like object with previously tagged text
    if(any(!relv_cols %in% colnames(mtrx))){
      stop(simpleError("If input is a matrix, it must have three columns named \"token\", \"tag\", and \"lemma\"!"))
    } else {}
    tagged.mtrx <- as.matrix(mtrx[,relv_cols])
  }

  if(identical(tagger, "TreeTagger") | identical(tagger, "manual")){
    # add sentence endings as defined
    if(isTRUE(apply.sentc.end)){
      sntc.end.tag <- kRp.POS.tags(lang, tags="sentc", list.tags=TRUE)[[1]]
      matched.sentc.tokens <- tagged.mtrx[, "token"] %in% sentc.end
      tagged.mtrx[matched.sentc.tokens, "tag"] <- sntc.end.tag
    } else {}

    # add word classes, comments and numer of letters ("wclass", "desc", "lttr")
    tagged.mtrx <- treetag.com(tagged.mtrx, lang=lang, add.desc=add.desc)
  } else {
    ## additional taggers go here...
    stop(simpleError(paste0("Sorry, but tagger \"", tagger, "\" is not supported.")))
  }

  # probably apply stopword detection and stemming
  tagged.mtrx <- stopAndStem(tagged.mtrx, stopwords=stopwords, stemmer=stemmer, lowercase=TRUE)
  
  # add columns "idx", "sntc" and "doc_id"
  tagged.mtrx <- indexSentenceDoc(tagged.mtrx, lang=lang, doc_id=doc_id)

  results <- kRp_tagged(lang=lang, tokens=tagged.mtrx)
  ## descriptive statistics
  describe(results) <- basic.tagged.descriptives(
    results,
    lang=lang,
    txt.vector=paste.tokenized.text(tagged.mtrx[["token"]]),
    doc_id=doc_id
  )

  return(results)
}
