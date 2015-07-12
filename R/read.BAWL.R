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


#' Import BAWL-R data
#'
#' Read the Berlin Affective Word List -- Reloaded (V\"o, Conrad, Kuchinke, Hartfeld, Hofmann & Jacobs, 2009; [1]) into a valid object of class
#' \code{\link[koRpus]{kRp.corp.freq-class}}.
#'
#' To use this function, you must first export the BAWL-R list into CSV format: Use comma for decimal values and semicolon as value separator
#' (often referred to as CSV2). Once you have successfully imported the word list, you can use the object to perform frequency analysis.
#'
#' @param csv A character string, path to the BAWL-R in CSV2 format.
#' @param fileEncoding A character string naming the encoding of the file, if neccessary.
#' @return An object of class \code{\link[koRpus]{kRp.corp.freq-class}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords corpora
#' @seealso \code{\link[koRpus]{kRp.corp.freq-class}}, \code{\link[koRpus:query]{query}},
#'    \code{\link[koRpus:kRp.text.analysis]{kRp.text.analysis}}
#' @references
#' V\"o, M. L.-H., Conrad, M., Kuchinke, L., Hartfeld, K., Hofmann, M.F. & Jacobs, A.M. (2009).
#'   The Berlin Affective Word List Reloaded (BAWL-R). \emph{Behavior Research Methods}, 41(2), 534--538.
#'
#' [1] \url{http://www.ewi-psy.fu-berlin.de/einrichtungen/arbeitsbereiche/allgpsy/BAWL-R/index.html}
#' @examples
#' \dontrun{
#' bawl.corp <- read.BAWL("~/mydata/valence/BAWL-R.csv")
#'
#' # you can now use query() now to create subsets of the word list,
#' # e.g., only nound with 5 letters and an valence rating of >= 1
#' bawl.stimulus <- query(bawl.corp,
#'   query=list(
#'     list(wclass="noun"),
#'     list(lttr=5),
#'     list("EMO_MEAN"=1, rel="ge")
#'   )
#' )
#' }
#' @export
read.BAWL <- function(csv, fileEncoding=NULL){
  # sanity check...
  if(is.character(csv) && grepl("*.xls", csv, ignore.case=TRUE)){
    stop(simpleError("You must export the BAWL-R list into CSV format first! Use comma for decimal values and semicolon as value separator (CSV2)."))
  } else {}

  if(is.null(fileEncoding)){
    fileEncoding <- ""
  } else {}

  # probe file for format
  fileProbe <- enc2utf8(readLines(csv, n=2, encoding=fileEncoding))[-1]
  if(any(grepl("\"", fileProbe))){
    message("Oh, well... someone had trouble following our instructions. Trying to work around the wrong data format...")
    BAWL.raw <- read.csv(file=csv,  na.strings="NA", nrows=-1, skip=0, check.names=TRUE, strip.white=TRUE,
      blank.lines.skip=TRUE, stringsAsFactors=FALSE)
    # try to turn character factors into numeric values
    for (thisVar in colnames(BAWL.raw)[4:19]){
      BAWL.raw[[thisVar]] <- as.numeric(gsub(",", ".", as.character(BAWL.raw[[thisVar]])))
      rm(thisVar)
    }
  } else {
    BAWL.raw <- read.csv2(file=csv,  na.strings="NA", nrows=-1, skip=0, check.names=TRUE, strip.white=TRUE,
      blank.lines.skip=TRUE, stringsAsFactors=FALSE)
  }

  # add koRpus POS tags
  BAWL.tags <- BAWL.wclass <- BAWL.raw[["WORD_CLASS"]]
  BAWL.tags[BAWL.tags %in% "A"] <- "ADJ"
  BAWL.wclass[BAWL.wclass %in% "A"] <- "adjective"
  BAWL.tags[BAWL.tags %in% "N"] <- "NN"
  BAWL.wclass[BAWL.wclass %in% "N"] <- "noun"
  BAWL.wclass[BAWL.wclass %in% "V"] <- "verb"

  # call internal function create.corp.freq.object()
  corp.freq.mtx <- matrix(
      c(1:nrow(BAWL.raw), BAWL.raw[["WORD_LOWER"]], BAWL.raw[["Ftot.1MIL"]], BAWL.tags, BAWL.wclass),
      ncol=5, dimnames=list(c(),c("num", "word", "freq", "tag", "wclass"))
    )

  # descriptive statistics
  dscrpt.meta <- data.frame(
    tokens=NA,
    types=NA,
    words.p.sntc=NA,
    chars.p.sntc=NA,
    chars.p.wform=NA,
    chars.p.word=NA)

  results <- create.corp.freq.object(matrix.freq=corp.freq.mtx,
            num.running.words=1000000,
            # "df.meta" must be a data.frame with two columns: "meta" (name of meta information) and its "value".
            df.meta=as.data.frame(matrix(ncol=2, dimnames=list(c(),c("meta", "value")))),
            df.dscrpt.meta=dscrpt.meta,
            extra.cols=BAWL.raw)

  return(results)
}
