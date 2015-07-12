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


#' Import LCC data
#'
#' Read data from LCC[1] formatted corpora (Quasthoff, Richter & Biemann, 2006).
#'
#' The LCC database can either be unpacked or still a .tar/.tar.gz/.zip archive. If the latter is the case, then
#' all necessary files will be extracted to a temporal location automatically, and by default removed again
#' when the function has finished reading from it.
#'
#' @note Please note that MySQL support is not implemented yet.
#'
#' @param LCC.path A character string, either path to a .tar/.tar.gz/.zip file in LCC format (flatfile),
#'    or the path to the directory with the unpacked archive.
#' @param format Either "flatfile" or "MySQL", depending on the type of LCC data.
#' @param fileEncoding A character string naming the encoding of the LCC files. Old zip archives used "ISO_8859-1".
#'    This option will only influence the reading of meta information, as the actual database encoding is derived from
#'    there.
#' @param n An integer value defining how many lines of data should be read if \code{format="flatfile"}. Reads all at -1.
#' @param keep.temp Logical. If \code{LCC.path} is a tarred/zipped archive, setting \code{keep.temp=TRUE} will keep
#'    the temporarily unpacked files for further use. By default all temporary files will be removed when
#'    the function ends.
#' @param prefix Character string, giving the prefix for the file names in the archive. Needed for newer LCC tar archives
#'    if they are already decompressed (autodetected if \code{LCC.path} points to the tar archive directly).
#' @return An object of class \code{\link[koRpus]{kRp.corp.freq-class}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords corpora
#' @seealso \code{\link[koRpus]{kRp.corp.freq-class}}
#' @references Quasthoff, U., Richter, M. & Biemann, C. (2006). Corpus Portal for Search in Monolingual Corpora, In
#'    \emph{Proceedings of the Fifth International Conference on Language Resources and Evaluation}, Genoa, 1799--1802.
#'
#'    [1] \url{http://corpora.informatik.uni-leipzig.de/download.html}
#' @export
#' @examples
#' \dontrun{
#' # old format .zip archive
#' my.LCC.data <- read.corp.LCC("~/mydata/corpora/de05_3M.zip")
#' # new format tar archive
#' my.LCC.data <- read.corp.LCC("~/mydata/corpora/rus_web_2002_300K-text.tar")
#' # in case the tar archive was already unpacked
#' my.LCC.data <- read.corp.LCC("~/mydata/corpora/rus_web_2002_300K-text", prefix="rus_web_2002_300K-")
#' 
#' tagged.results <- treetag("/some/text.txt")
#' freq.analysis(tagged.results, corp.freq=my.LCC.data)
#' }

read.corp.LCC <- function(LCC.path, format="flatfile", fileEncoding="UTF-8", n=-1, keep.temp=FALSE, prefix=NULL){

  # basic checks before we even proceed...
  if(identical(format, "flatfile")){
    # check if LCC.path is a zip file
    if(!as.logical(file_test("-d", LCC.path))){
      if(file.exists(LCC.path)){
        # prepare temporary location to later alter variable to use that location
        tmp.path <- tempfile("koRpus.LCC")
        if(!dir.create(tmp.path, recursive=TRUE)) stop(simpleError("Can't create temporary directory!"))
        if(isTRUE(keep.temp)){
          message(paste("Unpacked data will be kept in\n", tmp.path))
        } else {
          # if the function is done, remove the tempdir
          on.exit(unlink(tmp.path, recursive=TRUE))
        }
        # old zip format
        if(grepl("(\\.zip|\\.ZIP)$", LCC.path)){
          # seems to be an existing zip file
          message("Unzipping LCC archive... ", appendLF=FALSE)
          # to save time and diskspace, unpack only the needed files
          LCC.zip.content <- as.character(unzip(LCC.path, list=TRUE)$Name)
          LCC.zip.wanted <- LCC.zip.content[grep("(words|meta).txt$", LCC.zip.content)]
          unzip(LCC.path, files=LCC.zip.wanted, junkpaths=TRUE, exdir=tmp.path)
          message("done.")
        } else if(grepl("(\\.tar|\\.tar.gz|\\.tgz)$", tolower(LCC.path))){
          # seems to be an existing tar file
          # compression should be detected automatically
          message("Fetching needed files from LCC archive... ", appendLF=FALSE)
          # to save time and diskspace, unpack only the needed files
          LCC.tar.content <- as.character(untar(LCC.path, list=TRUE))
          LCC.tar.wanted <- LCC.tar.content[grep("(words|meta).txt$", LCC.tar.content)]
          if(is.null(prefix)){
            prefix <- gsub("(words|meta).txt$", "", LCC.tar.wanted)[1]
          } else {}
          untar(LCC.path, files=LCC.tar.wanted, exdir=tmp.path)
          message("done.")
        } else {
          stop(simpleError(paste("Unknown LCC data format:", LCC.path)))
        }
        # the database should now be in tmp.path
        # alter variable to use the temporary location
        LCC.path <- tmp.path
      } else {
        stop(simpleError(paste("Cannot access LCC data:", LCC.path)))
      }
    } else {
      # valid path?
      check.file(LCC.path, mode="dir")
    }

    # does meta.txt exist?
    LCC.meta <- file.path(LCC.path, paste0(prefix, "meta.txt"))
    check.file(LCC.meta, mode="exist")
    # does words.txt exist?
    LCC.words <- file.path(LCC.path, paste0(prefix, "words.txt"))
    check.file(LCC.words, mode="exist")
  } else if(identical(format, "MySQL")){
    stop(simpleMessage("Sorry, not implemented yet..."))
  } else {
    stop(simpleError(paste("Unknown format:", format)))
  }

  ## here we go!
  # TODO: need another if(identical(format, "flatfile")) here when MySQL is implemented?

  table.meta <- read.delim(LCC.meta, header=FALSE, col.names=c(1,"meta","value"),
      strip.white=TRUE, fill=FALSE, stringsAsFactors=FALSE, fileEncoding=fileEncoding)[,-1]
  # check the format type
  if(all(c("number of distinct word forms", "average sentence length in characters") %in% table.meta[,1])){
    LCC.archive.format <- "zip"
  } else if(all(c("SENTENCES", "WORD_TOKENS", "WORD_TYPES") %in% table.meta[,1])){
    LCC.archive.format <- "tar"
  } else {
    stop(simpleError("Sorry, this format is not supported! Please contact the authors."))
  }
  if(identical(LCC.archive.format, "zip")){
    num.distinct.words  <- as.numeric(table.meta[table.meta[,1] == "number of distinct word forms", 2])
    num.running.words   <- as.numeric(table.meta[table.meta[,1] == "number of running word forms", 2])
    avg.sntclgth.words  <- as.numeric(table.meta[table.meta[,1] == "average sentence length in words", 2])
    avg.sntclgth.chars  <- as.numeric(table.meta[table.meta[,1] == "average sentence length in characters", 2])
    avg.wrdlgth.form    <- as.numeric(table.meta[table.meta[,1] == "average word form length", 2])
    avg.wrdlgth.running <- as.numeric(table.meta[table.meta[,1] == "average running word length", 2])
    fileEncoding        <- table.meta[table.meta[,1] == "database encoding", 2]
  } else if(identical(LCC.archive.format, "tar")) {
    num.distinct.words  <- as.numeric(table.meta[table.meta[,1] == "WORD_TYPES", 2])
    num.running.words   <- as.numeric(table.meta[table.meta[,1] == "WORD_TOKENS", 2])
    avg.sntclgth.words <- avg.sntclgth.chars  <- avg.wrdlgth.form <- avg.wrdlgth.running <- NA
    fileEncoding        <- table.meta[table.meta[,1] == "database encoding", 2]
  }

  dscrpt.meta <- data.frame(
    tokens=num.running.words,
    types=num.distinct.words,
    words.p.sntc=avg.sntclgth.words,
    chars.p.sntc=avg.sntclgth.chars,
    chars.p.wform=avg.wrdlgth.form,
    chars.p.word=avg.wrdlgth.running)

  # LCC files can be veeeery large. if so, reading them will most likely freeze R
  # as a precaution we'll therefore use a file connection and readLines()
  LCC.file.con <- file(LCC.words, open="r", encoding=fileEncoding)
  rL.words <- readLines(LCC.file.con, n=n)
  close(LCC.file.con)

  table.words <- matrix(unlist(strsplit(rL.words, "\t")), ncol=3, byrow=TRUE, dimnames=list(c(),c("num","word","freq")))

  # call internal function create.corp.freq.object()
  results <- create.corp.freq.object(matrix.freq=table.words,
            num.running.words=num.running.words,
            df.meta=table.meta,
            df.dscrpt.meta=dscrpt.meta)

  return(results)
}
