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


#' Guess language a text is written in
#' 
#' This function tries to guess the language a text is written in.
#'
#' To accomplish the task, the method described by Benedetto, Caglioti & Loreto (2002) is used, utilizing both
#' gzip compression and tranlations of the Universal Declaration of Human Rights[1]. The latter holds the world
#' record for being translated into the most different languages, and is publicly available.
#'
#' @note For this implementation the documents provided by the "UDHR in Unicode" project[2] have been used.
#' Their translations are \emph{not part of this package} and must be downloaded seperately to use \code{guess.lang}!
#' You need the ZIP archive containing \emph{all the plain text files} from \url{http://unicode.org/udhr/downloads.html}.
#'
#' @param txt.file A character vector pointing to the file with the text to be analyzed.
#' @param udhr.path A character string, either pointing to the directory where you unzipped the translations of the
#'    Universal Declaration of Human Rights, or to the ZIP file containing them.
#' @param comp.length Numeric value, giving the number of characters to be used of \code{txt} to estimate the language.
#' @param keep.udhr Logical, whether all the UDHR translations should be kept in the resulting object.
#' @param quiet Logical. If \code{FALSE}, short status messages will be shown.
#' @param in.mem Logical. If \code{TRUE}, the gzip compression will remain in memory (using \code{memCompress}), which
#'    is probably the faster method. Otherwise temporary files are created and automatically removed on exit.
#' @param format Either "file" or "obj". If the latter, \code{txt.file} is not interpreted as a file path but the text to analyze itself.
#' @return An object of class \code{\link[koRpus]{kRp.lang-class}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @references
#' Benedetto, D., Caglioti, E. & Loreto, V. (2002). Language trees and zipping. \emph{Physical Review Letters}, 88(4), 048702.
#'
#' [1] \url{http://www.ohchr.org/EN/UDHR/Pages/UDHRIndex.aspx}
#'
#' [2] \url{http://unicode.org/udhr}
#' @keywords misc
#' @examples
#' \dontrun{
#'   # using the still zipped bulk file
#'   guess.lang("/home/user/data/some.txt", udhr.path="/home/user/data/udhr_txt.zip")
#'   # using the unzipped UDHR archive
#'   guess.lang("/home/user/data/some.txt", udhr.path="/home/user/data/udhr_txt/")
#' }
#' @export

guess.lang <- function(txt.file, udhr.path, comp.length=300, keep.udhr=FALSE, quiet=TRUE, in.mem=TRUE, format="file"){

  # try to read the file
  if(file.exists(txt.file)){
    txt <- paste(scan(txt.file, what=character(), quiet=quiet), collapse=" ")
  } else if(identical(format, "obj")) {
    txt <- txt.file
  } else {
    stop(simpleError(paste("File not found:", txt.file)))
  }

  # force text into UTF-8 format
  txt <- enc2utf8(txt)

  # read the declarations into a data.frame
  udhr <- read.udhr(udhr.path, quiet=quiet)

  # limit test to a certain number of characters
  text.sliced <- unlist(strsplit(txt, split=""))
  # see if text is long enough
  text.length <- length(text.sliced)
  if(text.length < comp.length){
    warning(paste0("Text is shorter (", text.length, " characters) than defined comp.length  (", comp.length,
      " characters).\n  Full text was used and comp.length ignored."))
    txt.short <- paste(text.sliced, collapse="")
  } else {
    txt.short <- paste(text.sliced[1:comp.length], collapse="")
  }

  udhr.comressed <- c()
  for (num.udhr in 1:dim(udhr)[1]){
    curr.udhr <- udhr[num.udhr,]
    if(!isTRUE(quiet)){
      cat(paste0("Comparing text to ", curr.udhr["name"]), "...\n", sep="")
    } else {}
    udhr.plain <- paste(curr.udhr["text"])
    udhr.plus.text <- paste(curr.udhr["text"], txt.short, collapse="")
    udhr.plain.gz <- txt.compress(udhr.plain, in.mem=in.mem)
    udhr.plus.text.gz <- txt.compress(udhr.plus.text, in.mem=in.mem)
    compression.diff <- udhr.plus.text.gz$gz.size - udhr.plain.gz$gz.size
    udhr.comressed <- c(udhr.comressed, compression.diff)
  }

  # check if to throw out all the full declarations
  if(isTRUE(keep.udhr)){
    udhr.results <- udhr
  } else {
    # define "text" and never use it, to satisfy the picky R CMD check...
    # "text" only appears in subset() below, meaning the variable inside the subsetted object.
    # who cares.
    text <- NULL
    udhr.results <- subset(udhr, select=-text)
  }
  # add the diff
  udhr.results[["diff"]] <- udhr.comressed
  udhr.results[["diff.std"]] <- scale(udhr.comressed)

  # sort results by diff
  udhr.results <- udhr.results[order(udhr.results[["diff"]]),]
  dimnames(udhr.results)[[1]] <- 1:length(dimnames(udhr.results)[[1]])

  # get the best match
  lang.estim <- udhr.results[1,"name"]
  lang.estim.lc <- udhr.results[1,"iso639-3"]

  results <- new("kRp.lang", lang=lang.estim.lc, lang.name=lang.estim, txt=txt.short, txt.full=txt, udhr=udhr.results)

  return(results)
}
