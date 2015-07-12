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


#' Import Celex data
#' 
#' Read data from Celex[1] formatted corpora.
#'
#' @param celex.path A character string, path to a frequency file in Celex format to read.
#' @param running.words An integer value, number of running words in the Celex data corpus to be read.
#' @param fileEncoding A character string naming the encoding of the Celex files.
#' @param n An integer value defining how many lines of data should be read if \code{format="flatfile"}. Reads all at -1.
#' @return An object of class \code{\link[koRpus]{kRp.corp.freq-class}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords corpora
#' @seealso \code{\link[koRpus]{kRp.corp.freq-class}}
#' @references [1] \url{http://celex.mpi.nl}
#' @export
#' @examples
#' \dontrun{
#' my.Celex.data <- read.corp.celex("~/mydata/Celex/GERMAN/GFW/GFW.CD", running.words=5952000)
#' freq.analysis("/some/text.txt", corp.freq=my.Celex.data)
#' }

read.corp.celex <- function(celex.path, running.words, fileEncoding="ISO_8859-1", n=-1){

  # basic checks before we even proceed...
  # valid file?
  check.file(celex.path, mode="exist")

  ## here we go!

  # celex files can be veeeery large. if so, reading them will most likely freeze R
  # as a precaution we'll therefore use a file connection and readLines()
  celex.file.con <- file(celex.path, open="r", encoding=fileEncoding)
  rL.words <- readLines(celex.file.con, n=n)
  close(celex.file.con)

  table.words <- matrix(unlist(strsplit(rL.words, "\\", fixed=TRUE)), ncol=13, byrow=TRUE, dimnames=list(c(),c("num","word",3,"freq",5:13)))[,-c(3,5:13)]

  num.distinct.words  <- dim(table.words)[1]
  num.running.words   <- running.words
  avg.sntclgth.words  <- NA
  avg.sntclgth.chars  <- NA
  avg.wrdlgth.form    <- NA
  avg.wrdlgth.running <- NA

  dscrpt.meta <- data.frame(
    tokens=num.running.words,
    types=num.distinct.words,
    words.p.sntc=avg.sntclgth.words,
    chars.p.sntc=avg.sntclgth.chars,
    chars.p.wform=avg.wrdlgth.form,
    chars.p.word=avg.wrdlgth.running)

  # call internal function create.corp.freq.object()
  results <- create.corp.freq.object(matrix.freq=table.words,
            num.running.words=num.running.words,
            df.meta=data.frame(meta=NA,value=NA),
            df.dscrpt.meta=dscrpt.meta)

  return(results)
}
