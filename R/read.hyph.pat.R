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


#' Reading patgen-compatible hyphenation pattern files
#' 
#' This function reads hyphenation pattern files, to be used with \code{\link[koRpus:hyphen]{hyphen}}.
#'
#' Hyphenation patterns that can be used are available from CTAN[1]. But actually any file
#' with only the patterns themselves, one per line, should work.
#'
#' The language designation is of no direct consequence here, but if the resulting pattern object is to be
#' used by other functions in this package, it should resamble the designation that's used for the
#' same language there.
#'
#' @param file A character string with a valid path to a file with hyphenation patterns (one pattern per line).
#' @param lang A character string, usually two letters short, naming the language the patterns are meant to
#'    be used with (e.g. "es" for Spanish).
#' @param fileEncoding A character string defining the character encoding of the file to be read. Unless
#'    you have a really good reason to do otherwise, your pattern files should all be UTF-8 encoded.
#' @return An object of class \code{\link[koRpus:kRp.hyph.pat-class]{kRp.hyph.pat-class}}.
#' @keywords hyphenation
#' @seealso
#'    \code{\link[koRpus:hyphen]{hyphen}},
#'    \code{\link[koRpus:manage.hyph.pat]{manage.hyph.pat}}
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @references
#' [1] \url{http://tug.ctan.org/tex-archive/language/hyph-utf8/tex/generic/hyph-utf8/patterns/txt/}
#' @export
#' @examples
#' \dontrun{
#' read.hyph.pat("~/patterns/hyph-en-us.pat.txt", lang="en_us")
#' }

read.hyph.pat <- function(file, lang, fileEncoding="UTF-8"){

  # check file
  check.file(file, mode="exist")

  hyphen.file.con <- file(file, open="r", encoding=fileEncoding)
  hyphen.raw <- readLines(hyphen.file.con)
  close(hyphen.file.con)
  # explicitly set encoding of this vector to generate objects which
  # do not cause warnings by R CMD check
  hyphen.raw <- enc2utf8(hyphen.raw)
  Encoding(hyphen.raw) <- "UTF-8"

  # .se5ra -> .sera
  hyphen.char <- gsub("[[:digit:]]", "", hyphen.raw)
  # z2weig -> "0z2weig"
  hyphen.nums <- gsub("(^[^.[:digit:]]+)", "0\\1", hyphen.raw, perl=TRUE)
  # .se5ra -> "00500"
  hyphen.nums <- gsub("[^[:digit:]]", 0, gsub("([^[:digit:]^[:punct:]])([[:digit:]])", "\\2", hyphen.nums, perl=TRUE))

  hyphen.pars <- cbind(orig=hyphen.raw, char=hyphen.char, nums=hyphen.nums)

  result <- new("kRp.hyph.pat", lang=lang, pattern=hyphen.pars)

  return(result)
}
