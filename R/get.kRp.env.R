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


#' Get koRpus session environment
#'
#' The function \code{get.kRp.env} returns information on your session environment regarding the koRpus package, e.g.
#' where your local TreeTagger installation resides, if it was set before using
#' \code{\link[koRpus:set.kRp.env]{set.kRp.env}}.
#'
#' @param ... Named parameters to get from the koRpus environment. Valid arguments are:
#'   \describe{
#'     \item{TT.cmd}{ Logical, whether the set tagger command should be returned.}
#'     \item{lang}{ Logical, whether the set language should be returned.}
#'     \item{TT.options}{ Logical, whether the set TT.options for \code{treetag} should be returned.}
#'     \item{hyph.cache.file}{ Logical, whether the set hyphenation cache file for \code{hyphen} should be returned.}
#'   }
#' @param errorIfUnset Logical, if \code{TRUE} and the desired property is not set at all, the function will fail with an error message.
#' @return A character string or list, possibly including:
#'  \item{TT.cmd}{Path information for the TreeTagger command}
#'  \item{lang}{The specified language}
#'  \item{TT.options}{A list with options for \code{treetag}}
#'  \item{hyph.cache.file}{The specified hyphenation cache file for \code{hyphen}}
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords misc
#' @seealso \code{\link[koRpus:set.kRp.env]{set.kRp.env}}
#' @export
#' @examples
#' \dontrun{
#' set.kRp.env(TT.cmd="~/bin/treetagger/cmd/tree-tagger-german", lang="de")
#' get.kRp.env(TT.cmd=TRUE)
#' }

get.kRp.env <- function(..., errorIfUnset=TRUE){
  kRp.vars <- list(...)
  # set all desired variables
  TT.cmd <- kRp.vars[["TT.cmd"]]
  lang <- kRp.vars[["lang"]]
  TT.options <- kRp.vars[["TT.options"]]
  hyph.cache.file <- kRp.vars[["hyph.cache.file"]]
  if (all(c(is.null(TT.cmd), is.null(lang), is.null(TT.options), is.null(hyph.cache.file)))){
    stop(simpleError("You must at least set one (valid) parameter!"))
  } else {}
  if(!all(is.logical(unlist(kRp.vars)))){
    stop(simpleError("You can only use logical values to query parameters!"))
  } else {}

  tt.env <- list()
  if(isTRUE(TT.cmd)){
    if(exists("TT.cmd", envir=.koRpus.env, inherits=FALSE)){
      tt.env$TT.cmd <- get("TT.cmd", envir=.koRpus.env)
      if(!identical(tt.env$TT.cmd, "manual") & !identical(tt.env$TT.cmd, "tokenize")){
        stopifnot(check.file(tt.env$TT.cmd, mode="exec"))
      } else {}
    } else {
      if(isTRUE(errorIfUnset)){
        stop(simpleError("No TreeTagger command specified! If you want to use TreeTagger, you must tell treetag() where it is installed, either by using set.kRp.env() or setting 'treetagger' an 'TT.options' accordingly."))
      } else {}
    }
  } else {}

  if(isTRUE(lang)){
    if(exists("lang", envir=.koRpus.env, inherits=FALSE)){
      tt.env$lang <- get("lang", envir=.koRpus.env)
    } else {
      if(isTRUE(errorIfUnset)){
        stop(simpleError("No language specified!"))
      } else {}
    }
  } else {}

  if(isTRUE(TT.options)){
    if(exists("TT.options", envir=.koRpus.env, inherits=FALSE)){
      tt.env$TT.options <- get("TT.options", envir=.koRpus.env)
    } else {
      if(isTRUE(errorIfUnset)){
        stop(simpleError("No TT.options specified!"))
      } else {}
    }
  } else {}

  if(isTRUE(hyph.cache.file)){
    if(exists("hyph.cache.file", envir=.koRpus.env, inherits=FALSE)){
      tt.env$hyph.cache.file <- get("hyph.cache.file", envir=.koRpus.env)
    } else {
      if(isTRUE(errorIfUnset)){
        stop(simpleError("No hyphenation cache file specified!"))
      } else {}
    }
  } else {}

  if(length(tt.env) == 1){
    tt.env <- tt.env[[1]]
  } else if(length(tt.env) < 1){
    tt.env <- NULL
  } else {}

  return(tt.env)
}
