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


#' A function to set information on your koRpus environmenton
#'
#' The function \code{set.kRp.env} can be called once before any of the analysing functions. It writes information
#' on your session environment regarding the koRpus package, e.g. path to a local TreeTagger installation,
#' to a hidden environment.
#'
#' To get the contents of the hitten environment, the function \code{\link[koRpus:get.kRp.env]{get.kRp.env}}
#' can be used.
#'
#' @param ... Named parameters to set in the koRpus environment. Valid arguments are:
#'   \describe{
#'     \item{TT.cmd}{ A character string pointing to the tagger command you want to use for basic text analysis, or \code{"manual"} if you want to set \code{TT.options} as well. Set to \code{"tokenize"} to use \code{\link[koRpus:tokenize]{tokenize}}.}
#'     \item{lang}{ A character string specifying a valid language.}
#'     \item{TT.options}{ A list with arguments to be used as \code{TT.options} by \code{treetag}.}
#'     \item{hyph.cache.file}{ A character string specifying a path to a file to use for storing already hyphenated data, used by \code{hyphen}.}
#'   }
#'   To explicitly unset a value again, set it to an empty character string (e.g., \code{lang=""}).
#' @param validate Logical, if \code{TRUE} given paths will be checked for actual availablity, and the function will fail if files can't be found.
#' @return Returns an invisible \code{NULL}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords misc
#' @seealso \code{\link[koRpus:get.kRp.env]{get.kRp.env}}
#' @export
#' @examples
#' \dontrun{
#' set.kRp.env(TT.cmd="~/bin/treetagger/cmd/tree-tagger-german", lang="de")
#' get.kRp.env(TT.cmd=TRUE)
#' }

set.kRp.env <- function(..., validate=TRUE){
  kRp.vars <- list(...)
  # set all desired variables
  TT.cmd <- kRp.vars[["TT.cmd"]]
  lang <- kRp.vars[["lang"]]
  TT.options <- kRp.vars[["TT.options"]]
  hyph.cache.file <- kRp.vars[["hyph.cache.file"]]
  if (all(c(is.null(TT.cmd), is.null(lang), is.null(TT.options), is.null(hyph.cache.file)))){
    stop(simpleError("You must at least set one (valid) parameter!"))
  } else {}

  # assume using TreeTaggers tokenizer as the default
  TT.tknz <- TRUE
  manual.config <- TRUE
  if(!is.null(TT.cmd)){
    if(identical(TT.cmd, "tokenize")){
      TT.tknz <- FALSE
      manual.config <- FALSE
    } else {}
    if(identical(TT.cmd, "")){
      rm("TT.cmd", envir=.koRpus.env)
    } else if(!identical(TT.cmd, "manual") & !identical(TT.cmd, "tokenize")){
      if(isTRUE(validate)){
        stopifnot(check.file(TT.cmd, mode="exec"))
      } else {}
      assign("TT.cmd", file.path(TT.cmd), envir=.koRpus.env)
    } else {
      assign("TT.cmd", TT.cmd, envir=.koRpus.env)
    }
  } else {}

  if(!is.null(lang)){
    if(identical(lang, "")){
      rm("lang", envir=.koRpus.env)
    } else {
      stopifnot(is.character(lang))
      assign("lang", lang, envir=.koRpus.env)
    }
  } else {}

  if(!is.null(TT.options)){
    if(identical(TT.options, "")){
      rm("TT.options", envir=.koRpus.env)
    } else {
      if(isTRUE(validate)){
        # moved TT.options checks to internal function to call it here
        checkTTOptions(TT.options=TT.options, manual.config=manual.config, TT.tknz=TT.tknz)
      } else {}
      assign("TT.options", TT.options, envir=.koRpus.env)
    }
  } else {}

  if(!is.null(hyph.cache.file)){
    if(identical(hyph.cache.file, "")){
      rm("hyph.cache.file", envir=.koRpus.env)
    } else {
      stopifnot(is.character(hyph.cache.file))
      assign("hyph.cache.file", hyph.cache.file, envir=.koRpus.env)
    }
  } else {}

  return(invisible(NULL))
}
