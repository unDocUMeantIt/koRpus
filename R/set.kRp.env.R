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


#' A function to set information on your koRpus environment
#'
#' The function \code{set.kRp.env} can be called before any of the analysing functions. It writes information
#' on your session environment regarding the koRpus package, e.g. path to a local TreeTagger installation,
#' to your global \code{\link[base:.Options]{.Options}}.
#'
#' To get the current settings, the function \code{\link[koRpus:get.kRp.env]{get.kRp.env}}
#' should be used. For the most part, \code{set.kRp.env} is a convenient wrapper for
#' \code{\link[base:options]{options}}. To permanently set some defaults, you could also add
#' respective \code{options} calls to an \code{\link[base:.Rprofile]{.Rprofile}} file.
#'
#' @param ... Named parameters to set in the koRpus environment. Valid arguments are:
#'   \describe{
#'     \item{TT.cmd}{ A character string pointing to the tagger command you want to use for basic text analysis, or \code{"manual"} if
#'        you want to set \code{TT.options} as well. Set to \code{"tokenize"} to use \code{\link[koRpus:tokenize]{tokenize}}.}
#'     \item{lang}{ A character string specifying a valid language.}
#'     \item{TT.options}{ A list with arguments to be used as \code{TT.options} by \code{\link[koRpus:treetag]{treetag}}.}
#'     \item{hyph.cache.file}{ A character string specifying a path to a file to use for storing already hyphenated data, used by
#'        \code{\link[koRpus]{hyphen}}.}
#'     \item{add.desc}{ A logical value, whether tag descriptions should be added directly to tagged text objects.}
#'   }
#'   To explicitly unset a value again, set it to an empty character string (e.g., \code{lang=""}).
#' @param validate Logical, if \code{TRUE} given paths will be checked for actual availablity, and the function will fail if files can't be found.
#' @return Returns an invisible \code{NULL}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords misc
#' @seealso \code{\link[koRpus:get.kRp.env]{get.kRp.env}}
#' @importFrom sylly set.sylly.env
#' @export
#' @examples
#' \dontrun{
#' set.kRp.env(TT.cmd="~/bin/treetagger/cmd/tree-tagger-german", lang="de")
#' get.kRp.env(TT.cmd=TRUE)
#' 
#' # example for setting permanent default values in an .Rprofile file
#' options(
#'   koRpus=list(
#'     TT.cmd="manual",
#'     TT.options=list(
#'       path="~/bin/treetagger",
#'       preset="de"),
#'     lang="de"
#'   )
#' )
#' # be aware that setting a permamnent default language without loading
#' # the respective language support package might trigger errors
#' }

set.kRp.env <- function(..., validate=TRUE){
  kRp.vars <- list(...)
  # set all desired variables
  TT.cmd <- kRp.vars[["TT.cmd"]]
  lang <- kRp.vars[["lang"]]
  TT.options <- kRp.vars[["TT.options"]]
  hyph.cache.file <- kRp.vars[["hyph.cache.file"]]
  add.desc <- kRp.vars[["add.desc"]]
  if(all(sapply(c(TT.cmd, lang, TT.options, hyph.cache.file, add.desc), is.null))){
    stop(simpleError("You must at least set one (valid) parameter!"))
  } else {}
  
  # get current settings from .Options
  koRpus_options <- getOption("koRpus", list())

  # assume using TreeTaggers tokenizer as the default
  TT.tknz <- TRUE
  manual.config <- TRUE
  if(!is.null(TT.cmd)){
    if(identical(TT.cmd, "tokenize")){
      TT.tknz <- FALSE
      manual.config <- FALSE
    } else {}
    if(identical(TT.cmd, "")){
      koRpus_options[["TT.cmd"]] <- NULL
    } else if(!identical(TT.cmd, "manual") & !identical(TT.cmd, "tokenize")){
      if(isTRUE(validate)){
        stopifnot(check.file(TT.cmd, mode="exec"))
      } else {}
      koRpus_options[["TT.cmd"]] <- file.path(TT.cmd)
    } else {
      koRpus_options[["TT.cmd"]] <- TT.cmd
    }
  } else {}

  if(!is.null(lang)){
    if(identical(lang, "")){
      koRpus_options[["lang"]] <- NULL
    } else {
      stopifnot(is.character(lang))
      koRpus_options[["lang"]] <- lang
    }
  } else {}

  if(!is.null(TT.options)){
    if(identical(TT.options, "")){
      koRpus_options[["TT.options"]] <- NULL
    } else {
      if(isTRUE(validate)){
        # moved TT.options checks to internal function to call it here
        checkTTOptions(TT.options=TT.options, manual.config=manual.config, TT.tknz=TT.tknz)
      } else {}
      koRpus_options[["TT.options"]] <- TT.options
    }
  } else {}

  if(!is.null(hyph.cache.file)){
    sylly::set.sylly.env(hyph.cache.file=hyph.cache.file)
  } else {}

  if(!is.null(add.desc)){
    if(is.logical(add.desc)){
      koRpus_options[["add.desc"]] <- add.desc
    } else {
      stop(simpleError("'add.desc' must be TRUE or FALSE!"))
    }
  } else {}

  options(koRpus=koRpus_options)

  return(invisible(NULL))
}
