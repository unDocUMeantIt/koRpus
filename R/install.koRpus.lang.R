# Copyright 2017-2019 Meik Michalke <meik.michalke@hhu.de>
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

#' Install language support packages
#' 
#' This is a wrapper for \code{\link[utils:install.packages]{install.packages}}, making it more
#' convenient to install additional language support packages for koRpus.
#' 
#' For a list of currently available language packages see \code{\link[koRpus:available.koRpus.lang]{available.koRpus.lang}}.
#' See \code{\link[koRpus:set.lang.support]{set.lang.support}} for more details on koRpus' language support in general.
#'
#' @param lang Character vector, one or more valid language identifiers (like \code{en} for English or \code{de}
#'    for German).
#' @param repos The URL to additional repositories to query. You should probably leave this to the
#'    default, but if you would like to use a third party repository, you're free to do so. The
#'    value is temporarily appended to the repos currently returned by \code{getOption("repos")}.
#' @param ... Additional options for \code{install.packages}.
#' @return Does not return any useful objects, just calls \code{\link[utils:install.packages]{install.packages}}.
#' @seealso \code{\link[utils:install.packages]{install.packages}}, \code{\link[koRpus:available.koRpus.lang]{available.koRpus.lang}}
#' @importFrom utils install.packages
#' @export
#' @examples
#' \dontrun{
#' # install support for German
#' install.koRpus.lang("de")
#' # load the package
#' library("koRpus.lang.de")
#' }
install.koRpus.lang <- function(lang, repos="https://undocumeantit.github.io/repos/l10n/", ...){
  # append repos, don't replace them
  repos <- c(getOption("repos"), l10n=repos)
  lang_packages <- paste0("koRpus.lang.", lang)
  all_available <- suppressMessages(available.koRpus.lang(repos=repos))
  valid_pckg <- lang_packages %in% all_available

  if(all(valid_pckg)){
    utils::install.packages(pkgs=lang_packages, repos=repos, ...)
  } else {
    stop(simpleError(
      paste0(
        "Invalid language packages:\n  ",
        paste0(lang_packages[!valid_pckg], collapse=", "),
        "\n\nPlease try available.koRpus.lang() for a list of valid packages!"
      )
    ))
  }
  
  return(invisible(NULL))
}
