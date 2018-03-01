# Copyright 2017 Meik Michalke <meik.michalke@hhu.de>
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

#' List available language packages
#' 
#' Get a list of all currently available language packages for koRpus from the official
#' l10n repository.
#' 
#' koRpus' language support is modular by design, meaning you can (and must) load
#' an extension package for each language you want to work with in a given session.
#' These language support packages are named \code{koRpus.lang.**}, where \code{**}
#' is replaced by a valid language identifier (like \code{en} for English or \code{de}
#' for German). See \code{\link[koRpus:set.lang.support]{set.lang.support}} for more details.
#' 
#' This function downloads the package list from (also) the official localization repository
#' for koRpus and lists all currently available language packages that you could install
#' and load. Apart from than it does not download or install anything.
#' 
#' You can install the packages by either calling the convenient wrapper function
#' \code{\link[koRpus:install.koRpus.lang]{install.koRpus.lang}}, or
#' \code{\link[utils:install.packages]{install.packages}} (see examples).
#'
#' @param repos The URL to additional repositories to query. You should probably leave this to the
#'    default, but if you would like to use a third party repository, you're free to do so. The
#'    value is temporarily appended to the repos currently returned by \code{getOption("repos")}.
#' @return Returns an invisible character vector with all available language packages.
#' @seealso \code{\link[koRpus:install.koRpus.lang]{install.koRpus.lang}}
#' @export
#' @examples
#' \dontrun{
#' # see all available language packages
#' available.koRpus.lang()
#' 
#' # install support for German
#' install.koRpus.lang("de")
#' # alternatively, you could call install.packages directly
#' install.packages("koRpus.lang.de", repos="https://undocumeantit.github.io/repos/l10n/")
#' }
available.koRpus.lang <- function(repos="https://undocumeantit.github.io/repos/l10n/"){
  # append repos, don't replace them
  repos <- c(getOption("repos"), l10n=repos)

  all_available <- check_lang_packages(available=TRUE, repos=repos, available.only=TRUE)

  if(length(all_available) < 1){
    message("No language support packages found in the repository.")
    return(invisible(NULL))
  } else {}

  supported_lang <- names(all_available)
  installed <- c()
  to_install <- c()
  for(this_lang in supported_lang){
    this_package <- all_available[[this_lang]]
    if(isTRUE(this_package[["installed"]])){
      installed[this_lang] <- " [installed"
      if(isTRUE(this_package[["loaded"]])){
        installed[this_lang] <- paste0(installed[this_lang], ", loaded]")
      } else {
        installed[this_lang] <- paste0(installed[this_lang], "]")
      }
    } else {
      installed[this_lang] <- ""
      to_install[this_lang] <- gsub("koRpus\\.lang\\.", "", this_lang)
    }
  }

  lang_msg <- paste0(
    "The following language support packages are currently available:\n\n  ",
    paste0(
      paste0(supported_lang, installed),
      collapse="\n  "
    ),
    "\n",
    if(length(to_install) > 1){
      paste0(
        "\nTo install all missing packages, run:\n\n  ",
        paste0("install.koRpus.lang(c(\"", paste0(to_install, collapse="\", \""), "\"))\n")
      )
    } else if(length(to_install) > 0){
      paste0(
        "\nTo install the missing package, run:\n\n  ",
        paste0("install.koRpus.lang(\"", to_install, "\")\n")
      )
    } else {}
  )

  message(lang_msg)
  return(invisible(supported_lang))
}
