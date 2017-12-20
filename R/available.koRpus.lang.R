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
#' This function downloads the package list from the official localization repository
#' for koRpus and lists all currently available language packages that you could install
#' and load. Apart from than it does not download or install anything.
#' 
#' You can install the packages by either calling the convenient wrapper function
#' \code{\link[koRpus:install.koRpus.lang]{install.koRpus.lang}}, or
#' \code{\link[utils:install.packages]{install.packages}} (see examples).
#'
#' @param repos The URL to the repository to query. You should probably leave this to the
#'    default, but if you would like to use a third party repository, you're free to do so.
#' @return Returns an invisible character vector with all available language packages.
#' @seealso \code{\link[koRpus:install.koRpus.lang]{install.koRpus.lang}}, 
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
  all_available <- available.packages(repos=repos)
  available_koRpus_lang_package <- grepl("koRpus.lang.*", all_available[,"Package"])
  supported_lang <- unique(all_available[available_koRpus_lang_package,"Package"])

  lang_msg <- paste0(
    "The following language support packages are currently available:\n\n  ",
    paste0(supported_lang, collapse="\n  "), "\n"
  )

  message(lang_msg)
  return(invisible(supported_lang))
}
