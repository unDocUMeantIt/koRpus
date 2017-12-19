# Copyright 2016-2017 Meik Michalke <meik.michalke@hhu.de>
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

## setting up the internal environment

# empty environment for TreeTagger information
.koRpus.env <- new.env()
# set default for tag descriptions in objects
.koRpus.env[["add.desc"]] <- FALSE

## wrapper for paste0() needed?
if(isTRUE(R_system_version(getRversion()) < 2.15)){
  # if this is an older R version, we need a wrapper function for paste0()
  # which was introduced with R 2.15 as a more efficient shortcut to paste(..., sep="")
  paste0 <- function(..., collapse=NULL){
    return(paste(..., sep="", collapse=collapse))
  }
} else {}

# make sure the language packs are fully loaded
# after using 'imports' instead of 'depends' for these packages,
# we now need to call the resepective functions once
#' @importFrom sylly.de hyph.support.de
#' @importFrom sylly.en hyph.support.en
#' @importFrom sylly.es hyph.support.es
#' @importFrom sylly.fr hyph.support.fr
#' @importFrom sylly.it hyph.support.it
#' @importFrom sylly.ru hyph.support.ru
.onLoad <- function(...){
  sylly.de::hyph.support.de()
  sylly.en::hyph.support.en()
  sylly.es::hyph.support.es()
  sylly.fr::hyph.support.fr()
  sylly.it::hyph.support.it()
  sylly.ru::hyph.support.ru()

  ## check for language support packages
  # koRpus is rather useless without at least one language support package loaded
  all_loaded_namepsaces <- loadedNamespaces()
  any_loaded_koRpus_lang <- any(grepl("koRpus.lang.*", all_loaded_namepsaces))

  all_installed_packages <- installed.packages(fields="Title")
  is_koRpus_lang_package <- grepl("koRpus.lang.*", all_installed_packages[,"Package"])
  have_koRpus_lang <- any(is_koRpus_lang_package)

  additional_info <- paste0("\n\nFor a list of available language packages, please call:\n\n  koRpus:::available.koRpus.lang()\n")

  if(!isTRUE(any_loaded_koRpus_lang)){
    if(isTRUE(have_koRpus_lang)){
      supported_lang <- unique(all_installed_packages[is_koRpus_lang_package,"Package"])
      supported_lang_title <- unique(all_installed_packages[is_koRpus_lang_package,"Title"])
      lang_msg <- paste0(
        "\nFound the following language support packages on this system: \n\n  ",
        paste0(supported_lang, " -- ", supported_lang_title, collapse="\n  "), "\n\n",
        "Please load the respective packages for all languages you need.",
        additional_info
      )
      message(lang_msg)
    } else {
      lang_msg <- paste0(
        "\nNo language support packages for koRpus found on this system!\n",
        "You need to install support packages for all languages you want to analyze.",
        additional_info
      )
      warning(lang_msg, call.=FALSE)
    }
  } else {}
}

# this function prints info on available language packages
available.koRpus.lang <- function(repos="https://undocumeantit.github.io/repos/l10n/"){
  all_available <- available.packages(repos=repos)
  available_koRpus_lang_package <- grepl("koRpus.lang.*", all_available[,"Package"])
  supported_lang <- unique(all_available[available_koRpus_lang_package,"Package"])

  lang_msg <- paste0(
    "The following language support packages are currently available:\n\n  ",
    paste0(supported_lang, collapse="\n  "), "\n\n",
    "Installation (replace \"koRpus.lang.**\" with a valid package name):\n\n  ",
    "install.packages(\"koRpus.lang.**\", repo=\"https://undocumeantit.github.io/repos/l10n/\")\n\n",
    "For further information please visit:\n",
    "  https://undocumeantit.github.io/repos/\n"
  )

  message(lang_msg)
  return(invisible(NULL))
}
