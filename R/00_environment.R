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

# empty environment for various information
.koRpus.env <- new.env()

.onLoad <- function(...){
  ## check if "add.desc" is already set in global options
  if(is.null(getOption("koRpus"))){
    # case one: no koRpus options at all, that's easy
    options(koRpus=list(add.desc=FALSE, checked_lang_support=FALSE))
  } else {
    koRpus_options <- getOption("koRpus", list())
    for (thisOption in c("add.desc", "checked_lang_support")){
      if(is.null(koRpus_options[[thisOption]])){
        # case two, we have options, but add.desc is not set
        koRpus_options[[thisOption]] <- FALSE
        options(koRpus=koRpus_options)
      } else {
        if(!is.logical(koRpus_options[[thisOption]])){
          # case three, add.desc is set but invalid
          simpleError(paste0("check your environment: 'koRpus$", thisOption, "' must be TRUE or FALSE!"))
        } else {}
      }
    }
  }
}

# make sure language packs are loaded or at least available
.onAttach <- function(...){
  ## check for language support packages
  # koRpus is rather useless without at least one language support package loaded
  # but we only need to check this once
  koRpus_options <- getOption("koRpus", list())
  if(!isTRUE(koRpus_options[["checked_lang_support"]])){
    all_installed <- check_lang_packages(available=FALSE)
    have_koRpus_lang <- length(all_installed) > 0

    additional_info <- paste0("For a list of available language packages, please call:\n\n  available.koRpus.lang()\n")

    if(isTRUE(have_koRpus_lang)){
      supported_lang <- names(all_installed)
      installed <- sapply(
        all_installed,
        function(this_package){
          status <- paste0(" -- ", this_package[["title"]])
          if(isTRUE(this_package[["loaded"]])){
            status <- paste0(status, " [loaded]")
          } else {}
          return(status)
        }
      )
      lang_msg <- paste0(
        "\nFound the following language support packages installed on this system: \n\n  ",
        paste0(
          paste0(supported_lang, installed),
          collapse="\n  "
        ), "\n\n",
        if(!any(grepl("loaded", installed))){
          paste0("Please load the respective packages for all languages you need.\n\n")
        },
        additional_info
      )
      packageStartupMessage(lang_msg)
    } else {
      lang_msg <- paste0(
        "\nNo language support packages for koRpus found on this system!\n",
        "You need to install support packages for all languages you want to analyze.\n\n",
        additional_info
      )
      warning(lang_msg, call.=FALSE)
    }
    koRpus_options[["checked_lang_support"]] <- TRUE
    options(koRpus=koRpus_options)
  } else {}
}
