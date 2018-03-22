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

#' @importFrom koRpus.lang.en lang.support.en
#' @importFrom sylly.en hyph.support.en
.onAttach <- function(...) {
  koRpus.lang.en::lang.support.en()
  sylly.en::hyph.support.en()
}
