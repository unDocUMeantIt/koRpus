# Copyright 2010-2017 Meik Michalke <meik.michalke@hhu.de>
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


#' Add support for new languages
#' 
#' You can use this function to add new languages to be used with \code{koRpus}.
#' 
#' Language support in this package is designed to be extended easily. You could call it modular,
#' although it's actually more "environemntal", but nevermind.
#' 
#' To add full new language support, say for Xyzedish, you basically have to call this function
#' three times (or at least twice, see hyphen section below) with different targets.
#' If you would like to re-use this language support, you should consider making it a package.
#' 
#' Be it a package or a script, it should contain all three calls to this function. If it succeeds,
#' it will fill an internal environment with the information you have defined.
#' 
#' The function \code{set.language.support()} gets called three times because there's three
#' functions of koRpus that need language support:
#'
#' \itemize{
#'    \item treetag() needs the preset information from its own start scripts
#'    \item kRp.POS.tags() needs to learn all possible POS tags that TreeTagger uses for the given
#'       language
#'    \item hyphen() needs to know which language pattern tests are available as data files (which
#'       you must provide also)
#' }
#'
#' All the calls follow the same pattern -- first, you name one of the three targets explained above,
#' and second, you provide a named list as the \code{value} for the respective \code{target} function.
#' 
#' @section "treetag":
#' 
#' The presets for the treetag() function are basically what the shell (GNU/Linux, MacOS) and batch
#' (Win) scripts define that come with TreeTagger. Look for scripts called
#' "$TREETAGGER/cmd/tree-tagger-xyzedish" and "$TREETAGGER\\cmd\\tree-tagger-xyzedish.bat",
#' figure out which call resembles which call and then define them in set.lang.support("treetag")
#' accordingly.
#' 
#' Have a look at the commented template in your \code{koRpus} installation directory for an elaborate
#' example.
#' 
#' @section "kRp.POS.tags":
#' 
#' If Xyzedish is supported by TreeTagger, you should find a tagset definition for the language on its
#' homepage. treetag() needs to know \emph{all} POS tags that TreeTagger might return, otherwise you
#' will get a self-explaining error message as soon as an unknown tag appears. Notice that this can
#' still happen after you implemented the full documented tag set: sometimes the contributed TreeTagger
#' parameter files added their own tags, e.g., for special punctuation. So please test your tag set well.
#' 
#' As you can see in the template file, you will also have to add a global word class and an explaination
#' for each tag. The former is especially important for further steps like frequency analysis.
#' 
#' Again, please have a look at the commented template and/or existing language support files in the
#' package sources, most of it should be almost self-explaining.
#' 
#' @section "hyphen":
#' 
#' Using the target "hyphen" will cause a call to the equivalent of this function in the \code{sylly} package.
#' See the documentation of its \code{\link[sylly:set.hyph.support]{set.hyph.support}} function for details.
#' 
#' @section Packaging:
#'
#' If you would like to create a proper language support package, you should only include the "treetag" and
#' "kRp.POS.tags" calls, and the hyphenation patterns should be loaded as a dependency to a package called
#' \code{sylly.xx}. You can generate such a sylly package rather quickly by using the private function
#' \code{sylly:::sylly_langpack()}.
#' 
#' @param target  One of "kRp.POS.tags", "treetag", or "hyphen", depending on what support is to be added.
#' @param value A named list that upholds exactly the structure defined here for its respective \code{target}.
#' @examples
#' \dontrun{
#' set.lang.support("hyphen",
#'   list("xyz"="xyz")
#' )
#' }
#' @importFrom sylly set.hyph.support
#' @export
set.lang.support <- function(target, value){

  all.kRp.env <- as.list(as.environment(.koRpus.env))

  if(identical(target, "hyphen")){
    sylly::set.hyph.support(target=target)
  } else if(identical(target, "kRp.POS.tags")){
    recent.tags <- all.kRp.env[["langSup"]][["kRp.POS.tags"]][["tags"]]
    # could be there is no such entries in the environment yet
    if(is.null(recent.tags)){
      recent.tags <- list()
    } else {}
    # to be safe do this as a for loop; this should replace older entries
    # but keep all other intact or just add new ones
    for (this.tags in names(value)){
      # check for duplicate entries, they would completely mess up tagging results
      dupes <- c(
        value[[this.tags]][["tag.class.def.words"]][,"tag"],
        value[[this.tags]][["tag.class.def.punct"]][,"tag"],
        value[[this.tags]][["tag.class.def.sentc"]][,"tag"]
      )
      if(any(duplicated(dupes))){
        stop(simpleError(paste0("Some tags are defined multiple times, this is not allowed! Please check these tags:\n    ",
          paste0(unique(dupes[duplicated(dupes)]), collapse=", ")
        )))
      } else {}
      recent.tags[[this.tags]] <- value[[this.tags]]
    }
    all.kRp.env[["langSup"]][["kRp.POS.tags"]][["tags"]] <- recent.tags
  } else if(identical(target, "treetag")){
    recent.presets <- all.kRp.env[["langSup"]][["treetag"]][["presets"]]
    # could be there is no such entries in the environment yet
    if(is.null(recent.presets)){
      recent.presets <- list()
    } else {}
    # to be safe do this as a for loop; this should replace older entries
    # but keep all other intact or just add new ones
    for (this.preset in names(value)){
      recent.presets[[this.preset]] <- value[[this.preset]]
    }
    all.kRp.env[["langSup"]][["treetag"]][["presets"]] <- recent.presets
  } else {
    stop(simpleError(paste0("Invalid target for language support: ", target)))
  }
  list2env(all.kRp.env, envir=as.environment(.koRpus.env))
  return(invisible(NULL))
}
