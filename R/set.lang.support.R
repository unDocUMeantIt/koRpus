# Copyright 2010-2015 Meik Michalke <meik.michalke@hhu.de>
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
#' Language support in this package is designed to be extended easily. You could call it modular, although it's actually more "environemntal", but nevermind.
#' 
#' To add full new language support, say for Xyzedish, you basically have to call this function three times with different values, and provide respective hyphenation patterns.
#' If you would like to re-use this language support, you should consider making it a package.
#' 
#' Be it a package or a script, it should contain all three calls to this function. If it succeeds, it will fill an internal environment with the information you have defined.
#' 
#' The function \code{set.language.support()} gets called three times because there's three functions of koRpus that need language support:
#'
#' \itemize{
#'    \item hyphen() needs to know which language pattern tests are available as data files (which you must provide also)
#'    \item treetag() needs the preset information from its own start scripts
#'    \item kRp.POS.tags() needs to learn all possible POS tags that TreeTagger uses for the given language
#' }
#'
#' All the calls follow the same pattern -- first, you name one of the three targets explained above, and second, you provide a named list as the \code{value} for the
#' respective \code{target} function.
#' 
#' @section "hyphen":
#' 
#' The named list usually has one single entry to tell the new language abbreviation, e.g., \code{set.lang.support("hyphen", list("xyz"="xyz"))}. However,
#' this will only work if a) the language support script is a part of the \code{koRpus} package itself, and b) the hyphen pattern is located in its \code{data} subdirectory.
#' 
#' For your custom hyphenation patterns to be found automatically, provide it as the value in the named list, e.g., \code{set.lang.support("hyphen", list("xyz"=hyph.xyz))}.
#' This will directly add the patterns to \code{korpus}' environment, so it will be found when hyphenation is requested for language \code{"xyz"}.
#' 
#' If you would like to provide hyphenation as part of a third party language package, you must name the object \code{hyph.<lang>}, save it to your package's \code{data}
#' subdirectory named \code{hyph.<lang>.rda}, and append \code{package="<yourpackage>"} to the named list; e.g.,
#' \code{set.lang.support("hyphen", list("xyz"=c("xyz", package="koRpus.lang.xyz"))}. Only then \code{koRpus} will look for the pattern object in your package, not its own
#' \code{data} directory.
#' 
#' @section "treetag":
#' 
#' The presets for the treetag() function are basically what the shell (GNU/Linux, MacOS) and batch (Win) scripts define that come
#' with TreeTagger. Look for scripts called "$TREETAGGER/cmd/tree-tagger-xyzedish" and "$TREETAGGER\\cmd\\tree-tagger-xyzedish.bat",
#' figure out which call resembles which call and then define them in set.lang.support("treetag") accordingly.
#' 
#' @section "kRp.POS.tags":
#' 
#' If Xyzedish is supported by TreeTagger, you should find a tagset definition for the language on its homepage. treetag() needs
#' to know \emph{all} POS tags that TreeTagger might return, otherwise you will get a self-explaining error message as soon as an unknown
#' tag appears. Notice that this can still happen after you implemented the full documented tag set: sometimes the contributed TreeTagger
#' parameter files added their own tags, e.g., for special punctuation. So please test your tag set well.
#' 
#' As you can see, you will also have to add a global word class and an explaination for each tag.
#' The former is especially important for further steps like frequency analysis.
#' 
#' Please have a look at the existing language support files in the package sources, most of it should be almost self-explaining.
#' 
#' @section Hyphenation patterns:
#' 
#' To be able to also do syllable count with the newly added language, you should add a hyphenation pattern file as well.
#' Refer to the documentation of read.hyph.pat() to learn how to produce a pattern object from a downloaded hyphenation pattern
#' file. Then save this object to the "data" directory of the koRpus sources. Make sure you use the correct name scheme (e.g. "hyph.xyz.rda")
#' and good compression.
#' 
#' @param target  One of "hyphen", "kRp.POS.tags", or "treetag", depending on what support is to be added.
#' @param value A named list that upholds exactly the structure defined in inst/README.languages
#' @examples
#' \dontrun{
#' set.lang.support("hyphen",
#'   list("xyz"="xyz")
#' )
#' }
#' @export
set.lang.support <- function(target, value){

  all.kRp.env <- as.list(as.environment(.koRpus.env))

  if(identical(target, "hyphen")){
    recent.pattern <- all.kRp.env[["langSup"]][["hyphen"]][["supported"]]
    # could be there is no such entries in the environment yet
    if(is.null(recent.pattern)){
      recent.pattern <- list()
    } else {}
    # to be safe do this as a for loop; this should replace older entries
    # but keep all other intact or just add new ones
    for (this.pattern in names(value)){
      if(inherits(value[[this.pattern]], "kRp.hyph.pat")){
        # we got a pattern object, directly add it to the environment
        recent.pattern[[this.pattern]] <- this.pattern
        assign(paste0("hyph.", this.pattern), value[[this.pattern]], envir=as.environment(.koRpus.env))
      } else {
        recent.pattern[[this.pattern]] <- value[[this.pattern]]
      }
    }
    all.kRp.env[["langSup"]][["hyphen"]][["supported"]] <- recent.pattern
  } else if(identical(target, "kRp.POS.tags")){
    recent.tags <- all.kRp.env[["langSup"]][["kRp.POS.tags"]][["tags"]]
    # could be there is no such entries in the environment yet
    if(is.null(recent.tags)){
      recent.tags <- list()
    } else {}
    # to be safe do this as a for loop; this should replace older entries
    # but keep all other intact or just add new ones
    for (this.tags in names(value)){
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
