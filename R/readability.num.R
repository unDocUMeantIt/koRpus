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


#' Calculate readability
#' 
#' This function is a stripped down version of \code{\link[koRpus:readability]{readability}}. It does not analyze text,
#' but directly takes the values used by the formulae to calculate the readability measures.
#' 
#' @param txt.features A named list with statistical information on the text, or an object of class \code{kRp.readability}
#'    (only its \code{desc} slot will then be used). Valid values are:
#'    \describe{
#'      \item{\code{sentences}:}{The number of sentences.}
#'      \item{\code{words}:}{The number of words.}
#'      \item{\code{letters}:}{A named vector providing the number of letters. Must contain a value called \code{"all"},
#'        the total number of letters, and several values called \code{"l<digit>"}, giving the number of words
#'        with \code{<digit>} letters. To calculate all implemented measures with default parameters, you need
#'        at least the values \code{"l5"} (words with five \emph{or less} letters) and \code{"l6"} (words with six letters).}
#'      \item{\code{syllables}:}{Similar to \code{letters}, but providing the number of syllables.  Must contain a value called \code{"all"},
#'        the total number of syllables, and several values called \code{"s<digit>"}, giving the number of words
#'        with \code{<digit>} syllables. To calculate all implemented measures with default parameters, you need
#'        at least the values \code{"s1"} and \code{"s2"}.
#'        Only needed to calculate measures which need syllable count (see \code{\link[koRpus:readability]{readability}}).}
#'      \item{\code{punct}:}{The number of punctuation characters. Only needed to calculate \code{"TRI"}.}
#'      \item{\code{all.chars}:}{The number of all characters (including spaces). Only needed to calculate \code{Danielson.Bryan}.}
#'      \item{\code{prepositions}:}{The number of prepositions. Only needed to calculate \code{"Coleman"} and \code{"Traenkle.Bailer"}.}
#'      \item{\code{conjunctions}:}{The number of conjunctions. Only needed to calculate \code{"Traenkle.Bailer"}.}
#'      \item{\code{pronouns}:}{The number of pronouns. Only needed to calculate \code{"Coleman"}.}
#'      \item{\code{foreign}:}{The number of foreign words. Only needed to calculate \code{"TRI"}.}
#'      \item{\code{TTR}:}{The type-token ratio. Only needed to calculate \code{"Dickes.Steiwer"}.}
#'      \item{\code{FOG.hard.words}:}{The number of hard words, counted according to FOG. Only needed to calculate \code{"FOG"}.}
#'      \item{\code{Bormuth.NOL}:}{Number of words not on the Bormuth word list. Only needed to calculate \code{"Bormuth"}.}
#'      \item{\code{Dale.Chall.NOL}:}{Number of words not on the Dale-Chall word list. Only needed to calculate \code{"Dale.Chall"}.}
#'      \item{\code{Harris.Jacobson.NOL}:}{Number of words not on the Harris-Jacobson word list. Only needed to calculate \code{"Harris.Jacobson"}.}
#'      \item{\code{Spache.NOL}:}{Number of words not on the Spache word list. Only needed to calculate \code{"Spache"}.}
#'    }
#' @param index A character vector, indicating which indices should actually be computed.
#' @param parameters A named list with magic numbers, defining the relevant parameters for each index. If none are given,
#'    the default values are used.
#' @param ... Additional options, see \code{\link[koRpus:readability]{readability}}.
#'
#' @examples
#' \dontrun{
#'test.features <- list(
#'  sentences=18,
#'  words=556,
#'  letters=c(all=2918, l1=19, l2=92, l3=74, l4=80, l5=51, l6=49),
#'  syll=c(all=974, s1=316, s2=116),
#'  punct=78,
#'  all.chars=3553,
#'  prepositions=74,
#'  conjunctions=18,
#'  pronouns=9,
#'  foreign=0,
#'  TTR=0.5269784,
#'  Bormuth=192,
#'  Dale.Chall=192,
#'  Harris.Jacobson=240,
#'  Spache=240)
#'
#'# should not calculate FOG, because FOG.hard.words is missing:
#'readability.num(test.features, index="all")
#'}
#' @export

readability.num <- function(
      txt.features=list(
        sentences=NULL,
        words=NULL,
        letters=c(all=0, l5=0, l6=0),
        syllables=c(all=0, s1=0, s2=0),
        punct=NULL,
        all.chars=NULL,
        prepositions=NULL,
        conjunctions=NULL,
        pronouns=NULL,
        foreign=NULL,
        TTR=NULL,
        FOG.hard.words=NULL,
        Bormuth.NOL=NULL,
        Dale.Chall.NOL=NULL,
        Harris.Jacobson.NOL=NULL,
        Spache.NOL=NULL),
      index=c("ARI", "Bormuth", "Coleman", "Coleman.Liau",
        "Dale.Chall", "Danielson.Bryan", "Dickes.Steiwer","DRP",
        "ELF", "Farr.Jenkins.Paterson", "Flesch", "Flesch.Kincaid",
        "FOG", "FORCAST", "Fucks", "Harris.Jacobson", "Linsear.Write", "LIX", "nWS",
        "RIX", "SMOG", "Spache", "Strain", "Traenkle.Bailer", "TRI", "Tuldava",
        "Wheeler.Smith"),
      parameters=list(), ...){

  # check if txt.features is a readability result, and
  # probably fetch the desct slot from it:
  if(inherits(txt.features, "kRp.readability")){
    txt.features <- slot(txt.features, "desc")
  } else {}

  # this function checks if all needed data is present for a given readability measure
  # and removes measures from the index list otherwise
  # data is expected to be a vector or list
  got.all.i.need <- function(measure, data, value){
    if(any(measure %in% index)) {
      for(this.measure in index[index %in% measure]){
        for (this.data in value){
          if(is.null(data[[this.data]])){
            warning(paste0(this.measure, ": Missing data (", this.data, "), not calculated!"), call.=FALSE)
            index <- index[!index %in% this.measure]
          } else {}
        }
      }
    } else {
      # the measure is not on the list of indices to be
      # calculated in the first place
    }
    return(index)
  }

  all.valid.fixed.indices <- c("ARI", "ARI.NRI", "ARI.simple", "Bormuth", "Coleman", "Coleman.Liau",
      "Dale.Chall", "Dale.Chall.old", "Dale.Chall.PSK", "Danielson.Bryan",
      "Dickes.Steiwer", "DRP", "ELF", "Farr.Jenkins.Paterson", "Farr.Jenkins.Paterson.PSK",
      "Flesch", "Flesch.de", "Flesch.es", "Flesch.fr", "Flesch.Kincaid", "Flesch.nl",
      "Flesch.PSK", "Flesch.Szigriszt", "FOG", "FOG.NRI", "FOG.PSK", "FORCAST", "FORCAST.RGL",
      "Fucks", "Harris.Jacobson", "Linsear.Write", "LIX", "nWS", "RIX", "SMOG", "SMOG.C",
      "SMOG.de", "SMOG.simple", "Spache", "Spache.de", "Spache.old", "Strain", "Traenkle.Bailer", "TRI",
      "Tuldava", "Wheeler.Smith", "Wheeler.Smith.de")
  # activate all?
  if(identical(index, "all")){
    index <- all.valid.fixed.indices
  } else {}

  # check txt.features for minimum data
  for (this.feature in c("sentences", "words")){
    if(is.null(txt.features[[this.feature]])){
      stop(simpleError(paste0("Missing data: ", this.feature, "!")))
    } else {}
  }

  need.sylls <- c("Coleman", "ELF", "Farr.Jenkins.Paterson", "Farr.Jenkins.Paterson.PSK",
    "Flesch", "Flesch.de", "Flesch.es", "Flesch.fr", "Flesch.Kincaid", "Flesch.nl",
    "Flesch.PSK", "Flesch.Szigriszt", "FOG", "FOG.NRI", "FOG.PSK", "FORCAST", "FORCAST.RGL",
    "Linsear.Write", "nWS", "SMOG", "SMOG.C", "SMOG.de", "SMOG.simple", "Strain", "TRI",
    "Tuldava", "Wheeler.Smith", "Wheeler.Smith.de")
  # these indices are ok with only the global number of syllables
  global.sylls.ok <- c("Coleman", "Flesch", "Flesch.de", "Flesch.es", "Flesch.fr", "Flesch.Kincaid",
    "Flesch.nl", "Flesch.PSK", "Flesch.Szigriszt", "Strain", "Tuldava")
  # check if we have syllables, if needed
  if(any(index %in% need.sylls)){
    # if a global number is ok, rewrite the given value to remain compatible nonetheless
    if(all(index %in% global.sylls.ok) && length(txt.features[["syllables"]]) == 1){
      txt.features[["syllables"]] <- c(all=txt.features[["syllables"]], s1=0, s2=0)
    } else {}
    # this only works for default parameters as of now
    if(!all(c("all", "s1", "s2") %in% names(txt.features[["syllables"]]))){
      stop(simpleError(paste0("Missing information on syllable count. You need to at least define \"all\", \"s1\" and \"s2\" for these measures:\n\t",
        paste(index[need.sylls %in% index], collapse=", "))))
    } else {}
  } else {}
  need.letters <- c("ARI", "ARI.NRI", "ARI.simple", "Bormuth", "Coleman.Liau",
      "Danielson.Bryan", "Dickes.Steiwer", "DRP", "Fucks", "Harris.Jacobson", "LIX",
      "nWS", "RIX", "Traenkle.Bailer")
  # check if we have letters, if needed
  if(any(index %in% need.letters)){
    # this only works for default parameters as of now
    if(!all(c("all", "l5", "l6") %in% names(txt.features[["letters"]]))){
      stop(simpleError(paste0("Missing information on letter count. You need to at least define \"all\", \"l5\" and \"l6\" for these measures:\n\t",
        paste(index[need.letters %in% index], collapse=", "))))
    } else {}
  } else {}
  
  ## check demanded measures, is all data available?
  index <- got.all.i.need("Bormuth", data=txt.features, value="Bormuth.NOL")
  index <- got.all.i.need("Coleman", data=txt.features, value=c("prepositions", "pronouns"))
  index <- got.all.i.need(c("Dale.Chall", "Dale.Chall.old", "Dale.Chall.PSK"), data=txt.features, value="Dale.Chall.NOL")
  index <- got.all.i.need("Danielson.Bryan", data=txt.features, value="all.chars")
  index <- got.all.i.need("Dickes.Steiwer", data=txt.features, value="TTR")
  index <- got.all.i.need(c("FOG", "FOG.NRI", "FOG.PSK"), data=txt.features, value="FOG.hard.words")
  index <- got.all.i.need("Harris.Jacobson", data=txt.features, value="Harris.Jacobson.NOL")
  index <- got.all.i.need(c("Spache", "Spache.old", "Spache.de"), data=txt.features, value="Spache.NOL")
  index <- got.all.i.need("Traenkle.Bailer", data=txt.features, value=c("prepositions", "conjunctions"))
  index <- got.all.i.need("TRI", data=txt.features, value=c("foreign", "punct"))

  word.lists <- list(
    Bormuth=txt.features[["Bormuth.NOL"]],
    Dale.Chall=txt.features[["Dale.Chall.NOL"]],
    Harris.Jacobson=txt.features[["Harris.Jacobson.NOL"]],
    Spache=txt.features[["Spache.NOL"]])

  results <- kRp.rdb.formulae(index=index, analyze.text=FALSE, txt.features=txt.features, parameters=parameters,
    word.lists=word.lists, ...)

  return(results)
}
