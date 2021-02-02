# Copyright 2010-2021 Meik Michalke <meik.michalke@hhu.de>
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


## ARI()
#' Readability: Automated Readability Index (ARI)
#' 
#' This is just a convenient wrapper function for \code{\link[koRpus:readability]{readability}}.
#'
#' Calculates the Automated Readability Index (ARI). In contrast to \code{\link[koRpus:readability]{readability}},
#' which by default calculates all possible indices, this function will only calculate the index value.
#'
#' If \code{parameters="NRI"}, the simplified parameters from the Navy Readability Indexes are used, if set to
#' \code{ARI="simple"}, the simplified formula is calculated.
#'
#' This formula doesn't need syllable count.
#'
#' @param txt.file Either an object of class \code{\link[koRpus:kRp.text-class]{kRp.text}}, a character vector which must be be
#'    a valid path to a file containing the text to be analyzed, or a list of text features. If the latter, calculation
#'    is done by \code{\link[koRpus:readability.num]{readability.num}}. 
#' @param parameters A numeric vector with named magic numbers, defining the relevant parameters for the index.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:readability]{readability}} for details.
#' @return An object of class \code{\link[koRpus:kRp.readability-class]{kRp.readability}}.
#' @references
#'    DuBay, W.H. (2004). \emph{The Principles of Readability}. Costa Mesa: Impact Information.
#'      WWW: \url{http://www.impact-information.com/impactinfo/readability02.pdf}; 22.03.2011.
#'
#'    Smith, E.A. & Senter, R.J. (1967). \emph{Automated readability index}. AMRL-TR-66-22. Wright-Paterson AFB, Ohio: Aerospace Medical Division.
#' @keywords readability
#' @export
#' @examples
#' \dontrun{
#' ARI(tagged.text)
#' }

ARI <- function(txt.file, parameters=c(asl=0.5, awl=4.71, const=21.43), ...){
  if(is.list(txt.file)){
    results <- readability.num(txt.features=txt.file, index="ARI", parameters=list(ARI=parameters), ...)
  } else {
    results <- readability(txt.file=txt.file, index="ARI", parameters=list(ARI=parameters), ...)
  }
  return(results)
}
## end ARI()


## bormuth()
#' Readability: Bormuth's Mean Cloze and Grade Placement
#' 
#' This is just a convenient wrapper function for \code{\link[koRpus:readability]{readability}}.
#'
#' Calculates Bormuth's Mean Cloze and estimted grade placement. In contrast to
#' \code{\link[koRpus:readability]{readability}}, which by default calculates all possible indices,
#' this function will only calculate the index value.
#'
#' This formula doesn't need syllable count.
#'
#' @usage bormuth(txt.file, word.list, clz=35,
#'    meanc=c(const=0.886593, awl=0.08364, afw=0.161911,
#'      asl1=0.021401, asl2=0.000577, asl3=0.000005),
#'    grade=c(const=4.275, m1=12.881, m2=34.934, m3=20.388,
#'      c1=26.194, c2=2.046, c3=11.767, mc1=44.285, mc2=97.62,
#'      mc3=59.538), ...)
#' @param txt.file Either an object of class \code{\link[koRpus:kRp.text-class]{kRp.text}}, a character vector which must be be
#'    a valid path to a file containing the text to be analyzed, or a list of text features. If the latter, calculation
#'    is done by \code{\link[koRpus:readability.num]{readability.num}}. 
#' @param clz Integer, the cloze criterion score in percent.
#' @param meanc A numeric vector with named magic numbers, defining the relevant parameters for Mean Cloze calculation.
#' @param grade A numeric vector with named magic numbers, defining the relevant parameters for Grade Placement calculation.
#'    If omitted, Grade Placement will not be calculated.
#' @param word.list A vector or matrix (with exactly one column) which defines familiar words. For valid results
#'    the long Dale-Chall list with 3000 words should be used.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:readability]{readability}} for details.
#' @return An object of class \code{\link[koRpus:kRp.readability-class]{kRp.readability}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords readability
#' @export
#' @examples
#' \dontrun{
#'   bormuth(tagged.text, word.list=new.dale.chall.wl)
#' }

bormuth <- function(txt.file, word.list, clz=35,
      meanc=c(const=0.886593, awl=0.08364, afw=0.161911, asl1=0.021401, asl2=0.000577, asl3=0.000005),
      grade=c(const=4.275, m1=12.881, m2=34.934, m3=20.388, c1=26.194, c2=2.046, c3=11.767, mc1=44.285, mc2=97.62, mc3=59.538), ...){
  param.list <- list(clz=clz, meanc=meanc, grade=grade)
  if(is.list(txt.file)){
    results <- readability.num(txt.features=txt.file, index="Bormuth", parameters=list(Bormuth=param.list), ...)
  } else {
    results <- readability(txt.file=txt.file, index="Bormuth", parameters=list(Bormuth=param.list), word.lists=list(Bormuth=word.list), ...)
  }
  return(results)
}
## end bormuth()


## coleman.liau()
#' Readability: Coleman-Liau Index
#' 
#' This is just a convenient wrapper function for \code{\link[koRpus:readability]{readability}}.
#'
#' Calculates the Coleman-Liau index. In contrast to \code{\link[koRpus:readability]{readability}},
#' which by default calculates all possible indices, this function will only calculate the index value.
#'
#' This formula doesn't need syllable count.
#'
#' @param txt.file Either an object of class \code{\link[koRpus:kRp.text-class]{kRp.text}}, a character vector which must be be
#'    a valid path to a file containing the text to be analyzed, or a list of text features. If the latter, calculation
#'    is done by \code{\link[koRpus:readability.num]{readability.num}}. 
#' @param ecp A numeric vector with named magic numbers, defining the relevant parameters for the cloze percentage estimate.
#' @param grade A numeric vector with named magic numbers, defining the relevant parameters to calculate grade equvalent for ECP values.
#' @param short A numeric vector with named magic numbers, defining the relevant parameters for the short form of the formula.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:readability]{readability}} for details.
#' @return An object of class \code{\link[koRpus:kRp.readability-class]{kRp.readability}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords readability
#' @export
#' @examples
#' \dontrun{
#' coleman.liau(tagged.text)
#' }

coleman.liau <- function(txt.file,
        ecp=c(const=141.8401, char=0.21459, sntc=1.079812),
        grade=c(ecp=-27.4004, const=23.06395),
        short=c(awl=5.88, spw=29.6, const=15.8), ...){
  # combine parameters
  param.list <- list(ecp=ecp, grade=grade, short=short)
  if(is.list(txt.file)){
    results <- readability.num(txt.features=txt.file, index="Coleman.Liau", parameters=list(Coleman.Liau=param.list), ...)
  } else {
    results <- readability(txt.file=txt.file, index="Coleman.Liau", parameters=list(Coleman.Liau=param.list), ...)
  }
  return(results)
}
## end coleman.liau()


## coleman()
#' Readability: Coleman's Formulas
#' 
#' This is just a convenient wrapper function for \code{\link[koRpus:readability]{readability}}.
#'
#' This function calculates the four readability formulas by Coleman. In contrast to
#' \code{\link[koRpus:readability]{readability}}, which by default calculates all possible
#' indices, this function will only calculate the index value.
#'
#' @param txt.file Either an object of class \code{\link[koRpus:kRp.text-class]{kRp.text}}, a character vector which must be be
#'    a valid path to a file containing the text to be analyzed, or a list of text features. If the latter, calculation
#'    is done by \code{\link[koRpus:readability.num]{readability.num}}. 
#' @param hyphen An object of class kRp.hyphen. If \code{NULL}, the text will be hyphenated automatically.
#' @param parameters A numeric vector with named magic numbers, defining the relevant parameters for all formulas of the index.
#' @param clz1 A numeric vector with named magic numbers for the first formula.
#' @param clz2 A numeric vector with named magic numbers for the second formula.
#' @param clz3 A numeric vector with named magic numbers for the third formula.
#' @param clz4 A numeric vector with named magic numbers for the fourth formula.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:readability]{readability}} for details.
#' @return An object of class \code{\link[koRpus:kRp.readability-class]{kRp.readability}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords readability
#' @export
#' @examples
#' \dontrun{
#' coleman(tagged.text)
#' }
coleman <- function(txt.file, hyphen=NULL,
    parameters=c(syll=1),
    clz1=c(word=1.29, const=38.45),
    clz2=c(word=1.16, sntc=1.48, const=37.95),
    clz3=c(word=1.07, sntc=1.18, pron=0.76, const=34.02),
    clz4=c(word=1.04, sntc=1.06, pron=0.56, prep=0.36, const=26.01), ...){
  all.parameters <- list(syll=parameters[["syll"]], clz1=clz1, clz2=clz2,  clz3=clz3,  clz4=clz4)
  if(is.list(txt.file)){
    results <- readability.num(txt.features=txt.file, index="Coleman", parameters=list(Coleman=all.parameters), ...)
  } else {
    results <- readability(txt.file=txt.file, hyphen=hyphen, index="Coleman", parameters=list(Coleman=all.parameters), ...)
  }
  return(results)
}
## end coleman()


## dale.chall()
#' Readability: Dale-Chall Readability Formula
#' 
#' This is just a convenient wrapper function for \code{\link[koRpus:readability]{readability}}.
#'
#' Calculates the New Dale-Chall Readability Formula. In contrast to \code{\link[koRpus:readability]{readability}},
#' which by default calculates all possible indices, this function will only calculate the index value.
#'
#' If \code{parameters="PSK"}, the parameters by Powers-Sumner-Kearl (1958) are used, and if
#' \code{parameters="old"}, the original parameters by Dale-Chall (1948), respectively.
#'
#' This formula doesn't need syllable count.
#'
#' @param txt.file Either an object of class \code{\link[koRpus:kRp.text-class]{kRp.text}}, a character vector which must be be
#'    a valid path to a file containing the text to be analyzed, or a list of text features. If the latter, calculation
#'    is done by \code{\link[koRpus:readability.num]{readability.num}}. 
#' @param word.list A vector or matrix (with exactly one column) which defines familiar words. For valid results
#'    the long Dale-Chall list with about 3000 words should be used.
#' @param parameters A numeric vector with named magic numbers, defining the relevant parameters for the index.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:readability]{readability}} for details.
#' @return An object of class \code{\link[koRpus:kRp.readability-class]{kRp.readability}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords readability
#' @export
#' @examples
#' \dontrun{
#' dale.chall(tagged.text, word.list=new.dale.chall.wl)
#' }

dale.chall <- function(txt.file, word.list, parameters=c(const=64, dword=0.95, asl=0.69), ...){
  # combine parameters
  if(identical(parameters, "PSK")){
    param.list <- "PSK"
  } else if(identical(parameters, "old")){
    param.list <- "old"
  } else {
    param.list <- list(dword=parameters[["dword"]], asl=parameters[["asl"]], const=parameters[["const"]])
  }
  if(is.list(txt.file)){
    results <- readability.num(txt.features=txt.file, index="Dale.Chall", parameters=list(Dale.Chall=param.list), ...)
  } else {
    results <- readability(txt.file=txt.file, index="Dale.Chall", parameters=list(Dale.Chall=param.list), word.lists=list(Dale.Chall=word.list), ...)
  }
  return(results)
}
## end dale.chall()


## danielson.bryan()
#' Readability: Danielson-Bryan
#' 
#' This is just a convenient wrapper function for \code{\link[koRpus:readability]{readability}}.
#'
#' Calculates the two Danielson-Bryan formulas. In contrast to
#' \code{\link[koRpus:readability]{readability}}, which by default calculates all possible indices,
#' this function will only calculate the index value.
#'
#' This formula doesn't need syllable count.
#'
#' @param txt.file Either an object of class \code{\link[koRpus:kRp.text-class]{kRp.text}}, a character vector which must be be
#'    a valid path to a file containing the text to be analyzed, or a list of text features. If the latter, calculation
#'    is done by \code{\link[koRpus:readability.num]{readability.num}}. 
#' @param db1 A numeric vector with named magic numbers, defining the relevant parameters for the first formula (regression).
#' @param db2 A numeric vector with named magic numbers, defining the relevant parameters for the second formula (cloze equivalent).
#' @param ... Further valid options for the main function, see \code{\link[koRpus:readability]{readability}} for details.
#' @return An object of class \code{\link[koRpus:kRp.readability-class]{kRp.readability}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords readability
#' @export
#' @examples
#' \dontrun{
#'   danielson.bryan(tagged.text)
#' }

danielson.bryan <- function(txt.file, db1=c(cpb=1.0364, cps=0.0194, const=0.6059),
      db2=c(const=131.059, cpb=10.364, cps=0.194), ...){
  param.list <- list(db1=db1, db2=db2)
  if(is.list(txt.file)){
    results <- readability.num(txt.features=txt.file, index="Danielson.Bryan", parameters=list(Danielson.Bryan=param.list), ...)
  } else {
    results <- readability(txt.file=txt.file, index="Danielson.Bryan", parameters=list(Danielson.Bryan=param.list), ...)
  }
  return(results)
}
## end danielson.bryan()


## dickes.steiwer()
#' Readability: Dickes-Steiwer Handformel
#' 
#' This is just a convenient wrapper function for \code{\link[koRpus:readability]{readability}}.
#'
#' This function calculates the shortcut formula by Dickes-Steiwer. In contrast to
#' \code{\link[koRpus:readability]{readability}}, which by default calculates all possible indices,
#' this function will only calculate the index value.
#'
#' This formula doesn't need syllable count.
#'
#' @param txt.file Either an object of class \code{\link[koRpus:kRp.text-class]{kRp.text}}, a character vector which must be be
#'    a valid path to a file containing the text to be analyzed, or a list of text features. If the latter, calculation
#'    is done by \code{\link[koRpus:readability.num]{readability.num}}. 
#' @param parameters A numeric vector with named magic numbers, defining the relevant parameters for the index.
#' @param case.sens Logical, whether types should be counted case sensitive.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:readability]{readability}} for details.
#' @return An object of class \code{\link[koRpus:kRp.readability-class]{kRp.readability}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords readability
#' @export
#' @examples
#' \dontrun{
#'   dickes.steiwer(tagged.text)
#' }

dickes.steiwer <- function(txt.file, parameters=c(const=235.95993, awl=73.021, asl=12.56438, ttr=50.03293), case.sens=FALSE, ...){
  all.parameters <- list(const=parameters[["const"]], awl=parameters[["awl"]], asl=parameters[["asl"]], ttr=parameters[["ttr"]], case.sens=case.sens)
  if(is.list(txt.file)){
    results <- readability.num(txt.features=txt.file, index="Dickes.Steiwer", parameters=list(Dickes.Steiwer=all.parameters), ...)
  } else {
    results <- readability(txt.file=txt.file, index="Dickes.Steiwer", parameters=list(Dickes.Steiwer=all.parameters), ...)
  }
  return(results)
}
## end dickes.steiwer()


## DRP()
#' Readability: Degrees of Reading Power (DRP)
#' 
#' This is just a convenient wrapper function for \code{\link[koRpus:readability]{readability}}.
#'
#' Calculates the Degrees of Reading Power, using the Bormuth Mean Cloze Score. In contrast to
#' \code{\link[koRpus:readability]{readability}}, which by default calculates all possible indices,
#' this function will only calculate the index value.
#'
#' This formula doesn't need syllable count.
#'
#' @param txt.file Either an object of class \code{\link[koRpus:kRp.text-class]{kRp.text}}, a character vector which must be be
#'    a valid path to a file containing the text to be analyzed, or a list of text features. If the latter, calculation
#'    is done by \code{\link[koRpus:readability.num]{readability.num}}. 
#' @param word.list A vector or matrix (with exactly one column) which defines familiar words. For valid results
#'    the long Dale-Chall list with 3000 words should be used.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:readability]{readability}} for details.
#' @return An object of class \code{\link[koRpus:kRp.readability-class]{kRp.readability}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords readability
#' @export
#' @examples
#' \dontrun{
#'   DRP(tagged.text, word.list=new.dale.chall.wl)
#' }

DRP <- function(txt.file, word.list, ...){
  if(is.list(txt.file)){
    results <- readability.num(txt.features=txt.file, index="DRP", ...)
  } else {
    results <- readability(txt.file=txt.file, index="DRP", word.lists=list(Bormuth=word.list), ...)
  }
  return(results)
}
## end DRP()


## ELF()
#' Readability: Fang's Easy Listening Formula (ELF)
#' 
#' This is just a convenient wrapper function for \code{\link[koRpus:readability]{readability}}.
#'
#' This function calculates Fang's Easy Listening Formula (ELF). In contrast to
#' \code{\link[koRpus:readability]{readability}}, which by default calculates all possible indices,
#' this function will only calculate the index value.
#'
#' @param txt.file Either an object of class \code{\link[koRpus:kRp.text-class]{kRp.text}}, a character vector which must be be
#'    a valid path to a file containing the text to be analyzed, or a list of text features. If the latter, calculation
#'    is done by \code{\link[koRpus:readability.num]{readability.num}}.
#' @param hyphen An object of class kRp.hyphen. If \code{NULL}, the text will be hyphenated automatically.
#' @param parameters A numeric vector with named magic numbers, defining the relevant parameters for the index.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:readability]{readability}} for details.
#' @return An object of class \code{\link[koRpus:kRp.readability-class]{kRp.readability}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @references
#'    DuBay, W.H. (2004). \emph{The Principles of Readability}. Costa Mesa: Impact Information.
#'      WWW: \url{http://www.impact-information.com/impactinfo/readability02.pdf}; 22.03.2011.
#' @keywords readability
#' @export
#' @examples
#' \dontrun{
#'   ELF(tagged.text)
#' }

ELF <- function(txt.file, hyphen=NULL, parameters=c(syll=1), ...){
  if(is.list(txt.file)){
    results <- readability.num(txt.features=txt.file, index="ELF", parameters=list(ELF=parameters), ...)
  } else {
    results <- readability(txt.file=txt.file, hyphen=hyphen, index="ELF", parameters=list(ELF=parameters), ...)
  }
  return(results)
}
## end ELF()


## farr.jenkins.paterson()
#' Readability: Farr-Jenkins-Paterson Index
#' 
#' This is just a convenient wrapper function for \code{\link[koRpus:readability]{readability}}.
#'
#' Calculates the Farr-Jenkins-Paterson index, a simplified version of Flesch Reading Ease.
#' In contrast to \code{\link[koRpus:readability]{readability}},
#' which by default calculates all possible indices, this function will only calculate the index value.
#'
#' If \code{parameters="PSK"}, the revised parameters by Powers-Sumner-Kearl (1958) are used.
#'
#' @param txt.file Either an object of class \code{\link[koRpus:kRp.text-class]{kRp.text}}, a character vector which must be be
#'    a valid path to a file containing the text to be analyzed, or a list of text features. If the latter, calculation
#'    is done by \code{\link[koRpus:readability.num]{readability.num}}. 
#' @param hyphen An object of class kRp.hyphen. If \code{NULL}, the text will be hyphenated automatically.
#' @param parameters A numeric vector with named magic numbers, defining the relevant parameters for the index, or \code{"PSK"}.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:readability]{readability}} for details.
#' @return An object of class \code{\link[koRpus:kRp.readability-class]{kRp.readability}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @references
#'    Farr, J.N., Jenkins, J.J. & Paterson, D.G. (1951). Simplification of Flesch Reading Ease formula. \emph{Journal of Applied Psychology}, 35(5), 333--337.
#'
#'    Powers, R.D, Sumner, W.A, & Kearl, B.E. (1958). A recalculation of four adult readability formulas, \emph{Journal of Educational Psychology}, 49(2), 99--105.
#' @keywords readability
#' @seealso \code{\link[koRpus:flesch]{flesch}}
#' @export
#' @examples
#' \dontrun{
#' farr.jenkins.paterson(tagged.text)
#' }

farr.jenkins.paterson <- function(txt.file, hyphen=NULL, parameters=c(const=-31.517, asl=1.015, monsy=1.599), ...){
  if(is.list(txt.file)){
    results <- readability.num(txt.features=txt.file, index="Farr.Jenkins.Paterson", parameters=list(Farr.Jenkins.Paterson=parameters), ...)
  } else {
    results <- readability(txt.file=txt.file, hyphen=hyphen, index="Farr.Jenkins.Paterson", parameters=list(Farr.Jenkins.Paterson=parameters), ...)
  }
  return(results)
}
## end farr.jenkins.paterson()


## flesch.kincaid()
#' Readability: Flesch-Kincaid Grade Level
#' 
#' This is just a convenient wrapper function for \code{\link[koRpus:readability]{readability}}.
#'
#' Calculates the Flesch-Kincaid grade level. In contrast to \code{\link[koRpus:readability]{readability}},
#' which by default calculates all possible indices, this function will only calculate the index value.
#'
#' @param txt.file Either an object of class \code{\link[koRpus:kRp.text-class]{kRp.text}}, a character vector which must be be
#'    a valid path to a file containing the text to be analyzed, or a list of text features. If the latter, calculation
#'    is done by \code{\link[koRpus:readability.num]{readability.num}}. 
#' @param hyphen An object of class kRp.hyphen. If \code{NULL}, the text will be hyphenated automatically.
#' @param parameters A numeric vector with named magic numbers, defining the relevant parameters for the index.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:readability]{readability}} for details.
#' @return An object of class \code{\link[koRpus:kRp.readability-class]{kRp.readability}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords readability
#' @export
#' @examples
#' \dontrun{
#' flesch.kincaid(tagged.text)
#' }

flesch.kincaid <- function(txt.file, hyphen=NULL, parameters=c(asl=0.39, asw=11.8, const=15.59), ...){
  if(is.list(txt.file)){
    results <- readability.num(txt.features=txt.file, index="Flesch.Kincaid", parameters=list(Flesch.Kincaid=parameters), ...)
  } else {
    results <- readability(txt.file=txt.file, hyphen=hyphen, index="Flesch.Kincaid", parameters=list(Flesch.Kincaid=parameters), ...)
  }
  return(results)
}
## end flesch.kincaid()


## flesch()
#' Readability: Flesch Readability Ease
#' 
#' This is just a convenient wrapper function for \code{\link[koRpus:readability]{readability}}.
#'
#' Calculates the Flesch Readability Ease index. In contrast to
#' \code{\link[koRpus:readability]{readability}}, which by default calculates all possible indices,
#' this function will only calculate the Flesch RE value.
#'
#' Certain internationalisations of the parameters are also implemented. They can be used by setting
#' \code{parameters} to \code{"es"} (Fernandez-Huerta),  \code{"es-s"} (Szigriszt), \code{"nl"} (Douma),
#' \code{"nl-b"} (Brouwer), \code{"de"} (Amstad) or \code{"fr"} (Kandel-Moles).
#' If \code{parameters="PSK"}, the revised parameters by Powers-Sumner-Kearl (1958) are used
#' to calculate a grade level.
#'
#' @param txt.file Either an object of class \code{\link[koRpus:kRp.text-class]{kRp.text}}, a character vector which must be be
#'    a valid path to a file containing the text to be analyzed, or a list of text features. If the latter, calculation
#'    is done by \code{\link[koRpus:readability.num]{readability.num}}. 
#' @param hyphen An object of class kRp.hyphen. If \code{NULL}, the text will be hyphenated automatically.
#' @param parameters Either a numeric vector with named magic numbers, defining the relevant parameters for the index, or
#'    a valid character string naming a preset for implemented languages (\code{"de"}, \code{"es"}, \code{"es-s"},
#'    \code{"nl"}, \code{"nl-b"}, \code{"fr"}).
#' @param ... Further valid options for the main function, see \code{\link[koRpus:readability]{readability}} for details.
#' @return An object of class \code{\link[koRpus:kRp.readability-class]{kRp.readability}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords readability
#' @seealso \code{\link[koRpus:flesch.kincaid]{flesch.kincaid}} for grade levels,
#' \code{\link[koRpus:farr.jenkins.paterson]{farr.jenkins.paterson}} for a simplified Flesch formula.
#' @export
#' @examples
#' \dontrun{
#' flesch(german.tagged.text, parameters="de")
#' }

flesch <- function(txt.file, hyphen=NULL, parameters=c(const=206.835, asl=1.015, asw=84.6), ...){
  if(is.list(txt.file)){
    results <- readability.num(txt.features=txt.file, index="Flesch", parameters=list(Flesch=parameters), ...)
  } else {
    results <- readability(txt.file=txt.file, hyphen=hyphen, index="Flesch", parameters=list(Flesch=parameters), ...)
  }
  return(results)
}
## end flesch()


## FOG()
#' Readability: Gunning FOG Index
#' 
#' This is just a convenient wrapper function for \code{\link[koRpus:readability]{readability}}.
#'
#' Calculates the Gunning FOG index. In contrast to \code{\link[koRpus:readability]{readability}},
#' which by default calculates all possible indices, this function will only calculate the index value.
#'
#' If \code{parameters="PSK"}, the revised parameters by Powers-Sumner-Kearl (1958) are used, and
#' if \code{parameters="NRI"}, the simplified parameters from the Navy Readability Indexes, respectively.
#'
#' @param txt.file Either an object of class \code{\link[koRpus:kRp.text-class]{kRp.text}}, a character vector which must be be
#'    a valid path to a file containing the text to be analyzed, or a list of text features. If the latter, calculation
#'    is done by \code{\link[koRpus:readability.num]{readability.num}}. 
#' @param hyphen An object of class kRp.hyphen. If \code{NULL}, the text will be hyphenated automatically.
#' @param parameters A list with named magic numbers and a vector with verb suffixes, defining the relevant parameters for the index,
#'    or one of \code{"PSK"} or \code{"NRI"}.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:readability]{readability}} for details.
#' @return An object of class \code{\link[koRpus:kRp.readability-class]{kRp.readability}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @references
#'    DuBay, W.H. (2004). \emph{The Principles of Readability}. Costa Mesa: Impact Information.
#'      WWW: \url{http://www.impact-information.com/impactinfo/readability02.pdf}; 22.03.2011.
#'
#'    Powers, R.D, Sumner, W.A, & Kearl, B.E. (1958). A recalculation of four adult readability formulas,
#'      \emph{Journal of Educational Psychology}, 49(2), 99--105.
#' @keywords readability
#' @export
#' @examples
#' \dontrun{
#' FOG(tagged.text)
#' }

FOG <- function(txt.file, hyphen=NULL, parameters=list(syll=3, const=0.4, suffix=c("es", "ed", "ing")), ...){
  if(is.list(txt.file)){
    results <- readability.num(txt.features=txt.file, index="FOG", parameters=list(FOG=parameters), ...)
  } else {
    results <- readability(txt.file=txt.file, hyphen=hyphen, index="FOG", parameters=list(FOG=parameters), ...)
  }
  return(results)
}
## end FOG()


## FORCAST()
#' Readability: FORCAST Index
#' 
#' This is just a convenient wrapper function for \code{\link[koRpus:readability]{readability}}.
#'
#' Calculates the FORCAST index (both grade level and reading age). In contrast to \code{\link[koRpus:readability]{readability}},
#' which by default calculates all possible indices, this function will only calculate the index value.
#'
#' If \code{parameters="RGL"}, the parameters for the precise Reading Grade Level are used.
#'
#' @param txt.file Either an object of class \code{\link[koRpus:kRp.text-class]{kRp.text}}, a character vector which must be be
#'    a valid path to a file containing the text to be analyzed, or a list of text features. If the latter, calculation
#'    is done by \code{\link[koRpus:readability.num]{readability.num}}. 
#' @param hyphen An object of class kRp.hyphen. If \code{NULL}, the text will be hyphenated automatically.
#' @param parameters A numeric vector with named magic numbers, defining the relevant parameters for the index, or \code{"RGL"}.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:readability]{readability}} for details.
#' @return An object of class \code{\link[koRpus:kRp.readability-class]{kRp.readability}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords readability
#' @references
#'    Klare, G.R. (1975). Assessing readability. \emph{Reading Research Quarterly}, 10(1), 62--102.
#' @export
#' @examples
#' \dontrun{
#' FORCAST(tagged.text)
#' }

FORCAST <- function(txt.file, hyphen=NULL, parameters=c(syll=1, mult=.10, const=20), ...){
  if(is.list(txt.file)){
    results <- readability.num(txt.features=txt.file, index="FORCAST", parameters=list(FORCAST=parameters), ...)
  } else {
    results <- readability(txt.file=txt.file, hyphen=hyphen, index="FORCAST", parameters=list(FORCAST=parameters), ...)
  }
  return(results)
}
## end FORCAST()


## fucks()
#' Readability: Fucks' Stilcharakteristik
#' 
#' This is just a convenient wrapper function for \code{\link[koRpus:readability]{readability}}.
#'
#' Calculates Fucks' Stilcharakteristik ("characteristics of style"; Fucks, 1955, as cited in Briest, 1974).
#' In contrast to \code{\link[koRpus:readability]{readability}}, which by default calculates all possible indices,
#' this function will only calculate the index value.
#'
#' @param txt.file Either an object of class \code{\link[koRpus:kRp.text-class]{kRp.text}}, a character vector which must be be
#'    a valid path to a file containing the text to be analyzed, or a list of text features. If the latter, calculation
#'    is done by \code{\link[koRpus:readability.num]{readability.num}}. 
#' @param ... Further valid options for the main function, see \code{\link[koRpus:readability]{readability}} for details.
#' @return An object of class \code{\link[koRpus:kRp.readability-class]{kRp.readability}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @references
#'    Briest, W. (1974). Kann man Verständlichkeit messen? \emph{Zeitschrift für Phonetik, Sprachwissenschaft und Kommunikationsforschung}, 27, 543--563.
#' @keywords readability
#' @export
#' @examples
#' \dontrun{
#'   fucks(tagged.text)
#' }

fucks <- function(txt.file, ...){
  if(is.list(txt.file)){
    results <- readability.num(txt.features=txt.file, index="Fucks", ...)
  } else {
    results <- readability(txt.file=txt.file, index="Fucks", ...)
  }
  return(results)
}
## end fucks()


## harris.jacobson()
#' Readability: Harris-Jacobson indices
#'
#' This is just a convenient wrapper function for \code{\link[koRpus:readability]{readability}}.
#'
#' This function calculates the revised Harris-Jacobson readability formulas (1 to 5), as described in their paper for the
#' 18th Annual Meeting of the College Reading Association (Harris & Jacobson, 1974). In contrast to \code{\link[koRpus:readability]{readability}},
#' which by default calculates all possible indices, this function will only calculate the index values.
#'
#' This formula doesn't need syllable count.
#'
#' @param txt.file Either an object of class \code{\link[koRpus:kRp.text-class]{kRp.text}}, a character vector which must be be
#'    a valid path to a file containing the text to be analyzed, or a list of text features. If the latter, calculation
#'    is done by \code{\link[koRpus:readability.num]{readability.num}}. 
#' @param word.list A vector or matrix (with exactly one column) which defines familiar words. For valid results
#'    the short Harris-Jacobson word list for grades 1 and 2 (english) should be used.
#' @param parameters A numeric vector with named magic numbers, defining the relevant parameters for all formulas of the index.
#' @param hj1 A numeric vector with named magic numbers for the first of the formulas.
#' @param hj2 A numeric vector with named magic numbers for the second of the formulas.
#' @param hj3 A numeric vector with named magic numbers for the third of the formulas.
#' @param hj4 A numeric vector with named magic numbers for the fourth of the formulas.
#' @param hj5 A numeric vector with named magic numbers for the fifth of the formulas.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:readability]{readability}} for details.
#' @return An object of class \code{\link[koRpus:kRp.readability-class]{kRp.readability}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @references
#'    Harris, A.J. & Jacobson, M.D. (1974). Revised Harris-Jacobson readability formulas. In \emph{18th Annual Meeting of the College Reading Association}, Bethesda.
#' @keywords readability
#' @export
#' @examples
#' \dontrun{
#' harris.jacobson(tagged.text, word.list=harris.jacobson.wl)
#' }

harris.jacobson <- function(txt.file, word.list,
  parameters=c(char=6),
  hj1=c(dword=0.094, asl=0.168, const=0.502),
  hj2=c(dword=0.140, asl=0.153, const=0.560),
  hj3=c(asl=0.158, lword=0.055, const=0.355),
  hj4=c(dword=0.070, asl=0.125, lword=0.037, const=0.497),
  hj5=c(dword=0.118, asl=0.134, lword=0.032, const=0.424), ...){
  all.parameters <- list(char=parameters[["char"]], hj1=hj1, hj2=hj2,  hj3=hj3,  hj4=hj4,  hj5=hj5)
  if(is.list(txt.file)){
    results <- readability.num(txt.features=txt.file, index="Harris.Jacobson", parameters=list(Harris.Jacobson=all.parameters), ...)
  } else {
    results <- readability(txt.file=txt.file, hyphen=hyphen, index="Harris.Jacobson", parameters=list(Harris.Jacobson=all.parameters), word.lists=list(Harris.Jacobson=word.list), ...)
  }
  return(results)
}
## end harris.jacobson()


## linsear.write()
#' Readability: Linsear Write Index
#' 
#' This is just a convenient wrapper function for \code{\link[koRpus:readability]{readability}}.
#'
#' This function calculates the Linsear Write index. In contrast to \code{\link[koRpus:readability]{readability}},
#' which by default calculates all possible indices, this function will only calculate the index value.
#'
#' @param txt.file Either an object of class \code{\link[koRpus:kRp.text-class]{kRp.text}}, a character vector which must be be
#'    a valid path to a file containing the text to be analyzed, or a list of text features. If the latter, calculation
#'    is done by \code{\link[koRpus:readability.num]{readability.num}}. 
#' @param hyphen An object of class kRp.hyphen. If \code{NULL}, the text will be hyphenated automatically.
#' @param parameters A numeric vector with named magic numbers, defining the relevant parameters for the index.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:readability]{readability}} for details.
#' @return An object of class \code{\link[koRpus:kRp.readability-class]{kRp.readability}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords readability
#' @export
#' @examples
#' \dontrun{
#' linsear.write(tagged.text)
#' }

linsear.write <- function(txt.file, hyphen=NULL, parameters=c(short.syll=2, long.syll=3, thrs=20), ...){
  if(is.list(txt.file)){
    results <- readability.num(txt.features=txt.file, index="Linsear.Write", parameters=list(Linsear.Write=parameters), ...)
  } else {
    results <- readability(txt.file=txt.file, hyphen=hyphen, index="Linsear.Write", parameters=list(Linsear.Write=parameters), ...)
  }
  return(results)
}
## end linsear.write()


## LIX()
#' Readability: Bj\"ornsson's L\"asbarhetsindex (LIX)
#' 
#' This is just a convenient wrapper function for \code{\link[koRpus:readability]{readability}}.
#'
#' This function calculates the readability index ("l\"asbarhetsindex") by Bj\"ornsson. In contrast to \code{\link[koRpus:readability]{readability}},
#' which by default calculates all possible indices, this function will only calculate the index value.
#'
#' This formula doesn't need syllable count.
#'
#' @param txt.file Either an object of class \code{\link[koRpus:kRp.text-class]{kRp.text}}, a character vector which must be be
#'    a valid path to a file containing the text to be analyzed, or a list of text features. If the latter, calculation
#'    is done by \code{\link[koRpus:readability.num]{readability.num}}. 
#' @param parameters A numeric vector with named magic numbers, defining the relevant parameters for the index.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:readability]{readability}} for details.
#' @return An object of class \code{\link[koRpus:kRp.readability-class]{kRp.readability}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @references
#'    Anderson, J. (1981). Analysing the readability of english and non-english texts in the classroom with Lix. In
#'      \emph{Annual Meeting of the Australian Reading Association}, Darwin, Australia.
#'
#'    Anderson, J. (1983). Lix and Rix: Variations on a little-known readability index. \emph{Journal of Reading}, 26(6), 490--496.
#' @keywords readability
#' @export
#' @examples
#' \dontrun{
#'   LIX(tagged.text)
#' }

LIX <- function(txt.file, parameters=c(char=6, const=100), ...){
  if(is.list(txt.file)){
    results <- readability.num(txt.features=txt.file, index="LIX", parameters=list(LIX=parameters), ...)
  } else {
    results <- readability(txt.file=txt.file, index="LIX", parameters=list(LIX=parameters), ...)
  }
  return(results)
}
## end LIX()


## nWS()
#' Readability: Neue Wiener Sachtextformeln
#'
#' This is just a convenient wrapper function for \code{\link[koRpus:readability]{readability}}.
#'
#' This function calculates the new Wiener Sachtextformeln (formulas 1 to 4). In contrast to \code{\link[koRpus:readability]{readability}},
#' which by default calculates all possible indices, this function will only calculate the index values.
#'
#' @param txt.file Either an object of class \code{\link[koRpus:kRp.text-class]{kRp.text}}, a character vector which must be be
#'    a valid path to a file containing the text to be analyzed, or a list of text features. If the latter, calculation
#'    is done by \code{\link[koRpus:readability.num]{readability.num}}. 
#' @param hyphen An object of class kRp.hyphen. If \code{NULL}, the text will be hyphenated automatically.
#' @param parameters A numeric vector with named magic numbers, defining the relevant parameters for all formulas of the index.
#' @param nws1 A numeric vector with named magic numbers for the first of the formulas.
#' @param nws2 A numeric vector with named magic numbers for the second of the formulas.
#' @param nws3 A numeric vector with named magic numbers for the third of the formulas.
#' @param nws4 A numeric vector with named magic numbers for the fourth of the formulas.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:readability]{readability}} for details.
#' @return An object of class \code{\link[koRpus:kRp.readability-class]{kRp.readability}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @references
#'    Bamberger, R. & Vanecek, E. (1984). \emph{Lesen--Verstehen--Lernen--Schreiben}. Wien: Jugend und Volk.
#' @keywords readability
#' @export
#' @examples
#' \dontrun{
#' nWS(tagged.text)
#' }

nWS <- function(txt.file, hyphen=NULL,
  parameters=c(ms.syll=3, iw.char=6, es.syll=1),
  nws1=c(ms=19.35, sl=0.1672, iw=12.97, es=3.27, const=0.875),
  nws2=c(ms=20.07, sl=0.1682, iw=13.73, const=2.779),
  nws3=c(ms=29.63, sl=0.1905, const=1.1144),
  nws4=c(ms=27.44, sl=0.2656, const=1.693), ...){
  all.parameters <- list(ms.syll=parameters[["ms.syll"]], iw.char=parameters[["iw.char"]], es.syll=parameters[["es.syll"]], nws1=nws1, nws2=nws2,  nws3=nws3,  nws4=nws4)
  if(is.list(txt.file)){
    results <- readability.num(txt.features=txt.file, index="nWS", parameters=list(nWS=all.parameters), ...)
  } else {
    results <- readability(txt.file=txt.file, hyphen=hyphen, index="nWS", parameters=list(nWS=all.parameters), ...)
  }
  return(results)
}
## end nWS()


## RIX()
#' Readability: Anderson's Readability Index (RIX)
#'
#' This is just a convenient wrapper function for \code{\link[koRpus:readability]{readability}}.
#'
#' This function calculates the Readability Index (RIX) by Anderson, which is a simplified version of the l\"asbarhetsindex (LIX)
#' by Bj\"ornsson. In contrast to \code{\link[koRpus:readability]{readability}},
#' which by default calculates all possible indices, this function will only calculate the index value.
#'
#' This formula doesn't need syllable count.
#'
#' @param txt.file Either an object of class \code{\link[koRpus:kRp.text-class]{kRp.text}}, a character vector which must be be
#'    a valid path to a file containing the text to be analyzed, or a list of text features. If the latter, calculation
#'    is done by \code{\link[koRpus:readability.num]{readability.num}}. 
#' @param parameters A numeric vector with named magic numbers, defining the relevant parameters for the index.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:readability]{readability}} for details.
#' @return An object of class \code{\link[koRpus:kRp.readability-class]{kRp.readability}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @references
#'    Anderson, J. (1981). Analysing the readability of english and non-english texts in the classroom with Lix. In
#'      \emph{Annual Meeting of the Australian Reading Association}, Darwin, Australia.
#'
#'    Anderson, J. (1983). Lix and Rix: Variations on a little-known readability index. \emph{Journal of Reading}, 26(6), 490--496.
#' @keywords readability
#' @export
#' @examples
#' \dontrun{
#'   RIX(tagged.text)
#' }

RIX <- function(txt.file, parameters=c(char=6), ...){
  if(is.list(txt.file)){
    results <- readability.num(txt.features=txt.file, index="RIX", parameters=list(RIX=parameters), ...)
  } else {
    results <- readability(txt.file=txt.file, index="RIX", parameters=list(RIX=parameters), ...)
  }
  return(results)
}
## end RIX()


## SMOG()
#' Readability: Simple Measure of Gobbledygook (SMOG)
#'
#' This is just a convenient wrapper function for \code{\link[koRpus:readability]{readability}}.
#'
#' This function calculates the Simple Measure of Gobbledygook (SMOG). In contrast to \code{\link[koRpus:readability]{readability}},
#' which by default calculates all possible indices, this function will only calculate the index value.
#'
#' By default calculates formula D by McLaughlin (1969).
#' If \code{parameters} is set to \code{SMOG="C"}, formula C will be calculated.
#' If \code{parameters} is set to \code{SMOG="simple"}, the simplified formula is used, and
#' if \code{parameters="de"}, the formula adapted to German texts ("Qu", Bamberger & Vanecek, 1984, p. 78).
#'
#' @param txt.file Either an object of class \code{\link[koRpus:kRp.text-class]{kRp.text}}, a character vector which must be be
#'    a valid path to a file containing the text to be analyzed, or a list of text features. If the latter, calculation
#'    is done by \code{\link[koRpus:readability.num]{readability.num}}. 
#' @param hyphen An object of class kRp.hyphen. If \code{NULL}, the text will be hyphenated automatically.
#' @param parameters A numeric vector with named magic numbers, defining the relevant parameters for the index.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:readability]{readability}} for details.
#' @return An object of class \code{\link[koRpus:kRp.readability-class]{kRp.readability}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @references
#'    Bamberger, R. & Vanecek, E. (1984). \emph{Lesen--Verstehen--Lernen--Schreiben}. Wien: Jugend und Volk.
#'
#'    McLaughlin, G.H. (1969). SMOG grading -- A new readability formula. \emph{Journal of Reading}, 12(8), 639--646.
#' @keywords readability
#' @export
#' @examples
#' \dontrun{
#' SMOG(tagged.text)
#' }

SMOG <- function(txt.file, hyphen=NULL, parameters=c(syll=3, sqrt=1.043, fact=30, const=3.1291, sqrt.const=0), ...){
  if(is.list(txt.file)){
    results <- readability.num(txt.features=txt.file, index="SMOG", parameters=list(SMOG=parameters), ...)
  } else {
    results <- readability(txt.file=txt.file, hyphen=hyphen, index="SMOG", parameters=list(SMOG=parameters), ...)
  }
  return(results)
}
## end SMOG()


## spache()
#' Readability: Spache Formula
#'
#' This is just a convenient wrapper function for \code{\link[koRpus:readability]{readability}}.
#'
#' Calculates the Spache Formula. In contrast to \code{\link[koRpus:readability]{readability}},
#' which by default calculates all possible indices, this function will only calculate the index value.
#'
#' By default the revised Spache formula is calculated. If \code{parameters="old"}, the original
#' parameters are used.
#'
#' This formula doesn't need syllable count.
#'
#' @param txt.file Either an object of class \code{\link[koRpus:kRp.text-class]{kRp.text}}, a character vector which must be be
#'    a valid path to a file containing the text to be analyzed, or a list of text features. If the latter, calculation
#'    is done by \code{\link[koRpus:readability.num]{readability.num}}. 
#' @param word.list A vector or matrix (with exactly one column) which defines familiar words. For valid results
#'    the short Dale-Chall list with 769 easy words should be used.
#' @param parameters A numeric vector with named magic numbers, defining the relevant parameters for the index.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:readability]{readability}} for details.
#' @return An object of class \code{\link[koRpus:kRp.readability-class]{kRp.readability}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords readability
#' @export
#' @examples
#' \dontrun{
#' spache(tagged.text, word.list=spache.revised.wl)
#' }

spache <- function(txt.file, word.list, parameters=c(asl=0.121, dword=0.082, const=0.659), ...){
  # combine parameters
  if(identical(parameters, "old")){
    param.list <- "old"
  } else {
    param.list <- list(asl=parameters[["asl"]], dword=parameters[["dword"]], const=parameters[["const"]])
  }
  if(is.list(txt.file)){
    results <- readability.num(txt.features=txt.file, index="Spache", parameters=list(Spache=param.list), ...)
  } else {
    results <- readability(txt.file=txt.file, index="Spache", parameters=list(Spache=param.list), word.lists=list(Spache=word.list), ...)
  }
  return(results)
}
## end spache()


## strain()
#' Readability: Strain Index
#'
#' This is just a convenient wrapper function for \code{\link[koRpus:readability]{readability}}.
#'
#' This function calculates the Strain index. In contrast to \code{\link[koRpus:readability]{readability}},
#' which by default calculates all possible indices, this function will only calculate the index value.
#'
#' @param txt.file Either an object of class \code{\link[koRpus:kRp.text-class]{kRp.text}}, a character vector which must be be
#'    a valid path to a file containing the text to be analyzed, or a list of text features. If the latter, calculation
#'    is done by \code{\link[koRpus:readability.num]{readability.num}}. 
#' @param hyphen An object of class kRp.hyphen. If \code{NULL}, the text will be hyphenated automatically.
#' @param parameters A numeric vector with named magic numbers, defining the relevant parameters for the index.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:readability]{readability}} for details.
#' @return An object of class \code{\link[koRpus:kRp.readability-class]{kRp.readability}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords readability
#' @export
#' @examples
#' \dontrun{
#' strain(tagged.text)
#' }

strain <- function(txt.file, hyphen=NULL, parameters=c(sent=3, const=10), ...){
  if(is.list(txt.file)){
    results <- readability.num(txt.features=txt.file, index="Strain", parameters=list(Strain=parameters), ...)
  } else {
    results <- readability(txt.file=txt.file, hyphen=hyphen, index="Strain", parameters=list(Strain=parameters), ...)
  }
  return(results)
}
## end strain()


## traenkle.bailer()
#' Readability: Traenkle-Bailer Formeln
#' 
#' This is just a convenient wrapper function for \code{\link[koRpus:readability]{readability}}.
#'
#' This function calculates the two formulae by Tr\"ankle-Bailer, which are based on the Dickes-Steiwer formulae.
#' In contrast to \code{\link[koRpus:readability]{readability}}, which by default calculates all possible indices,
#' this function will only calculate the index values.
#'
#' This formula doesn't need syllable count.
#'
#' @param txt.file Either an object of class \code{\link[koRpus:kRp.text-class]{kRp.text}}, a character vector which must be be
#'    a valid path to a file containing the text to be analyzed, or a list of text features. If the latter, calculation
#'    is done by \code{\link[koRpus:readability.num]{readability.num}}. 
#' @param TB1 A numeric vector with named magic numbers for the first of the formulas.
#' @param TB2 A numeric vector with named magic numbers for the second of the formulas.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:readability]{readability}} for details.
#' @return An object of class \code{\link[koRpus:kRp.readability-class]{kRp.readability}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords readability
#' @export
#' @examples
#' \dontrun{
#'   traenkle.bailer(tagged.text)
#' }

traenkle.bailer <- function(txt.file,
      TB1=c(const=224.6814, awl=79.8304, asl=12.24032, prep=1.292857),
      TB2=c(const=234.1063, awl=96.11069, prep=2.05444, conj=1.02805), ...){
  all.parameters <- list(TB1=TB1, TB2=TB2)
  if(is.list(txt.file)){
    results <- readability.num(txt.features=txt.file, index="Traenkle.Bailer", parameters=list(Traenkle.Bailer=all.parameters), ...)
  } else {
    results <- readability(txt.file=txt.file, index="Traenkle.Bailer", parameters=list(Traenkle.Bailer=all.parameters), ...)
  }
  return(results)
}
## end traenkle.bailer()


## TRI()
#' Readability: Kuntzsch's Text-Redundanz-Index
#'
#' This is just a convenient wrapper function for \code{\link[koRpus:readability]{readability}}.
#'
#' This function calculates Kuntzsch's Text-Redundanz-Index (text redundancy index). In contrast to
#' \code{\link[koRpus:readability]{readability}}, which by default calculates all possible indices,
#' this function will only calculate the index value.
#'
#' @param txt.file Either an object of class \code{\link[koRpus:kRp.text-class]{kRp.text}}, a character vector which must be be
#'    a valid path to a file containing the text to be analyzed, or a list of text features. If the latter, calculation
#'    is done by \code{\link[koRpus:readability.num]{readability.num}}. 
#' @param hyphen An object of class kRp.hyphen. If \code{NULL}, the text will be hyphenated automatically.
#' @param parameters A numeric vector with named magic numbers, defining the relevant parameters for the index.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:readability]{readability}} for details.
#' @return An object of class \code{\link[koRpus:kRp.readability-class]{kRp.readability}}.
#' @keywords readability
#' @export
#' @examples
#' \dontrun{
#'   TRI(tagged.text)
#' }

TRI <- function(txt.file, hyphen=NULL, parameters=c(syll=1, word=0.449, pnct=2.467, frgn=0.937, const=14.417), ...){
  if(is.list(txt.file)){
    results <- readability.num(txt.features=txt.file, index="TRI", parameters=list(TRI=parameters), ...)
  } else {
    results <- readability(txt.file=txt.file, hyphen=hyphen, index="TRI", parameters=list(TRI=parameters), ...)
  }
  return(results)
}
## end TRI()


## tuldava()
#' Readability: Tuldava's Text Difficulty Formula
#'
#' This is just a convenient wrapper function for \code{\link[koRpus:readability]{readability}}.
#'
#' This function calculates Tuldava's Text Difficulty Formula. In contrast to
#' \code{\link[koRpus:readability]{readability}}, which by default calculates all possible indices,
#' this function will only calculate the index value.
#' 
#' @note This index originally has no parameter weights. To be able the use weights anyway, each parameter of the formula
#'    is available and its weight set to 1 by default.
#'
#' @param txt.file Either an object of class \code{\link[koRpus:kRp.text-class]{kRp.text}}, a character vector which must be be
#'    a valid path to a file containing the text to be analyzed, or a list of text features. If the latter, calculation
#'    is done by \code{\link[koRpus:readability.num]{readability.num}}. 
#' @param hyphen An object of class kRp.hyphen. If \code{NULL}, the text will be hyphenated automatically.
#' @param parameters A numeric vector with named magic numbers, defining the relevant parameters for the index.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:readability]{readability}} for details.
#' @return An object of class \code{\link[koRpus:kRp.readability-class]{kRp.readability}}.
#' @keywords readability
#' @export
#' @examples
#' \dontrun{
#'   tuldava(tagged.text)
#' }

tuldava <- function(txt.file, hyphen=NULL, parameters=c(syll=1, word1=1, word2=1, sent=1), ...){
  if(is.list(txt.file)){
    results <- readability.num(txt.features=txt.file, index="Tuldava", parameters=list(Tuldava=parameters), ...)
  } else {
    results <- readability(txt.file=txt.file, hyphen=hyphen, index="Tuldava", parameters=list(Tuldava=parameters), ...)
  }
  return(results)
}
## end tuldava()


## wheeler.smith()
#' Readability: Wheeler-Smith Score
#'
#' This is just a convenient wrapper function for \code{\link[koRpus:readability]{readability}}.
#'
#' This function calculates the Wheeler-Smith Score. In contrast to
#' \code{\link[koRpus:readability]{readability}}, which by default calculates all possible indices,
#' this function will only calculate the index value.
#'
#' If \code{parameters="de"}, the calculation stays the same, but grade placement
#' is done according to Bamberger & Vanecek (1984), that is for german texts.
#'
#' @param txt.file Either an object of class \code{\link[koRpus:kRp.text-class]{kRp.text}}, a character vector which must be be
#'    a valid path to a file containing the text to be analyzed, or a list of text features. If the latter, calculation
#'    is done by \code{\link[koRpus:readability.num]{readability.num}}. 
#' @param hyphen An object of class kRp.hyphen. If \code{NULL}, the text will be hyphenated automatically.
#' @param parameters A numeric vector with named magic numbers, defining the relevant parameters for the index.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:readability]{readability}} for details.
#' @return An object of class \code{\link[koRpus:kRp.readability-class]{kRp.readability}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @references
#'    Bamberger, R. & Vanecek, E. (1984). \emph{Lesen--Verstehen--Lernen--Schreiben}. Wien: Jugend und Volk.
#'
#'    Wheeler, L.R. & Smith, E.H. (1954). A practical readability formula for the classroom teacher in the primary grades. \emph{Elementary English},
#'      31, 397--399.
#' @keywords readability
#' @export
#' @examples
#' \dontrun{
#'   wheeler.smith(tagged.text)
#' }

wheeler.smith <- function(txt.file, hyphen=NULL, parameters=c(syll=2), ...){
  if(is.list(txt.file)){
    results <- readability.num(txt.features=txt.file, index="Wheeler.Smith", parameters=list(Wheeler.Smith=parameters), ...)
  } else {
    results <- readability(txt.file=txt.file, hyphen=hyphen, index="Wheeler.Smith", parameters=list(Wheeler.Smith=parameters), ...)
  }
  return(results)
}
## end wheeler.smith()
