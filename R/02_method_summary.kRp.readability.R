# Copyright 2010-2019 Meik Michalke <meik.michalke@hhu.de>
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


#' @param flat Logical, if TRUE only a named vector of main results is returned
#' @rdname summary-methods
#' @aliases summary,kRp.readability-method
#' @export
#' @docType methods
#' @examples
#' \dontrun{
#' summary(flesch(tagged.txt))
#' }
#' @include 01_class_05_kRp.readability.R
#' @include 02_method_summary.kRp.lang.R
setMethod("summary", signature(object="kRp.readability"), function(object, flat=FALSE){

  # function to add stuff to the matrix,
  # "adds" must me a named list
  add.to.sumtab <- function(table, adds){
    add.index <- add.flavour <- add.raw <- add.grade <- add.age <- ""
    if("index" %in% names(adds)){
      if(!is.na(adds[["index"]])){
        add.index <- adds[["index"]]
      } else {}
    } else {}
    if("flavour" %in% names(adds) & !identical(adds[["flavour"]], "default")){
      add.flavour <- adds[["flavour"]]
    } else {}
    if("raw" %in% names(adds)){
      if(!is.na(adds[["raw"]])){
        if(is.numeric(adds[["raw"]])){
          add.raw <- round(adds[["raw"]], digits=2)
        } else {
          add.raw <- adds[["raw"]]
        }
      } else {}
    } else {}
    if("grade" %in% names(adds)){
      if(!is.na(adds[["grade"]])){
        if(is.numeric(adds[["grade"]])){
          add.grade <- round(adds[["grade"]], digits=2)
        } else {
          add.grade <- adds[["grade"]]
        }
      } else {}
    } else {}
    if("age" %in% names(adds)){
      if(!is.na(adds[["age"]])){
        if(is.numeric(adds[["age"]])){
          add.age <- round(adds[["age"]], digits=1)
        } else {
          add.age <- adds[["age"]]
        }
      } else {}
    } else {}
#     if("interp" %in% names(adds)){
#       add.interp <- adds[["interp"]]
#     } else {
#       add.interp <- ""
#     }
    res.table <- rbind(table, list(add.index, add.flavour, add.raw, add.grade, add.age))
    return(res.table)
  }

  summary.table <- data.frame(index="", flavour="", raw="", grade="", age="", stringsAsFactors=FALSE)
  summary.flat <- c()

  if(sum(!is.na(slot(object, "ARI"))) > 1){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, ARI=slot(object, "ARI")[["grade"]])
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="ARI",
          flavour=slot(object, "ARI")[["flavour"]],
          grade=slot(object, "ARI")[["grade"]]
        ))
    }
  } else {}
  if(sum(!is.na(slot(object, "ARI.NRI"))) > 1){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, ARI.NRI=slot(object, "ARI.NRI")[["grade"]])
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="ARI",
          flavour=slot(object, "ARI.NRI")[["flavour"]],
          grade=slot(object, "ARI.NRI")[["grade"]]
        ))
    }
  } else {}
  if(sum(!is.na(slot(object, "ARI.simple"))) > 1){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, ARI.simple=slot(object, "ARI.simple")[["index"]])
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="ARI",
          flavour=slot(object, "ARI.simple")[["flavour"]],
          raw=slot(object, "ARI.simple")[["index"]]
        ))
    }
  } else {}

  if(sum(is.na(slot(object, "Bormuth"))) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Bormuth=slot(object, "Bormuth")[["mean.cloze"]])
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Bormuth",
          flavour=slot(object, "Bormuth")[["flavour"]],
          raw=slot(object, "Bormuth")[["mean.cloze"]],
          grade=slot(object, "Bormuth")[["grade"]]
        ))
    }
  } else {}

  if(sum(is.na(slot(object, "Coleman"))) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat,
        Coleman.C1=round(slot(object, "Coleman")[["C1"]], digits=0),
        Coleman.C2=round(slot(object, "Coleman")[["C2"]], digits=0),
        Coleman.C3=round(slot(object, "Coleman")[["C3"]], digits=0),
        Coleman.C4=round(slot(object, "Coleman")[["C4"]], digits=0))
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Coleman C1",
          flavour=slot(object, "Coleman")[["flavour"]],
          raw=round(slot(object, "Coleman")[["C1"]], digits=0)
        ))
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Coleman C2",
          flavour=slot(object, "Coleman")[["flavour"]],
          raw=round(slot(object, "Coleman")[["C2"]], digits=0)
        ))
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Coleman C3",
          flavour=slot(object, "Coleman")[["flavour"]],
          raw=round(slot(object, "Coleman")[["C3"]], digits=0)
        ))
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Coleman C4",
          flavour=slot(object, "Coleman")[["flavour"]],
          raw=round(slot(object, "Coleman")[["C4"]], digits=0)
        ))
    }
  } else {}

  if(sum(is.na(slot(object, "Coleman.Liau"))) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Coleman.Liau=round(slot(object, "Coleman.Liau")[["ECP"]], digits=0))
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Coleman-Liau",
          flavour=slot(object, "Coleman.Liau")[["flavour"]],
          raw=round(slot(object, "Coleman.Liau")[["ECP"]], digits=0),
          grade=slot(object, "Coleman.Liau")[["grade"]]
        ))
    }
  } else {}

  if(sum(is.na(slot(object, "Dale.Chall"))) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Dale.Chall=slot(object, "Dale.Chall")[["raw"]])
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Dale-Chall",
          flavour=slot(object, "Dale.Chall")[["flavour"]],
          raw=slot(object, "Dale.Chall")[["raw"]],
          grade=slot(object, "Dale.Chall")[["grade"]],
          age=slot(object, "Dale.Chall")[["age"]]
        ))
    }
  } else {}
  if(sum(is.na(slot(object, "Dale.Chall.PSK"))) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Dale.Chall.PSK=slot(object, "Dale.Chall.PSK")[["raw"]])
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Dale-Chall",
          flavour=slot(object, "Dale.Chall.PSK")[["flavour"]],
          raw=slot(object, "Dale.Chall.PSK")[["raw"]],
          grade=slot(object, "Dale.Chall.PSK")[["grade"]],
          age=slot(object, "Dale.Chall.PSK")[["age"]]
        ))
    }
  } else {}
  if(sum(is.na(slot(object, "Dale.Chall.old"))) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Dale.Chall.old=slot(object, "Dale.Chall.old")[["raw"]])
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Dale-Chall",
          flavour=slot(object, "Dale.Chall.old")[["flavour"]],
          raw=slot(object, "Dale.Chall.old")[["raw"]],
          grade=slot(object, "Dale.Chall.old")[["grade"]],
          age=slot(object, "Dale.Chall.old")[["age"]]
        ))
    }
  } else {}

  if(sum(is.na(slot(object, "Danielson.Bryan"))) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat,
        Danielson.Bryan.DB1=slot(object, "Danielson.Bryan")[["DB1"]],
        Danielson.Bryan.DB2=slot(object, "Danielson.Bryan")[["DB2"]])
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Danielson-Bryan DB1",
          flavour=slot(object, "Danielson.Bryan")[["flavour"]],
          raw=slot(object, "Danielson.Bryan")[["DB1"]]
        ))
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Danielson-Bryan DB2",
          flavour=slot(object, "Danielson.Bryan")[["flavour"]],
          raw=slot(object, "Danielson.Bryan")[["DB2"]],
          grade=slot(object, "Danielson.Bryan")[["DB2.grade"]]
        ))
    }
  } else {}

  if(sum(is.na(slot(object, "Dickes.Steiwer"))) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Dickes.Steiwer=slot(object, "Dickes.Steiwer")[["Dickes.Steiwer"]])
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Dickes-Steiwer",
          flavour=slot(object, "Dickes.Steiwer")[["flavour"]],
          raw=slot(object, "Dickes.Steiwer")[["Dickes.Steiwer"]]
        ))
    }
  } else {}

  if(sum(is.na(slot(object, "DRP"))) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, DRP=slot(object, "DRP")[["DRP"]])
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="DRP",
          raw=slot(object, "DRP")[["DRP"]]
        ))
    }
  } else {}

  if(sum(is.na(slot(object, "ELF"))) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, ELF=slot(object, "ELF")[["ELF"]])
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="ELF",
          flavour=slot(object, "ELF")[["flavour"]],
          raw=slot(object, "ELF")[["ELF"]]
        ))
    }
  } else {}

  if(sum(is.na(slot(object, "Farr.Jenkins.Paterson"))) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Farr.Jenkins.Paterson=slot(object, "Farr.Jenkins.Paterson")[["FJP"]])
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Farr-Jenkins-Paterson",
          flavour=slot(object, "Farr.Jenkins.Paterson")[["flavour"]],
          raw=slot(object, "Farr.Jenkins.Paterson")[["FJP"]],
          grade=slot(object, "Farr.Jenkins.Paterson")[["grade"]]
        ))
    }
  } else {}
  if(sum(is.na(slot(object, "Farr.Jenkins.Paterson.PSK"))) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Farr.Jenkins.Paterson.PSK=slot(object, "Farr.Jenkins.Paterson.PSK")[["FJP"]])
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Farr-Jenkins-Paterson",
          flavour=slot(object, "Farr.Jenkins.Paterson.PSK")[["flavour"]],
          grade=slot(object, "Farr.Jenkins.Paterson.PSK")[["FJP"]]
        ))
    }
  } else {}

  if(sum(!is.na(slot(object, "Flesch"))) > 1){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Flesch=slot(object, "Flesch")[["RE"]])
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Flesch",
          flavour=slot(object, "Flesch")[["flavour"]],
          raw=slot(object, "Flesch")[["RE"]],
          grade=slot(object, "Flesch")[["grade"]],
          age=slot(object, "Flesch")[["age"]]
        ))
    }
  } else {}
  if(sum(!is.na(slot(object, "Flesch.PSK"))) > 1){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Flesch.PSK=slot(object, "Flesch.PSK")[["grade"]])
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Flesch",
          flavour=slot(object, "Flesch.PSK")[["flavour"]],
          raw=slot(object, "Flesch.PSK")[["RE"]],
          grade=slot(object, "Flesch.PSK")[["grade"]],
          age=slot(object, "Flesch.PSK")[["age"]]
        ))
    }
  } else {}
  if(sum(!is.na(slot(object, "Flesch.Brouwer"))) > 1){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Flesch.Brouwer=slot(object, "Flesch.Brouwer")[["RE"]])
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Flesch",
          flavour=slot(object, "Flesch.Brouwer")[["flavour"]],
          raw=slot(object, "Flesch.Brouwer")[["RE"]],
          grade=slot(object, "Flesch.Brouwer")[["grade"]],
          age=slot(object, "Flesch.Brouwer")[["age"]]
        ))
    }
  } else {}
  if(sum(!is.na(slot(object, "Flesch.Szigriszt"))) > 1){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Flesch.Szigriszt=slot(object, "Flesch.Szigriszt")[["RE"]])
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Flesch",
          flavour=slot(object, "Flesch.Szigriszt")[["flavour"]],
          raw=slot(object, "Flesch.Szigriszt")[["RE"]],
          grade=slot(object, "Flesch.Szigriszt")[["grade"]],
          age=slot(object, "Flesch.Szigriszt")[["age"]]
        ))
    }
  } else {}
  if(sum(!is.na(slot(object, "Flesch.de"))) > 1){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Flesch.de=slot(object, "Flesch.de")[["RE"]])
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Flesch",
          flavour=slot(object, "Flesch.de")[["flavour"]],
          raw=slot(object, "Flesch.de")[["RE"]],
          grade=slot(object, "Flesch.de")[["grade"]],
          age=slot(object, "Flesch.de")[["age"]]
        ))
    }
  } else {}
  if(sum(!is.na(slot(object, "Flesch.es"))) > 1){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Flesch.es=slot(object, "Flesch.es")[["RE"]])
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Flesch",
          flavour=slot(object, "Flesch.es")[["flavour"]],
          raw=slot(object, "Flesch.es")[["RE"]],
          grade=slot(object, "Flesch.es")[["grade"]],
          age=slot(object, "Flesch.es")[["age"]]
        ))
    }
  } else {}
  if(sum(!is.na(slot(object, "Flesch.fr"))) > 1){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Flesch.fr=slot(object, "Flesch.fr")[["RE"]])
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Flesch",
          flavour=slot(object, "Flesch.fr")[["flavour"]],
          raw=slot(object, "Flesch.fr")[["RE"]],
          grade=slot(object, "Flesch.fr")[["grade"]],
          age=slot(object, "Flesch.fr")[["age"]]
        ))
    }
  } else {}
  if(sum(!is.na(slot(object, "Flesch.nl"))) > 1){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Flesch.nl=slot(object, "Flesch.nl")[["RE"]])
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Flesch",
          flavour=slot(object, "Flesch.nl")[["flavour"]],
          raw=slot(object, "Flesch.nl")[["RE"]],
          grade=slot(object, "Flesch.nl")[["grade"]],
          age=slot(object, "Flesch.nl")[["age"]]
        ))
    }
  } else {}

  if(sum(is.na(slot(object, "Flesch.Kincaid"))) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Flesch.Kincaid=slot(object, "Flesch.Kincaid")[["grade"]])
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Flesch-Kincaid",
          flavour=slot(object, "Flesch.Kincaid")[["flavour"]],
          grade=slot(object, "Flesch.Kincaid")[["grade"]],
          age=slot(object, "Flesch.Kincaid")[["age"]]
        ))
    }
  } else {}

  if(sum(is.na(slot(object, "FOG"))) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, FOG=slot(object, "FOG")[["FOG"]])
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="FOG",
          flavour=slot(object, "FOG")[["flavour"]],
          grade=slot(object, "FOG")[["FOG"]]
        ))
    }
  } else {}
  if(sum(is.na(slot(object, "FOG.PSK"))) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, FOG.PSK=slot(object, "FOG.PSK")[["FOG"]])
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="FOG",
          flavour=slot(object, "FOG.PSK")[["flavour"]],
          grade=slot(object, "FOG.PSK")[["FOG"]]
        ))
    }
  } else {}
  if(sum(is.na(slot(object, "FOG.NRI"))) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, FOG.NRI=slot(object, "FOG.NRI")[["FOG"]])
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="FOG",
          flavour=slot(object, "FOG.NRI")[["flavour"]],
          grade=slot(object, "FOG.NRI")[["FOG"]]
        ))
    }
  } else {}

  if(sum(is.na(slot(object, "FORCAST"))) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, FORCAST=slot(object, "FORCAST")[["grade"]])
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="FORCAST",
          flavour=slot(object, "FORCAST")[["flavour"]],
          grade=slot(object, "FORCAST")[["grade"]],
          age=slot(object, "FORCAST")[["age"]]
        ))
    }
  } else {}
  if(sum(is.na(slot(object, "FORCAST.RGL"))) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, FORCAST.RGL=slot(object, "FORCAST.RGL")[["grade"]])
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="FORCAST",
          flavour=slot(object, "FORCAST.RGL")[["flavour"]],
          grade=slot(object, "FORCAST.RGL")[["grade"]],
          age=slot(object, "FORCAST.RGL")[["age"]]
        ))
    }
  } else {}

  if(sum(is.na(slot(object, "Fucks"))) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Fucks=slot(object, "Fucks")[["Fucks"]])
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Fucks",
          raw=slot(object, "Fucks")[["Fucks"]],
          grade=slot(object, "Fucks")[["grade"]]
        ))
    }
  } else {}

  if(sum(is.na(slot(object, "Harris.Jacobson"))) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat,
        HJ1=slot(object, "Harris.Jacobson")[["HJ1"]],
        HJ2=slot(object, "Harris.Jacobson")[["HJ2"]],
        HJ3=slot(object, "Harris.Jacobson")[["HJ3"]],
        HJ4=slot(object, "Harris.Jacobson")[["HJ4"]])
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Harris-Jacobson HJ1",
          flavour=slot(object, "Harris.Jacobson")[["flavour"]],
          raw=slot(object, "Harris.Jacobson")[["HJ1"]]
        ))
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Harris-Jacobson HJ2",
          flavour=slot(object, "Harris.Jacobson")[["flavour"]],
          raw=slot(object, "Harris.Jacobson")[["HJ2"]]
        ))
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Harris-Jacobson HJ3",
          flavour=slot(object, "Harris.Jacobson")[["flavour"]],
          raw=slot(object, "Harris.Jacobson")[["HJ3"]]
        ))
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Harris-Jacobson HJ4",
          flavour=slot(object, "Harris.Jacobson")[["flavour"]],
          raw=slot(object, "Harris.Jacobson")[["HJ4"]]
        ))
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Harris-Jacobson HJ5",
          flavour=slot(object, "Harris.Jacobson")[["flavour"]],
          raw=slot(object, "Harris.Jacobson")[["HJ5"]]
        ))
    }
  } else {}

  if(sum(is.na(slot(object, "Linsear.Write"))) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Linsear.Write=slot(object, "Linsear.Write")[["grade"]])
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Linsear-Write",
          flavour=slot(object, "Linsear.Write")[["flavour"]],
          grade=slot(object, "Linsear.Write")[["grade"]]
        ))
    }
  } else {}

  if(sum(is.na(slot(object, "LIX"))) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, LIX=slot(object, "LIX")[["index"]])
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="LIX",
          flavour=slot(object, "LIX")[["flavour"]],
          raw=slot(object, "LIX")[["index"]],
          grade=slot(object, "LIX")[["grade"]]
        ))
    }
  } else {}

  if(sum(is.na(slot(object, "Wiener.STF"))) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat,
        nWS1=slot(object, "Wiener.STF")[["nWS1"]],
        nWS2=slot(object, "Wiener.STF")[["nWS2"]],
        nWS3=slot(object, "Wiener.STF")[["nWS3"]],
        nWS4=slot(object, "Wiener.STF")[["nWS4"]])
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="nWS1",
          flavour=slot(object, "Wiener.STF")[["flavour"]],
          grade=slot(object, "Wiener.STF")[["nWS1"]]
        ))
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="nWS2",
          flavour=slot(object, "Wiener.STF")[["flavour"]],
          grade=slot(object, "Wiener.STF")[["nWS2"]]
        ))
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="nWS3",
          flavour=slot(object, "Wiener.STF")[["flavour"]],
          grade=slot(object, "Wiener.STF")[["nWS3"]]
        ))
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="nWS4",
          flavour=slot(object, "Wiener.STF")[["flavour"]],
          grade=slot(object, "Wiener.STF")[["nWS4"]]
        ))
    }
  } else {}

  if(sum(is.na(slot(object, "RIX"))) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, RIX=slot(object, "RIX")[["index"]])
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="RIX",
          flavour=slot(object, "RIX")[["flavour"]],
          raw=slot(object, "RIX")[["index"]],
          grade=slot(object, "RIX")[["grade"]]
        ))
    }
  } else {}

  if(sum(is.na(slot(object, "SMOG"))) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, SMOG=slot(object, "SMOG")[["grade"]])
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="SMOG",
          flavour=slot(object, "SMOG")[["flavour"]],
          grade=slot(object, "SMOG")[["grade"]],
          age=slot(object, "SMOG")[["age"]]
        ))
    }
  } else {}
  if(sum(is.na(slot(object, "SMOG.de"))) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, SMOG.de=slot(object, "SMOG.de")[["grade"]])
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="SMOG",
          flavour=slot(object, "SMOG.de")[["flavour"]],
          grade=slot(object, "SMOG.de")[["grade"]],
          age=slot(object, "SMOG.de")[["age"]]
        ))
    }
  } else {}
  if(sum(is.na(slot(object, "SMOG.C"))) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, SMOG.C=slot(object, "SMOG.C")[["grade"]])
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="SMOG",
          flavour=slot(object, "SMOG.C")[["flavour"]],
          grade=slot(object, "SMOG.C")[["grade"]],
          age=slot(object, "SMOG.C")[["age"]]
        ))
    }
  } else {}
  if(sum(is.na(slot(object, "SMOG.simple"))) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, SMOG.simple=slot(object, "SMOG.simple")[["grade"]])
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="SMOG",
          flavour=slot(object, "SMOG.simple")[["flavour"]],
          grade=slot(object, "SMOG.simple")[["grade"]],
          age=slot(object, "SMOG.simple")[["age"]]
        ))
    }
  } else {}

  if(sum(is.na(slot(object, "Spache"))) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Spache=slot(object, "Spache")[["grade"]])
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Spache",
          flavour=slot(object, "Spache")[["flavour"]],
          grade=slot(object, "Spache")[["grade"]]
        ))
    }
  } else {}
  if(sum(is.na(slot(object, "Spache.old"))) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Spache.old=slot(object, "Spache.old")[["grade"]])
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Spache",
          flavour=slot(object, "Spache.old")[["flavour"]],
          grade=slot(object, "Spache.old")[["grade"]]
        ))
    }
  } else {}

  if(sum(is.na(slot(object, "Strain"))) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Strain=slot(object, "Strain")[["index"]])
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Strain",
          flavour=slot(object, "Strain")[["flavour"]],
          raw=slot(object, "Strain")[["index"]]
        ))
    }
  } else {}

  if(sum(is.na(slot(object, "Traenkle.Bailer"))) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat,
        Traenkle.Bailer.TB1=slot(object, "Traenkle.Bailer")[["TB1"]],
        Traenkle.Bailer.TB2=slot(object, "Traenkle.Bailer")[["TB2"]])
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Traenkle-Bailer TB1",
          flavour=slot(object, "Traenkle.Bailer")[["flavour"]],
          raw=slot(object, "Traenkle.Bailer")[["TB1"]]
        ))
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Traenkle-Bailer TB2",
          flavour=slot(object, "Traenkle.Bailer")[["flavour"]],
          raw=slot(object, "Traenkle.Bailer")[["TB2"]]
        ))
    }
  } else {}

  if(sum(is.na(slot(object, "TRI"))) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, TRI=slot(object, "TRI")[["TRI"]])
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="TRI",
          flavour=slot(object, "TRI")[["flavour"]],
          raw=slot(object, "TRI")[["TRI"]]
        ))
    }
  } else {}

  if(sum(is.na(slot(object, "Tuldava"))) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Tuldava=slot(object, "Tuldava")[["Tuldava"]])
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Tuldava",
          flavour=slot(object, "Tuldava")[["flavour"]],
          raw=slot(object, "Tuldava")[["Tuldava"]]
        ))
    }
  } else {}

  if(sum(is.na(slot(object, "Wheeler.Smith"))) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Wheeler.Smith=slot(object, "Wheeler.Smith")[["score"]])
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Wheeler-Smith",
          flavour=slot(object, "Wheeler.Smith")[["flavour"]],
          raw=slot(object, "Wheeler.Smith")[["score"]],
          grade=slot(object, "Wheeler.Smith")[["grade"]]
        ))
    }
  } else {}
  if(sum(is.na(slot(object, "Wheeler.Smith.de"))) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Wheeler.Smith.de=slot(object, "Wheeler.Smith.de")[["score"]])
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Wheeler-Smith",
          flavour=slot(object, "Wheeler.Smith.de")[["flavour"]],
          raw=slot(object, "Wheeler.Smith.de")[["score"]],
          grade=slot(object, "Wheeler.Smith.de")[["grade"]]
        ))
    }
  } else {}

  if(isTRUE(flat)){
    return(round(summary.flat, digits=2))
  } else {
    if(length(slot(object, "lang")) > 0){
      cat("Text language:", slot(object, "lang"), "\n")
    } else {}
    # remove the empty first row
    summary.table <- summary.table[-1,]
    dimnames(summary.table)[[1]] <- c(1:dim(summary.table)[[1]])
    return(format(summary.table, digits=2))
  }
})
