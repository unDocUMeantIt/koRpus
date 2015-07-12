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


#' @param flat Logical, if TRUE only a named vector of main results is returned
#' @rdname summary-methods
#' @aliases summary,kRp.readability-method
#' @export
#' @docType methods
#' @examples
#' \dontrun{
#' summary(flesch(tagged.txt))
#' }
#' @include 00_class_10_kRp.readability.R
#' @include 01_method_summary.kRp.lang.R
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

  if(sum(!is.na(object@ARI)) > 1){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, ARI=object@ARI$grade)
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="ARI",
          flavour=object@ARI$flavour,
          grade=object@ARI$grade
        ))
    }
  } else {}
  if(sum(!is.na(object@ARI.NRI)) > 1){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, ARI.NRI=object@ARI.NRI$grade)
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="ARI",
          flavour=object@ARI.NRI$flavour,
          grade=object@ARI.NRI$grade
        ))
    }
  } else {}
  if(sum(!is.na(object@ARI.simple)) > 1){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, ARI.simple=object@ARI.simple$index)
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="ARI",
          flavour=object@ARI.simple$flavour,
          raw=object@ARI.simple$index
        ))
    }
  } else {}

  if(sum(is.na(object@Bormuth)) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Bormuth=object@Bormuth$mean.cloze)
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Bormuth",
          flavour=object@Bormuth$flavour,
          raw=object@Bormuth$mean.cloze,
          grade=object@Bormuth$grade
        ))
    }
  } else {}

  if(sum(is.na(object@Coleman)) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat,
        Coleman.C1=round(object@Coleman$C1, digits=0),
        Coleman.C2=round(object@Coleman$C2, digits=0),
        Coleman.C3=round(object@Coleman$C3, digits=0),
        Coleman.C4=round(object@Coleman$C4, digits=0))
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Coleman C1",
          flavour=object@Coleman$flavour,
          raw=round(object@Coleman$C1, digits=0)
        ))
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Coleman C2",
          flavour=object@Coleman$flavour,
          raw=round(object@Coleman$C2, digits=0)
        ))
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Coleman C3",
          flavour=object@Coleman$flavour,
          raw=round(object@Coleman$C3, digits=0)
        ))
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Coleman C4",
          flavour=object@Coleman$flavour,
          raw=round(object@Coleman$C4, digits=0)
        ))
    }
  } else {}

  if(sum(is.na(object@Coleman.Liau)) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Coleman.Liau=round(object@Coleman.Liau$ECP, digits=0))
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Coleman-Liau",
          flavour=object@Coleman.Liau$flavour,
          raw=round(object@Coleman.Liau$ECP, digits=0),
          grade=object@Coleman.Liau$grade
        ))
    }
  } else {}

  if(sum(is.na(object@Dale.Chall)) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Dale.Chall=object@Dale.Chall$raw)
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Dale-Chall",
          flavour=object@Dale.Chall$flavour,
          raw=object@Dale.Chall$raw,
          grade=object@Dale.Chall$grade,
          age=object@Dale.Chall$age
        ))
    }
  } else {}
  if(sum(is.na(object@Dale.Chall.PSK)) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Dale.Chall.PSK=object@Dale.Chall.PSK$raw)
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Dale-Chall",
          flavour=object@Dale.Chall.PSK$flavour,
          raw=object@Dale.Chall.PSK$raw,
          grade=object@Dale.Chall.PSK$grade,
          age=object@Dale.Chall.PSK$age
        ))
    }
  } else {}
  if(sum(is.na(object@Dale.Chall.old)) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Dale.Chall.old=object@Dale.Chall.old$raw)
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Dale-Chall",
          flavour=object@Dale.Chall.old$flavour,
          raw=object@Dale.Chall.old$raw,
          grade=object@Dale.Chall.old$grade,
          age=object@Dale.Chall.old$age
        ))
    }
  } else {}

  if(sum(is.na(object@Danielson.Bryan)) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat,
        Danielson.Bryan.DB1=object@Danielson.Bryan$DB1,
        Danielson.Bryan.DB2=object@Danielson.Bryan$DB2)
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Danielson-Bryan DB1",
          flavour=object@Danielson.Bryan$flavour,
          raw=object@Danielson.Bryan$DB1
        ))
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Danielson-Bryan DB2",
          flavour=object@Danielson.Bryan$flavour,
          raw=object@Danielson.Bryan$DB2,
          grade=object@Danielson.Bryan$DB2.grade
        ))
    }
  } else {}

  if(sum(is.na(object@Dickes.Steiwer)) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Dickes.Steiwer=object@Dickes.Steiwer$Dickes.Steiwer)
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Dickes-Steiwer",
          flavour=object@Dickes.Steiwer$flavour,
          raw=object@Dickes.Steiwer$Dickes.Steiwer
        ))
    }
  } else {}

  if(sum(is.na(object@DRP)) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, DRP=object@DRP$DRP)
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="DRP",
          raw=object@DRP$DRP
        ))
    }
  } else {}

  if(sum(is.na(object@ELF)) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, ELF=object@ELF$ELF)
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="ELF",
          flavour=object@ELF$flavour,
          raw=object@ELF$ELF
        ))
    }
  } else {}

  if(sum(is.na(object@Farr.Jenkins.Paterson)) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Farr.Jenkins.Paterson=object@Farr.Jenkins.Paterson$FJP)
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Farr-Jenkins-Paterson",
          flavour=object@Farr.Jenkins.Paterson$flavour,
          raw=object@Farr.Jenkins.Paterson$FJP,
          grade=object@Farr.Jenkins.Paterson$grade
        ))
    }
  } else {}
  if(sum(is.na(object@Farr.Jenkins.Paterson.PSK)) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Farr.Jenkins.Paterson.PSK=object@Farr.Jenkins.Paterson.PSK$FJP)
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Farr-Jenkins-Paterson",
          flavour=object@Farr.Jenkins.Paterson.PSK$flavour,
          grade=object@Farr.Jenkins.Paterson.PSK$FJP
        ))
    }
  } else {}

  if(sum(!is.na(object@Flesch)) > 1){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Flesch=object@Flesch$RE)
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Flesch",
          flavour=object@Flesch$flavour,
          raw=object@Flesch$RE,
          grade=object@Flesch$grade,
          age=object@Flesch$age
        ))
    }
  } else {}
  if(sum(!is.na(object@Flesch.PSK)) > 1){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Flesch.PSK=object@Flesch.PSK$grade)
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Flesch",
          flavour=object@Flesch.PSK$flavour,
          raw=object@Flesch.PSK$RE,
          grade=object@Flesch.PSK$grade,
          age=object@Flesch.PSK$age
        ))
    }
  } else {}
  if(sum(!is.na(object@Flesch.Szigriszt)) > 1){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Flesch.Szigriszt=object@Flesch.Szigriszt$RE)
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Flesch",
          flavour=object@Flesch.Szigriszt$flavour,
          raw=object@Flesch.Szigriszt$RE,
          grade=object@Flesch.Szigriszt$grade,
          age=object@Flesch.Szigriszt$age
        ))
    }
  } else {}
  if(sum(!is.na(object@Flesch.de)) > 1){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Flesch.de=object@Flesch.de$RE)
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Flesch",
          flavour=object@Flesch.de$flavour,
          raw=object@Flesch.de$RE,
          grade=object@Flesch.de$grade,
          age=object@Flesch.de$age
        ))
    }
  } else {}
  if(sum(!is.na(object@Flesch.es)) > 1){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Flesch.es=object@Flesch.es$RE)
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Flesch",
          flavour=object@Flesch.es$flavour,
          raw=object@Flesch.es$RE,
          grade=object@Flesch.es$grade,
          age=object@Flesch.es$age
        ))
    }
  } else {}
  if(sum(!is.na(object@Flesch.fr)) > 1){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Flesch.fr=object@Flesch.fr$RE)
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Flesch",
          flavour=object@Flesch.fr$flavour,
          raw=object@Flesch.fr$RE,
          grade=object@Flesch.fr$grade,
          age=object@Flesch.fr$age
        ))
    }
  } else {}
  if(sum(!is.na(object@Flesch.nl)) > 1){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Flesch.nl=object@Flesch.nl$RE)
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Flesch",
          flavour=object@Flesch.nl$flavour,
          raw=object@Flesch.nl$RE,
          grade=object@Flesch.nl$grade,
          age=object@Flesch.nl$age
        ))
    }
  } else {}

  if(sum(is.na(object@Flesch.Kincaid)) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Flesch.Kincaid=object@Flesch.Kincaid$grade)
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Flesch-Kincaid",
          flavour=object@Flesch.Kincaid$flavour,
          grade=object@Flesch.Kincaid$grade,
          age=object@Flesch.Kincaid$age
        ))
    }
  } else {}

  if(sum(is.na(object@FOG)) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, FOG=object@FOG$FOG)
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="FOG",
          flavour=object@FOG$flavour,
          grade=object@FOG$FOG
        ))
    }
  } else {}
  if(sum(is.na(object@FOG.PSK)) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, FOG.PSK=object@FOG.PSK$FOG)
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="FOG",
          flavour=object@FOG.PSK$flavour,
          grade=object@FOG.PSK$FOG
        ))
    }
  } else {}
  if(sum(is.na(object@FOG.NRI)) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, FOG.NRI=object@FOG.NRI$FOG)
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="FOG",
          flavour=object@FOG.NRI$flavour,
          grade=object@FOG.NRI$FOG
        ))
    }
  } else {}

  if(sum(is.na(object@FORCAST)) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, FORCAST=object@FORCAST$grade)
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="FORCAST",
          flavour=object@FORCAST$flavour,
          grade=object@FORCAST$grade,
          age=object@FORCAST$age
        ))
    }
  } else {}
  if(sum(is.na(object@FORCAST.RGL)) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, FORCAST.RGL=object@FORCAST.RGL$grade)
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="FORCAST",
          flavour=object@FORCAST.RGL$flavour,
          grade=object@FORCAST.RGL$grade,
          age=object@FORCAST.RGL$age
        ))
    }
  } else {}

  if(sum(is.na(object@Fucks)) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Fucks=object@Fucks$Fucks)
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Fucks",
          raw=object@Fucks$Fucks,
          grade=object@Fucks$grade
        ))
    }
  } else {}

  if(sum(is.na(object@Harris.Jacobson)) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat,
        HJ1=object@Harris.Jacobson$HJ1,
        HJ2=object@Harris.Jacobson$HJ2,
        HJ3=object@Harris.Jacobson$HJ3,
        HJ4=object@Harris.Jacobson$HJ4)
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Harris-Jacobson HJ1",
          flavour=object@Harris.Jacobson$flavour,
          raw=object@Harris.Jacobson$HJ1
        ))
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Harris-Jacobson HJ2",
          flavour=object@Harris.Jacobson$flavour,
          raw=object@Harris.Jacobson$HJ2
        ))
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Harris-Jacobson HJ3",
          flavour=object@Harris.Jacobson$flavour,
          raw=object@Harris.Jacobson$HJ3
        ))
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Harris-Jacobson HJ4",
          flavour=object@Harris.Jacobson$flavour,
          raw=object@Harris.Jacobson$HJ4
        ))
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Harris-Jacobson HJ5",
          flavour=object@Harris.Jacobson$flavour,
          raw=object@Harris.Jacobson$HJ5
        ))
    }
  } else {}

  if(sum(is.na(object@Linsear.Write)) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Linsear.Write=object@Linsear.Write$grade)
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Linsear-Write",
          flavour=object@Linsear.Write$flavour,
          grade=object@Linsear.Write$grade
        ))
    }
  } else {}

  if(sum(is.na(object@LIX)) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, LIX=object@LIX$index)
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="LIX",
          flavour=object@LIX$flavour,
          raw=object@LIX$index,
          grade=object@LIX$grade
        ))
    }
  } else {}

  if(sum(is.na(object@Wiener.STF)) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat,
        nWS1=object@Wiener.STF$nWS1,
        nWS2=object@Wiener.STF$nWS2,
        nWS3=object@Wiener.STF$nWS3,
        nWS4=object@Wiener.STF$nWS4)
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="nWS1",
          flavour=object@Wiener.STF$flavour,
          grade=object@Wiener.STF$nWS1
        ))
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="nWS2",
          flavour=object@Wiener.STF$flavour,
          grade=object@Wiener.STF$nWS2
        ))
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="nWS3",
          flavour=object@Wiener.STF$flavour,
          grade=object@Wiener.STF$nWS3
        ))
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="nWS4",
          flavour=object@Wiener.STF$flavour,
          grade=object@Wiener.STF$nWS4
        ))
    }
  } else {}

  if(sum(is.na(object@RIX)) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, RIX=object@RIX$index)
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="RIX",
          flavour=object@RIX$flavour,
          raw=object@RIX$index,
          grade=object@RIX$grade
        ))
    }
  } else {}

  if(sum(is.na(object@SMOG)) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, SMOG=object@SMOG$grade)
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="SMOG",
          flavour=object@SMOG$flavour,
          grade=object@SMOG$grade,
          age=object@SMOG$age
        ))
    }
  } else {}
  if(sum(is.na(object@SMOG.de)) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, SMOG.de=object@SMOG.de$grade)
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="SMOG",
          flavour=object@SMOG.de$flavour,
          grade=object@SMOG.de$grade,
          age=object@SMOG.de$age
        ))
    }
  } else {}
  if(sum(is.na(object@SMOG.C)) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, SMOG.C=object@SMOG.C$grade)
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="SMOG",
          flavour=object@SMOG.C$flavour,
          grade=object@SMOG.C$grade,
          age=object@SMOG.C$age
        ))
    }
  } else {}
  if(sum(is.na(object@SMOG.simple)) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, SMOG.simple=object@SMOG.simple$grade)
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="SMOG",
          flavour=object@SMOG.simple$flavour,
          grade=object@SMOG.simple$grade,
          age=object@SMOG.simple$age
        ))
    }
  } else {}

  if(sum(is.na(object@Spache)) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Spache=object@Spache$grade)
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Spache",
          flavour=object@Spache$flavour,
          grade=object@Spache$grade
        ))
    }
  } else {}
  if(sum(is.na(object@Spache.old)) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Spache.old=object@Spache.old$grade)
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Spache",
          flavour=object@Spache.old$flavour,
          grade=object@Spache.old$grade
        ))
    }
  } else {}

  if(sum(is.na(object@Strain)) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Strain=object@Strain$index)
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Strain",
          flavour=object@Strain$flavour,
          raw=object@Strain$index
        ))
    }
  } else {}

  if(sum(is.na(object@Traenkle.Bailer)) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat,
        Traenkle.Bailer.TB1=object@Traenkle.Bailer$TB1,
        Traenkle.Bailer.TB2=object@Traenkle.Bailer$TB2)
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Traenkle-Bailer TB1",
          flavour=object@Traenkle.Bailer$flavour,
          raw=object@Traenkle.Bailer$TB1
        ))
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Traenkle-Bailer TB2",
          flavour=object@Traenkle.Bailer$flavour,
          raw=object@Traenkle.Bailer$TB2
        ))
    }
  } else {}

  if(sum(is.na(object@TRI)) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, TRI=object@TRI$TRI)
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="TRI",
          flavour=object@TRI$flavour,
          raw=object@TRI$TRI
        ))
    }
  } else {}

  if(sum(is.na(object@Tuldava)) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Tuldava=object@Tuldava$Tuldava)
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Tuldava",
          flavour=object@Tuldava$flavour,
          raw=object@Tuldava$Tuldava
        ))
    }
  } else {}

  if(sum(is.na(object@Wheeler.Smith)) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Wheeler.Smith=object@Wheeler.Smith$score)
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Wheeler-Smith",
          flavour=object@Wheeler.Smith$flavour,
          raw=object@Wheeler.Smith$score,
          grade=object@Wheeler.Smith$grade
        ))
    }
  } else {}
  if(sum(is.na(object@Wheeler.Smith.de)) == 0){
    if(isTRUE(flat)){
      summary.flat <- c(summary.flat, Wheeler.Smith.de=object@Wheeler.Smith.de$score)
    } else {
      summary.table <- add.to.sumtab(summary.table, adds=list(
          index="Wheeler-Smith",
          flavour=object@Wheeler.Smith.de$flavour,
          raw=object@Wheeler.Smith.de$score,
          grade=object@Wheeler.Smith.de$grade
        ))
    }
  } else {}

  if(isTRUE(flat)){
    return(round(summary.flat, digits=2))
  } else {
    if(length(object@lang) > 0){
      cat("Text language:", object@lang, "\n")
    } else {}
    # remove the empty first row
    summary.table <- summary.table[-1,]
    dimnames(summary.table)[[1]] <- c(1:dim(summary.table)[[1]])
    return(format(summary.table, digits=2))
  }
})
