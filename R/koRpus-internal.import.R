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


## Readability Studio import

import.RS <- function(res.txt){
  RS.res <- readLines(res.txt)
  # clean up empty values
  RS.res <- gsub("\t\t", "\tNA\t", gsub("\t$", "\tNA", RS.res))

  all.measures <- t(data.frame(
    c("Automated Readability Index","ARI",""),
  #  c("Bormuth Cloze Mean","Bormuth",""),
    c("Bormuth Grade Placement","Bormuth",""),
    c("Coleman-Liau","Coleman.Liau",""),
  #  c("Degrees of Reading Power","DRP",""),
    c("Degrees of Reading Power \\(grade equivalent\\)","DRP",""),
    c("Dolch Sight Words","Dolch Sight Words",""),
    c("Farr, Jenkins, Paterson","Farr.Jenkins.Paterson",""),
    c("Flesch-Kincaid","Flesch.Kincaid",""),
    c("Flesch Reading Ease","Flesch","en (Flesch)"),
    c("FORCAST","FORCAST",""),
    c("Fry","Fry",""),
    c("Gunning Fog","FOG",""),
    c("Harris-Jacobson Wide Range Formula","Harris.Jacobson",""),
    c("L\u00e4sbarhetsindex \\(LIX\\)","LIX",""),
    c("McAlpine EFLAW","McAlpine.EFLAW",""),
    c("New Dale-Chall","Dale.Chall","New Dale-Chall (1995)"),
    c("New Fog Count","FOG.NRI","New FOG (NRI)"),
    c("Powers, Sumner, Kearl \\(Dale-Chall\\)","Dale.Chall.PSK","Powers-Sumner-Kearl"),
    c("Powers, Sumner, Kearl \\(Farr, Jenkins, Paterson\\)","Farr.Jenkins.Paterson.PSK","Powers-Sumner-Kearl"),
    c("Powers, Sumner, Kearl \\(Flesch\\)","Flesch.PSK","Powers-Sumner-Kearl"),
    c("Powers, Sumner, Kearl \\(Gunning Fog\\)","FOG.PSK","Powers-Sumner-Kearl"),
    c("Rate Index \\(RIX\\)","RIX",""),
    c("Raygor Estimate","Raygor",""),
    c("Simplified Automated Readability Index","ARI.NRI","NRI"),
    c("SMOG","SMOG",""),
    c("Spache Revised","Spache","Revised formula (1978)"),
    c("Wheeler-Smith","Wheeler.Smith",""),
    c("Average \\(Mean\\)","Average (Mean)",""), stringsAsFactors=FALSE))

  measure.res <- t(sapply(all.measures[,1], function(cur.meas){
      cur.res <- unlist(strsplit(RS.res[grep(paste0("^",cur.meas,"\t(.*)\t(.*)\t"), RS.res)], "\t"))
      # add name and flavour:
      cur.res <- c(all.measures[all.measures[,1] == cur.meas,2],
          all.measures[all.measures[,1] == cur.meas,3], cur.res)
      # two special cases:
      if(identical(cur.meas, "Bormuth Grade Placement")){
        cur.res[7] <- unlist(strsplit(RS.res[grep("^Bormuth Cloze Mean\t(.*)\t(.*)\t", RS.res)], "\t"))[5]
      } else if(identical(cur.meas, "Degrees of Reading Power \\(grade equivalent\\)")){
        cur.res[6] <- unlist(strsplit(RS.res[grep("^Degrees of Reading Power\t(.*)\t(.*)\t", RS.res)], "\t"))[4]
      } else {}
      return(cur.res)
    }))

  # 1 Test  2 Grade Level  3 Reader Age  4 Scale Value  5 Predicted Cloze Score
  RS.all.res <- data.frame(index=measure.res[,1],
  flavour=measure.res[,2],
  raw=measure.res[,6], grade=measure.res[,4], age=measure.res[,5], cloze=measure.res[,7], stringsAsFactors=FALSE)

  # clean results
  RS.all.res[RS.all.res == "NA"] <- ""

  dimnames(RS.all.res)[[1]] <- c(1:dim(RS.all.res)[[1]])
  return(RS.all.res)
}

# imports the descriptive statistics file
import.RS.desc <- function(res.txt, kRp.format=TRUE){
  RS.res <- readLines(res.txt, n=40)[-1]
  # clean up empty values
  RS.res <- gsub("\t\t", "\tNA\t", gsub("\t$", "\tNA", RS.res))
  RS.res <- t(data.frame(strsplit(RS.res, "\t"), stringsAsFactors=FALSE))

  RS.res[RS.res == "NA"] <- ""
  dimnames(RS.res) <- list(c(1:dim(RS.res)[[1]]), c("statistic", "num", "pct"))

  # clean numbers
  RS.res[,"num"] <- gsub(",", ".", RS.res[,"num"])
  RS.res[,"num"] <- as.numeric(RS.res[,"num"])

  RS.res <- data.frame(RS.res, stringsAsFactors=FALSE)
  if(isTRUE(kRp.format)){
    class.analysis <- list(number=RS.res[RS.res$statistic == "Number of numerals:","num"],
              names=RS.res[RS.res$statistic == "Number of proper nouns:","num"]
      )
    RS.all.res <- list(classes=class.analysis,
      num.paragraphs=RS.res[RS.res$statistic == "Number of paragraphs:","num"],
      num.letters=RS.res[RS.res$statistic == "Number of characters (punctuation excluded):","num"],
      num.words=RS.res[RS.res$statistic == "Number of words:","num"],
      num.types=RS.res[RS.res$statistic == "Number of unique words:","num"],
      num.syll=RS.res[RS.res$statistic == "Number of syllables:","num"],
      num.monosyll=RS.res[RS.res$statistic == "Number of monosyllabic words:","num"],
      num.3syll=RS.res[RS.res$statistic == "Number of complex (3+ syllable) words:","num"],
      num.6syll=RS.res[RS.res$statistic == "Number of long (6+ characters) words:","num"],
      num.sentences=RS.res[RS.res$statistic == "Number of sentences (excluding incomplete sentences, see notes below):","num"],
      num.questions=RS.res[RS.res$statistic == "Number of interrogative sentences (questions):","num"],
      num.exclam=RS.res[RS.res$statistic == "Number of exclamatory sentences:","num"],
      avg.sentc.length=RS.res[RS.res$statistic == "Average sentence length:","num"],
      avg.word.length=RS.res[RS.res$statistic == "Average number of characters:","num"],
      avg.syll.word=RS.res[RS.res$statistic == "Average number of syllables:","num"],
      num.unf.dale.chall=RS.res[RS.res$statistic == "Number of Dale-Chall unfamiliar words:","num"],
      num.unf.har.jac=RS.res[RS.res$statistic == "Number of Harris-Jacobson unfamiliar words:","num"],
      num.unf.spache=RS.res[RS.res$statistic == "Number of Spache unfamiliar words:","num"]
    )
  } else {
    RS.all.res <- RS.res
  }

  return(RS.all.res)
}

## TextQuest import
import.TQ <- function(res.txt, encoding="Latin1"){
  TQ.res <- iconv(readLines(res.txt, encoding=encoding), from=encoding, to="UTF-8")
  TQ.res.R <- TQ.res[grep("^-[[:space:]]R[[:space:]][[:digit:]]{1,3}", TQ.res)]

  TQ.res.R.indexed <- list()

  for (this.R in TQ.res.R){
      split.I <- unlist(strsplit(this.R, ":[[:space:]]+"))
      name.I <- gsub("(-)([[:space:]]+)(R)([[:space:]]+)([[:digit:]]+)", "\\3\\5", split.I[1], perl=TRUE)
      value.I <- gsub("([-]{0,1}[[:digit:]]+[.]{0,1}[[:digit:]]*)([[:space:]]+)([-]{0,1}[[:digit:]]+[.]{0,1}[[:digit:]]*)([[:space:]]+)(.*)", "\\1", split.I[2], perl=TRUE)
      value.II <- gsub("([-]{0,1}[[:digit:]]+[.]{0,1}[[:digit:]]*)([[:space:]]+)([-]{0,1}[[:digit:]]+[.]{0,1}[[:digit:]]*)([[:space:]]+)(.*)", "\\3", split.I[2], perl=TRUE)
      if(name.I %in% paste0("R", sprintf("%02d", c(5:11,14,15,17,18,20,40,60)))) {
        value.III <- ""
        value.IV <- gsub("^[[:space:]]", "", split.I[3], perl=TRUE)
      } else if(identical(name.I, "R03")){
        value.III <- gsub("([[:space:]]+)([^[:space:]]+)([[:space:]]+)([^[:space:]]+)", "\\2", split.I[3], perl=TRUE)
        value.IV <- gsub("([[:space:]]+)([^[:space:]]+)([[:space:]]+)([^[:space:]]+)", "\\4", split.I[3], perl=TRUE)
      } else {
        value.III <- ""
        value.IV <- ""
      }
      desc.I <- gsub("([-]{0,1}[[:digit:]]+[.]{0,1}[[:digit:]]*)([[:space:]]+)([-]{0,1}[[:digit:]]+[.]{0,1}[[:digit:]]*)([[:space:]]+)(.*)", "\\5", split.I[2], perl=TRUE)
      TQ.res.R.indexed[[name.I]] <- c(value1=as.numeric(value.I), value2=as.numeric(value.II), value3=value.III, value4=value.IV, desc=desc.I)
    }
  TQ.res.R.indexed <- t(as.data.frame(TQ.res.R.indexed))
  TQ.v1 <- TQ.res.R.indexed[,"value1"]
  TQ.v2 <- TQ.res.R.indexed[,"value2"]
  TQ.v3 <- TQ.res.R.indexed[,"value3"]
  TQ.v4 <- TQ.res.R.indexed[,"value4"]
  # index      flavour      raw      grade        age
  all.measures <- t(data.frame(
    # raw values
    c(index="Coleman 1", flavour="",              raw=TQ.v2["R01"],  grade=NA,        age=NA),
    c(index="Coleman 2", flavour="",              raw=TQ.v2["R02"],  grade=NA,        age=NA),
    c(index="Coleman-Liau", flavour="",            raw=TQ.v2["R03"],  grade=TQ.v2["R13"],  age=NA), # raw, s. R13
    c(index="Danielson-Bryan DB1", flavour="",      raw=TQ.v2["R04"],  grade=NA,        age=NA),
    c(index="Danielson-Bryan DB2", flavour="",      raw=TQ.v2["R17"],  grade=TQ.v4["R17"],  age=NA),
    c(index="Farr-Jenkins-Paterson", flavour="",      raw=TQ.v2["R05"],  grade=TQ.v4["R05"],  age=NA),
    c(index="Farr-Jenkins-Paterson", flavour="Powers-Sumner-Kearl",  raw=NA, grade=TQ.v2["R09"],  age=NA),
    c(index="Flesch", flavour="en (Flesch)",        raw=TQ.v2["R06"], grade=TQ.v4["R06"],  age=NA),
    c(index="Flesch", flavour="Powers-Sumner-Kearl",  raw=NA,         grade=TQ.v2["R08"],  age=TQ.v4["R08"]),
    c(index="Flesch", flavour="Powers-Sumner-Kearl (R24)", raw=NA,      grade=TQ.v2["R24"],  age=TQ.v2["R34"]),
    c(index="Flesch", flavour="de (Amstad)",        raw=NA,         grade=TQ.v2["R36"],  age=NA),
    c(index="Flesch", flavour="es (Fernandez-Huerta)",  raw=NA,         grade=TQ.v2["R58"],  age=NA),
    c(index="Flesch", flavour="fr (Kandel-Moles)",    raw=NA,         grade=TQ.v2["R62"],  age=NA),
    c(index="Flesch", flavour="nl (Douma)",        raw=NA,         grade=TQ.v2["R64"],  age=NA),
    c(index="LIX", flavour="",                  raw=TQ.v2["R07"],  grade=TQ.v4["R07"],  age=NA),
    c(index="LIX", flavour="de Prosa",            raw=TQ.v2["R45"],  grade=NA,        age=NA),
    c(index="LIX", flavour="de Sachtext",          raw=TQ.v2["R46"],  grade=NA,        age=NA),
    c(index="LIX", flavour="dk",                raw=TQ.v2["R67"],  grade=NA,        age=NA),
    c(index="LIX", flavour="sw",                raw=TQ.v2["R68"],  grade=NA,        age=NA),
    c(index="RIX", flavour="",                  raw=TQ.v2["R10"],  grade=TQ.v4["R10"],  age=NA),
    c(index="RIX", flavour="de Prosa",            raw=TQ.v2["R47"],  grade=NA,        age=NA),
    c(index="RIX", flavour="de Sachtext",          raw=TQ.v2["R48"],  grade=NA,        age=NA),
    c(index="Wheeler-Smith", flavour="",          raw=TQ.v2["R11"],  grade=TQ.v4["R11"],  age=NA),
    c(index="Wheeler-Smith", flavour="de (Bamberger & Vanecek)",  raw=TQ.v1["R44"],  grade=TQ.v2["R44"],  age=NA),
    # grade levels
    c(index="ARI", flavour="",                  raw=NA,        grade=TQ.v2["R12"],  age=NA),
    c(index="ARI", flavour="simplified",          raw=TQ.v2["R30"],  grade=NA,        age=NA),
    c(index="Dale-Chall", flavour="",            raw=NA,        grade=TQ.v2["R15"],  age=NA),
    c(index="Dale-Chall", flavour="old",          raw=NA,        grade=TQ.v2["R14"],  age=NA),
    c(index="Dale-Chall", flavour="Powers-Sumner-Kearl",  raw=NA,      grade=TQ.v2["R16"],  age=NA),
    c(index="McAlpine EFLAW", flavour="",          raw=NA,        grade=TQ.v2["R18"],  age=NA),
    c(index="Flesch-Kincaid", flavour="",          raw=NA,        grade=TQ.v2["R19"],  age=TQ.v2["R31"]),
    c(index="FOG", flavour="",                  raw=NA,        grade=TQ.v2["R20"],  age=NA),
    c(index="FOG", flavour="Powers-Sumner-Kearl",    raw=NA,        grade=TQ.v2["R21"],  age=NA),
    c(index="FORCAST", flavour="",              raw=NA,        grade=TQ.v2["R22"],  age=TQ.v2["R32"]),
    c(index="Linsear-Write", flavour="",          raw=NA,        grade=TQ.v2["R23"],  age=NA),
    c(index="SMOG", flavour="simplified",          raw=NA,        grade=TQ.v2["R25"],  age=TQ.v2["R35"]),
    c(index="SMOG", flavour="p?",                raw=NA,        grade=TQ.v2["R26"],  age=NA),
    c(index="SMOG", flavour="Fomula C",            raw=NA,        grade=TQ.v2["R27"],  age=NA),
    c(index="SMOG.de", flavour="de (\"Qu\", Bamberger-Vanecek)", raw=NA,  grade=TQ.v2["R49"],  age=NA),
    c(index="SMOG", flavour="?",                raw=NA,        grade=TQ.v2["R43"],  age=NA),
    c(index="SMOG", flavour="es",                raw=NA,        grade=TQ.v2["R61"],  age=NA),
    c(index="SMOG", flavour="fr",                raw=NA,        grade=TQ.v2["R63"],  age=NA),
    c(index="Spache 1", flavour="",              raw=NA,        grade=TQ.v2["R28"],  age=NA),
    c(index="Spache 2", flavour="",              raw=NA,        grade=TQ.v2["R29"],  age=NA),
    # age
    c(index="Fry", flavour="",                  raw=NA,        grade=NA,        age=TQ.v2["R33"]),
    # german
    c(index="Dickes-Steiwer", flavour="",          raw=TQ.v2["R37"],  grade=NA,        age=NA),
    c(index="TRI", flavour="",                  raw=TQ.v2["R40"],  grade=NA,        age=NA),
    c(index="Fucks", flavour="",                raw=TQ.v1["R41"],  grade=TQ.v2["R41"],  age=NA), # value1: raw, value2: grade
    c(index="FDK", flavour="",                  raw=TQ.v2["R42"],  grade=NA,        age=NA),
    c(index="WSTF1", flavour="",                raw=TQ.v2["R50"],  grade=NA,        age=NA),
    c(index="WSTF2", flavour="",                raw=TQ.v2["R51"],  grade=NA,        age=NA),
    c(index="WSTF3", flavour="",                raw=TQ.v2["R52"],  grade=NA,        age=NA),
    c(index="WSTF4", flavour="",                raw=TQ.v2["R53"],  grade=NA,        age=NA),
    c(index="nWL1", flavour="",                raw=TQ.v2["R54"],  grade=NA,        age=NA),
    c(index="nWL2", flavour="",                raw=TQ.v2["R55"],  grade=NA,        age=NA),
    c(index="nWL3", flavour="",                raw=TQ.v2["R56"],  grade=NA,        age=NA),
    c(index="Traenkle-Bailer 1", flavour="",        raw=TQ.v2["R38"],  grade=NA,        age=NA),
    c(index="Traenkle-Bailer 2", flavour="",        raw=TQ.v2["R39"],  grade=NA,        age=NA),
    # spanish
    c(index="CSRI", flavour="",                raw=TQ.v2["R57"],  grade=NA,        age=NA),
    c(index="Gutierrez", flavour="",              raw=TQ.v2["R59"],  grade=NA,        age=NA),
    c(index="Spaulding", flavour="",              raw=TQ.v2["R60"],  grade=NA,        age=NA),
    # dutch
    c(index="Brouwer", flavour="",              raw=TQ.v2["R65"],  grade=NA,        age=NA),
    c(index="Staphorsius-Krom", flavour="",        raw=TQ.v2["R66"],  grade=NA,        age=NA),
    stringsAsFactors=FALSE))

  # clean results
  all.measures <- as.data.frame(all.measures, stringsAsFactors=FALSE)
  all.measures[is.na(all.measures)] <- ""
  dimnames(all.measures) <- list(c(1:dim(all.measures)[[1]]), c("index","flavour","raw","grade","age"))

  return(all.measures)
}

import.TQ.desc <- function(res.txt, encoding="Latin1"){
  TQ.res <- iconv(readLines(res.txt, encoding=encoding), from=encoding, to="UTF-8")
  TQ.res.I <- TQ.res[grep("^-[[:space:]]I[[:space:]][[:digit:]]{1,3}", TQ.res)]
  TQ.res.S <- TQ.res[grep("^-[[:space:]]S[[:space:]][[:digit:]]{1,3}", TQ.res)]

  TQ.res.I.indexed <- list()
  TQ.res.S.indexed <- list()

  for (this.I in TQ.res.I){
      split.I <- unlist(strsplit(this.I, ":[[:space:]]+"))
      name.I <- gsub("(-)([[:space:]]+)(I)([[:space:]]+)([[:digit:]]+)", "\\3\\5", split.I[1], perl=TRUE)
      value.I <- gsub("([[:digit:]]+)([[:space:]]+)(.*)", "\\1", split.I[2], perl=TRUE)
      if(name.I %in% paste0("I", sprintf("%02d", 1:11))) {
        value.II <- ""
        desc.I <- gsub("([[:digit:]]+)([[:space:]]+)(.*)", "\\3", split.I[2], perl=TRUE)
      } else {
#         value.II <- gsub("([[:digit:]]+)([[:space:]]+)\\(([[:space:]]*[[:digit:]]+[.[:digit:]]*) %\\)?(.*)", "\\3", split.I[2], perl=TRUE)
        value.II <- gsub("([[:digit:]]+)([[:space:]]+)([([:space:]]+)([[:digit:]]+[.[:digit:]]*) %\\)?(.*)", "\\4", split.I[2], perl=TRUE)
        desc.I <- gsub("([[:digit:]]+)([[:space:]]+)([([:space:]]+)([[:digit:]]+[.[:digit:]]*) %\\)?([[:space:]]*)(.*)", "\\6", split.I[2], perl=TRUE)
      }
      TQ.res.I.indexed[[name.I]] <- c(value1=as.numeric(value.I), value2=as.numeric(value.II), desc=desc.I)
    }
  for (this.S in TQ.res.S){
      split.I <- unlist(strsplit(this.S, ":[[:space:]]+"))
      name.I   <- gsub("(-)([[:space:]]+)(S)([[:space:]]+)([[:digit:]]+)", "\\3\\5", split.I[1], perl=TRUE)
      value.I  <- gsub("([[:digit:]]+[.[:digit:]]*)([[:space:]]+)(.*)", "\\1", split.I[2], perl=TRUE)
      if(name.I %in% paste0("S", sprintf("%02d", 1:6))) {
        value.II <- ""
      } else {
        value.II <- gsub("([[:digit:]]+[.[:digit:]]*)([[:space:]]+)([[:digit:]]+[.[:digit:]]*)([[:space:]]+)(.*)", "\\3", split.I[2], perl=TRUE)
      }
      desc.I   <- gsub("([[:digit:]]+[.[:digit:]]*)([[:space:]]+)([[:digit:]]*[.[:digit:]]*)([[:space:]]*)(.*)", "\\5", split.I[2], perl=TRUE)
      TQ.res.S.indexed[[name.I]] <- c(value1=as.numeric(value.I), value2=as.numeric(value.II), desc=desc.I)
    }
  TQ.res.I.indexed <- t(as.data.frame(TQ.res.I.indexed))
  TQ.res.S.indexed <- t(as.data.frame(TQ.res.S.indexed))

  return(list(I=TQ.res.I.indexed, S=TQ.res.S.indexed))
}
