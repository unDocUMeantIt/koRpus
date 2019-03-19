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


#' @export
#' @docType methods
#' @aliases show,kRp.TTR-method
#' @rdname show-methods
#' @examples
#' \dontrun{
#' MTLD(tagged.txt)
#' }
#' @include 01_class_02_kRp.TTR.R
#' @include 02_method_show.kRp.lang.R
setMethod("show", signature(object="kRp.TTR"), function(object){

  if(length(slot(object, "tt")[["num.tokens"]]) > 0){
    cat("\nTotal number of tokens:", slot(object, "tt")[["num.tokens"]])
    cat("\nTotal number of types: ", slot(object, "tt")[["num.types"]])
    if(isTRUE(slot(object, "tt")[["num.lemmas"]] > 0)){
      cat("\nTotal number of lemmas:", slot(object, "tt")[["num.lemmas"]])
    } else {}
  } else {}


  if(length(slot(object, "TTR")) > 0){
    cat("\n\nType-Token Ratio\n")
    cat("   TTR:", round(slot(object, "TTR"), digits=2), "\n")
  } else {}
  if(sum(!is.na(slot(object, "TTR.char"))) > 0){
    cat("\nTTR characteristics:\n")
    noInf.summary(slot(object, "TTR.char")[,"value"], add.sd=TRUE)
  } else {}


  if(!is.na(slot(object, "MSTTR")[["MSTTR"]])){
    prt.dropped <- slot(object, "MSTTR")[["dropped"]]
    cat("\n\nMean Segmental Type-Token Ratio\n")
    cat("               MSTTR:", round(slot(object, "MSTTR")[["MSTTR"]], digits=2))
    cat("\n          SD of TTRs:", round(slot(object, "MSTTR")[["sd"]], digits=2))
    cat("\n        Segment size:", slot(object, "param")[["segment"]])
    cat("\n      Tokens dropped:", prt.dropped, "\n")

    if(prt.dropped > 0){
      optimized.MSTTR <- segment.optimizer(slot(object, "tt")[["num.tokens"]], segment=slot(object, "param")[["segment"]])
      if(prt.dropped > optimized.MSTTR["drop"]) {
        hint.MSTTR <- paste("\nHint: A segment size of ", optimized.MSTTR["seg"], " would reduce the drop rate to ", optimized.MSTTR["drop"],
          ".\n      Maybe try ?segment.optimizer()\n", sep="")
        cat(hint.MSTTR)
      } else {}
    } else {}
  } else {}


  if(!is.na(slot(object, "MATTR")[["MATTR"]])){
    cat("\n\nMoving-Average Type-Token Ratio\n")
    cat("               MATTR:", round(slot(object, "MATTR")[["MATTR"]], digits=2))
    cat("\n          SD of TTRs:", round(slot(object, "MATTR")[["sd"]], digits=2))
    cat("\n         Window size:", slot(object, "param")[["window"]], "\n")
  } else {}
  if(sum(!is.na(slot(object, "MATTR.char"))) > 0){
    cat("\nMATTR characteristics:\n")
    noInf.summary(slot(object, "MATTR.char")[,"value"], add.sd=TRUE)
  } else {}


  if(length(slot(object, "C.ld")) > 0){
    cat("\n\nHerdan's C\n")
    cat("   C:", round(slot(object, "C.ld"), digits=2), "\n")
  } else {}
  if(sum(!is.na(slot(object, "C.char"))) > 0){
    cat("\nC characteristics:\n")
    noInf.summary(slot(object, "C.char")[,"value"], add.sd=TRUE)
  } else {}


  if(length(slot(object, "R.ld")) > 0){
    cat("\n\nGuiraud's R\n")
    cat("   R:", round(slot(object, "R.ld"), digits=2), "\n")
  } else {}
  if(sum(!is.na(slot(object, "R.char"))) > 0){
    cat("\nR characteristics:\n")
    noInf.summary(slot(object, "R.char")[,"value"], add.sd=TRUE)
  } else {}


  if(length(slot(object, "CTTR")) > 0){
    cat("\n\nCarroll's CTTR\n")
    cat("   CTTR:", round(slot(object, "CTTR"), digits=2), "\n")
  } else {}
  if(sum(!is.na(slot(object, "CTTR.char"))) > 0){
    cat("\nCTTR characteristics:\n")
    noInf.summary(slot(object, "CTTR.char")[,"value"], add.sd=TRUE)
  } else {}


  if(length(slot(object, "U.ld")) > 0){
    cat("\n\nUber Index\n")
    cat("   U:", round(slot(object, "U.ld"), digits=2), "\n")
  } else {}
  if(sum(!is.na(slot(object, "U.char"))) > 0){
    cat("\nU characteristics:\n")
    noInf.summary(slot(object, "U.char")[,"value"], add.sd=TRUE)
  } else {}


  if(length(slot(object, "S.ld")) > 0){
    cat("\n\nSummer's S\n")
    cat("   S:", round(slot(object, "S.ld"), digits=2), "\n")
  } else {}
  if(sum(!is.na(slot(object, "S.char"))) > 0){
    cat("\nS characteristics:\n")
    noInf.summary(slot(object, "S.char")[,"value"], add.sd=TRUE)
  } else {}


  if(length(slot(object, "K.ld")) > 0){
    cat("\n\nYule's K\n")
    cat("   K:", round(slot(object, "K.ld"), digits=2), "\n")
  } else {}
  if(sum(!is.na(slot(object, "K.char"))) > 0){
    cat("\nK characteristics:\n")
    noInf.summary(slot(object, "K.char")[,"value"], add.sd=TRUE)
  } else {}


  if(length(slot(object, "Maas")) > 0){
    cat("\n\nMaas' Indices\n")
    cat("       a:", round(slot(object, "Maas"), digits=2), "\n")
    if(length(slot(object, "lgV0")) > 0){
      cat("    lgV0:", round(slot(object, "lgV0"), digits=2), "\n")
    } else {}
    if(length(slot(object, "lgeV0")) > 0){
      cat("   lgeV0:", round(slot(object, "lgeV0"), digits=2), "\n")
    } else {}
    if(!all(is.na(slot(object, "Maas.grw")))){
      cat("\nRelative vocabulary growth (first half to full text)\n")
      cat("       a:", round(slot(object, "Maas.grw")[["a"]], digits=2), "\n")
      cat("    lgV0:", round(slot(object, "Maas.grw")[["lgV0"]], digits=2), "\n")
      cat("      V': ", round(slot(object, "Maas.grw")[["Vs"]], digits=2), " (", round(slot(object, "Maas.grw")[["Vs"]] * 100)," new types every 100 tokens)\n", sep="")
    } else {}
  } else {}
  if(sum(!is.na(slot(object, "Maas.char"))) > 0){
    cat("\nMaas Indices characteristics:\n")
    noInf.summary(slot(object, "Maas.char")[,"value"], add.sd=TRUE)
    noInf.summary(slot(object, "lgV0.char")[,"value"], add.sd=TRUE)
    noInf.summary(slot(object, "lgeV0.char")[,"value"], add.sd=TRUE)
  } else {}


  if(!is.na(slot(object, "HDD")[["HDD"]])){
    cat("\n\nHD-D\n")
    cat("          HD-D:", round(slot(object, "HDD")[["HDD"]], digits=2))
    cat("\n          ATTR:", round(slot(object, "HDD")[["ATTR"]], digits=2))
    cat("\n   Sample size:", slot(object, "param")[["rand.sample"]], "\n")
  } else {}
  if(sum(!is.na(slot(object, "HDD.char"))) > 0){
    cat("\nHD-D characteristics:\n")
    noInf.summary(slot(object, "HDD.char")[,"value"], add.sd=TRUE)
  } else {}


  if(!is.na(slot(object, "MTLD")[["MTLD"]])){
    cat("\n\nMeasure of Textual Lexical Diversity\n")
    cat("                MTLD:", round(slot(object, "MTLD")[["MTLD"]], digits=2))
    cat("\n   Number of factors:", round(slot(object, "MTLD")[["factors"]]["mean"], digits=2))
    cat("\n         Factor size:", round(slot(object, "param")[["factor.size"]], digits=2))
    cat("\n    SD tokens/factor:", round(slot(object, "MTLD")[["lengths"]][["sd"]], digits=2), "(all factors)")
    cat("\n                     ", round(slot(object, "MTLD")[["lengths"]][["sd.compl"]], digits=2), "(complete factors only)\n")
  } else {}
  if(sum(!is.na(slot(object, "MTLD.char"))) > 0){
    cat("\nMTLD characteristics:\n")
    noInf.summary(slot(object, "MTLD.char")[,"value"], add.sd=TRUE)
  } else {}

  
  if(!is.na(slot(object, "MTLDMA")[["MTLDMA"]])){
    cat("\n\nMoving-Average Measure of Textual Lexical Diversity\n")
    cat("             MTLD-MA:", round(slot(object, "MTLDMA")[["MTLDMA"]], digits=2))
    cat("\n    SD tokens/factor:", round(slot(object, "MTLDMA")[["sd"]], digits=2))
    cat("\n           Step size:", round(slot(object, "MTLDMA")[["steps"]], digits=0))
    cat("\n         Factor size:", round(slot(object, "param")[["factor.size"]], digits=2))
    cat("\n         Min. tokens:", round(slot(object, "param")[["min.tokens"]], digits=0), "\n")
  } else {}
  if(sum(!is.na(slot(object, "MTLDMA.char"))) > 0){
    cat("\nMTLD-MA characteristics:\n")
    noInf.summary(slot(object, "MTLDMA.char")[,"value"], add.sd=TRUE)
  } else {}


  # notes for special treatments 
  if(!is.na(slot(object, "param")[["case.sens"]]) & !isTRUE(slot(object, "param")[["case.sens"]])){
    message("\nNote: Analysis was conducted case insensitive.")
  } else {}

  if(isTRUE(slot(object, "param")[["lemmatize"]])){
    message("\nNote: Analysis was conducted with lemmatized tokens.")
  } else {}

})
