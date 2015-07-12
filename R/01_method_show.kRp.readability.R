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


#' @export
#' @docType methods
#' @aliases show,kRp.readability-method
#' @rdname show-methods
#' @examples
#' \dontrun{
#' flesch(tagged.txt)
#' }
#' @include 00_class_10_kRp.readability.R
#' @include 01_method_show.kRp.lang.R
setMethod("show", signature(object="kRp.readability"), function(object){

  if(sum(!is.na(object@ARI)) == 0){
    show.ARI <- FALSE
  } else {
    prt.ARI.flavour <- object@ARI$flavour
    prt.ARI <- round(object@ARI$grade, digits=2)
    show.ARI <- TRUE
  }
  if(sum(!is.na(object@ARI.NRI)) == 0){
    show.ARI.NRI <- FALSE
  } else {
    prt.ARI.NRI.flavour <- object@ARI.NRI$flavour
    prt.ARI.NRI <- round(object@ARI.NRI$grade, digits=2)
    show.ARI.NRI <- TRUE
  }
  if(sum(!is.na(object@ARI.simple)) == 0){
    show.ARI.simple <- FALSE
  } else {
    prt.ARI.simple.flavour <- object@ARI.simple$flavour
    prt.ARI.simple <- round(object@ARI.simple$index, digits=2)
    show.ARI.simple <- TRUE
  }

  if(sum(!is.na(object@Bormuth)) == 0){
    show.Bormuth <- FALSE
  } else {
    prt.Bormuth.flavour <- object@Bormuth$flavour
    prt.Bormuth.pct.fam <- round(object@Bormuth$pct.fam, digits=0)
    prt.Bormuth.MC <- round(object@Bormuth$mean.cloze, digits=2)
    prt.Bormuth.grade <- round(object@Bormuth$grade, digits=2)
    show.Bormuth <- TRUE
  }

  if(sum(!is.na(object@Coleman)) == 0){
    show.Coleman <- FALSE
  } else {
    prt.Coleman.flavour <- object@Coleman$flavour
    prt.Coleman.pron <- round(object@Coleman$num.pron, digits=2)
    prt.Coleman.prep <- round(object@Coleman$num.prep, digits=2)
    prt.Coleman.C1 <- round(object@Coleman$C1, digits=0)
    prt.Coleman.C2 <- round(object@Coleman$C2, digits=0)
    prt.Coleman.C3 <- round(object@Coleman$C3, digits=0)
    prt.Coleman.C4 <- round(object@Coleman$C4, digits=0)
    show.Coleman <- TRUE
  }

  if(sum(!is.na(object@Coleman.Liau)) == 0){
    show.Coleman.Liau <- FALSE
  } else {
    prt.Coleman.Liau.flavour <- object@Coleman.Liau$flavour
    prt.Coleman.Liau.ECP <- round(object@Coleman.Liau$ECP, digits=0)
    prt.Coleman.Liau.grade <- round(object@Coleman.Liau$grade, digits=2)
    prt.Coleman.Liau.short <- round(object@Coleman.Liau$short, digits=2)
    show.Coleman.Liau <- TRUE
  }

  if(sum(!is.na(object@Dale.Chall)) == 0){
    show.Dale.Chall <- FALSE
  } else {
    prt.Dale.Chall.flavour <- object@Dale.Chall$flavour
    prt.Dale.Chall.pct <- round(object@Dale.Chall$pct, digits=0)
    prt.Dale.Chall.raw <- round(object@Dale.Chall$raw, digits=2)
    prt.Dale.Chall.grade <- object@Dale.Chall$grade
    prt.Dale.Chall.age <- object@Dale.Chall$age
    show.Dale.Chall <- TRUE
  }
  if(sum(!is.na(object@Dale.Chall.PSK)) == 0){
    show.Dale.Chall.PSK <- FALSE
  } else {
    prt.Dale.Chall.PSK.flavour <- object@Dale.Chall.PSK$flavour
    prt.Dale.Chall.PSK.pct <- round(object@Dale.Chall.PSK$pct, digits=0)
    prt.Dale.Chall.PSK.raw <- round(object@Dale.Chall.PSK$raw, digits=2)
    prt.Dale.Chall.PSK.grade <- object@Dale.Chall.PSK$grade
    prt.Dale.Chall.PSK.age <- object@Dale.Chall.PSK$age
    show.Dale.Chall.PSK <- TRUE
  }
  if(sum(!is.na(object@Dale.Chall.old)) == 0){
    show.Dale.Chall.old <- FALSE
  } else {
    prt.Dale.Chall.old.flavour <- object@Dale.Chall.old$flavour
    prt.Dale.Chall.old.pct <- round(object@Dale.Chall.old$pct, digits=0)
    prt.Dale.Chall.old.raw <- round(object@Dale.Chall.old$raw, digits=2)
    prt.Dale.Chall.old.grade <- object@Dale.Chall.old$grade
    prt.Dale.Chall.old.age <- object@Dale.Chall.old$age
    show.Dale.Chall.old <- TRUE
  }

  if(sum(!is.na(object@Danielson.Bryan)) == 0){
    show.Danielson.Bryan <- FALSE
  } else {
    prt.Danielson.Bryan.flavour <- object@Danielson.Bryan$flavour
    prt.DB1 <- round(object@Danielson.Bryan$DB1, digits=2)
    prt.DB2 <- round(object@Danielson.Bryan$DB2, digits=2)
    prt.DB2.grade <- object@Danielson.Bryan$DB2.grade
    show.Danielson.Bryan <- TRUE
  }

  if(sum(!is.na(object@Dickes.Steiwer)) == 0){
    show.Dickes.Steiwer <- FALSE
  } else {
    prt.Dickes.Steiwer.flavour <- object@Dickes.Steiwer$flavour
    prt.Dickes.Steiwer.TTR <- round(object@Dickes.Steiwer$TTR, digits=2)
    prt.Dickes.Steiwer.score <- round(object@Dickes.Steiwer$Dickes.Steiwer, digits=2)
    show.Dickes.Steiwer <- TRUE
  }

  if(sum(!is.na(object@DRP)) == 0){
    show.DRP <- FALSE
  } else {
    prt.DRP.score <- round(object@DRP$DRP, digits=2)
    show.DRP <- TRUE
  }

  if(sum(!is.na(object@ELF)) == 0){
    show.ELF <- FALSE
  } else {
    prt.ELF.flavour <- object@ELF$flavour
    prt.ELF.exsyls <- round(object@ELF$num.exsyls, digits=0)
    prt.ELF.score <- round(object@ELF$ELF, digits=2)
    show.ELF <- TRUE
  }

  if(sum(!is.na(object@Farr.Jenkins.Paterson)) == 0){
    show.Farr.Jenkins.Paterson <- FALSE
  } else {
    prt.Farr.Jenkins.Paterson.flavour <- object@Farr.Jenkins.Paterson$flavour
    prt.Farr.Jenkins.Paterson <- round(object@Farr.Jenkins.Paterson$FJP, digits=2)
    prt.Farr.Jenkins.Paterson.grade <- object@Farr.Jenkins.Paterson$grade
    show.Farr.Jenkins.Paterson <- TRUE
  }
  if(sum(!is.na(object@Farr.Jenkins.Paterson.PSK)) == 0){
    show.Farr.Jenkins.Paterson.PSK <- FALSE
  } else {
    prt.Farr.Jenkins.Paterson.PSK.flavour <- object@Farr.Jenkins.Paterson.PSK$flavour
    prt.Farr.Jenkins.Paterson.PSK <- round(object@Farr.Jenkins.Paterson.PSK$FJP, digits=2)
    show.Farr.Jenkins.Paterson.PSK <- TRUE
  }

  if(sum(!is.na(object@Flesch)) == 0){
    show.Flesch <- FALSE
  } else {
    prt.Flesch.flavour <- object@Flesch$flavour
    prt.Flesch <- round(object@Flesch$RE, digits=2)
    prt.Flesch.grade <- object@Flesch$grade
    prt.Flesch.age <- round(object@Flesch$age, digits=2)
    show.Flesch <- TRUE
  }
  if(sum(!is.na(object@Flesch.PSK)) == 0){
    show.Flesch.PSK <- FALSE
  } else {
    prt.Flesch.PSK.flavour <- object@Flesch.PSK$flavour
    prt.Flesch.PSK <- round(object@Flesch.PSK$RE, digits=2)
    prt.Flesch.PSK.grade <- round(object@Flesch.PSK$grade, digits=2)
    prt.Flesch.PSK.age <- round(object@Flesch.PSK$age, digits=2)
    show.Flesch.PSK <- TRUE
  }
  if(sum(!is.na(object@Flesch.Szigriszt)) == 0){
    show.Flesch.Szigriszt <- FALSE
  } else {
    prt.Flesch.Szigriszt.flavour <- object@Flesch.Szigriszt$flavour
    prt.Flesch.Szigriszt <- round(object@Flesch.Szigriszt$RE, digits=2)
    prt.Flesch.Szigriszt.grade <- object@Flesch.Szigriszt$grade
    prt.Flesch.Szigriszt.age <- round(object@Flesch.Szigriszt$age, digits=2)
    show.Flesch.Szigriszt <- TRUE
  }
  if(sum(!is.na(object@Flesch.de)) == 0){
    show.Flesch.de <- FALSE
  } else {
    prt.Flesch.de.flavour <- object@Flesch.de$flavour
    prt.Flesch.de <- round(object@Flesch.de$RE, digits=2)
    prt.Flesch.de.grade <- object@Flesch.de$grade
    prt.Flesch.de.age <- round(object@Flesch.de$age, digits=2)
    show.Flesch.de <- TRUE
  }
  if(sum(!is.na(object@Flesch.es)) == 0){
    show.Flesch.es <- FALSE
  } else {
    prt.Flesch.es.flavour <- object@Flesch.es$flavour
    prt.Flesch.es <- round(object@Flesch.es$RE, digits=2)
    prt.Flesch.es.grade <- object@Flesch.es$grade
    prt.Flesch.es.age <- round(object@Flesch.es$age, digits=2)
    show.Flesch.es <- TRUE
  }
  if(sum(!is.na(object@Flesch.fr)) == 0){
    show.Flesch.fr <- FALSE
  } else {
    prt.Flesch.fr.flavour <- object@Flesch.fr$flavour
    prt.Flesch.fr <- round(object@Flesch.fr$RE, digits=2)
    prt.Flesch.fr.grade <- object@Flesch.fr$grade
    prt.Flesch.fr.age <- round(object@Flesch.fr$age, digits=2)
    show.Flesch.fr <- TRUE
  }
  if(sum(!is.na(object@Flesch.nl)) == 0){
    show.Flesch.nl <- FALSE
  } else {
    prt.Flesch.nl.flavour <- object@Flesch.nl$flavour
    prt.Flesch.nl <- round(object@Flesch.nl$RE, digits=2)
    prt.Flesch.nl.grade <- object@Flesch.nl$grade
    prt.Flesch.nl.age <- round(object@Flesch.nl$age, digits=2)
    show.Flesch.nl <- TRUE
  }

  if(sum(!is.na(object@Flesch.Kincaid)) == 0){
    show.Flesch.Kincaid <- FALSE
  } else {
    prt.Flesch.Kincaid.flavour <- object@Flesch.Kincaid$flavour
    prt.Flesch.Kincaid.grade <- round(object@Flesch.Kincaid$grade, digits=2)
    prt.Flesch.Kincaid.age <- round(object@Flesch.Kincaid$age, digits=2)
    show.Flesch.Kincaid <- TRUE
  }

  if(sum(!is.na(object@FOG)) == 0){
    show.FOG <- FALSE
  } else {
    prt.FOG.flavour <- object@FOG$flavour
    prt.FOG <- round(object@FOG$FOG, digits=2)
    show.FOG <- TRUE
  }
  if(sum(!is.na(object@FOG.PSK)) == 0){
    show.FOG.PSK <- FALSE
  } else {
    prt.FOG.PSK.flavour <- object@FOG.PSK$flavour
    prt.FOG.PSK <- round(object@FOG.PSK$FOG, digits=2)
    show.FOG.PSK <- TRUE
  }
  if(sum(!is.na(object@FOG.NRI)) == 0){
    show.FOG.NRI <- FALSE
  } else {
    prt.FOG.NRI.flavour <- object@FOG.NRI$flavour
    prt.FOG.NRI <- round(object@FOG.NRI$FOG, digits=2)
    show.FOG.NRI <- TRUE
  }

  if(sum(!is.na(object@FORCAST)) == 0){
    show.FORCAST <- FALSE
  } else {
    prt.FORCAST.flavour <- object@FORCAST$flavour
    prt.FORCAST.grade <- round(object@FORCAST$grade, digits=2)
    prt.FORCAST.age <- round(object@FORCAST$age, digits=2)
    show.FORCAST <- TRUE
  }
  if(sum(!is.na(object@FORCAST.RGL)) == 0){
    show.FORCAST.RGL <- FALSE
  } else {
    prt.FORCAST.RGL.flavour <- object@FORCAST.RGL$flavour
    prt.FORCAST.RGL.grade <- round(object@FORCAST.RGL$grade, digits=2)
    prt.FORCAST.RGL.age <- round(object@FORCAST.RGL$age, digits=2)
    show.FORCAST.RGL <- TRUE
  }

  if(sum(!is.na(object@Fucks)) == 0){
    show.Fucks <- FALSE
  } else {
    prt.Fucks.score <- round(object@Fucks$Fucks, digits=2)
    prt.Fucks.grade <- round(object@Fucks$grade, digits=2)
    show.Fucks <- TRUE
  }

  if(sum(!is.na(object@Harris.Jacobson)) == 0){
    show.Harris.Jacobson <- FALSE
  } else {
    prt.Harris.Jacobson.flavour <- object@Harris.Jacobson$flavour
    prt.Harris.Jacobson.pct <- round(object@Harris.Jacobson$pct, digits=0)
    prt.HJ1 <- round(object@Harris.Jacobson$HJ1, digits=2)
    prt.HJ2 <- round(object@Harris.Jacobson$HJ2, digits=2)
    prt.HJ3 <- round(object@Harris.Jacobson$HJ3, digits=2)
    prt.HJ4 <- round(object@Harris.Jacobson$HJ4, digits=2)
    prt.HJ5 <- round(object@Harris.Jacobson$HJ5, digits=2)
    show.Harris.Jacobson <- TRUE
  }

  if(sum(!is.na(object@Linsear.Write)) == 0){
    show.Linsear.Write <- FALSE
  } else {
    prt.Linsear.Write.flavour <- object@Linsear.Write$flavour
    prt.Linsear.Write.easy <- round(object@Linsear.Write$easy.words, digits=2)
    prt.Linsear.Write.hard <- round(object@Linsear.Write$hard.words, digits=2)
    prt.Linsear.Write <- round(object@Linsear.Write$grade, digits=2)
    show.Linsear.Write <- TRUE
  }

  if(sum(!is.na(object@LIX)) == 0){
    show.LIX <- FALSE
  } else {
    prt.LIX.flavour <- object@LIX$flavour
    prt.LIX <- round(object@LIX$index, digits=2)
    prt.LIX.grade <- object@LIX$grade
    prt.LIX.rating <- object@LIX$rating
    show.LIX <- TRUE
  }

  if(sum(!is.na(object@RIX)) == 0){
    show.RIX <- FALSE
  } else {
    prt.RIX.flavour <- object@RIX$flavour
    prt.RIX <- round(object@RIX$index, digits=2)
    prt.RIX.grade <- object@RIX$grade
    show.RIX <- TRUE
  }

  if(sum(!is.na(object@SMOG)) == 0){
    show.SMOG <- FALSE
  } else {
    prt.SMOG.flavour <- object@SMOG$flavour
    prt.SMOG.grade <- round(object@SMOG$grade, digits=2)
    prt.SMOG.age <- round(object@SMOG$age, digits=2)
    show.SMOG <- TRUE
  }
  if(sum(!is.na(object@SMOG.de)) == 0){
    show.SMOG.de <- FALSE
  } else {
    prt.SMOG.de.flavour <- object@SMOG.de$flavour
    prt.SMOG.de.grade <- round(object@SMOG.de$grade, digits=2)
    prt.SMOG.de.age <- round(object@SMOG.de$age, digits=2)
    show.SMOG.de <- TRUE
  }
  if(sum(!is.na(object@SMOG.C)) == 0){
    show.SMOG.C <- FALSE
  } else {
    prt.SMOG.C.flavour <- object@SMOG.C$flavour
    prt.SMOG.C.grade <- round(object@SMOG.C$grade, digits=2)
    prt.SMOG.C.age <- round(object@SMOG.C$age, digits=2)
    show.SMOG.C <- TRUE
  }
  if(sum(!is.na(object@SMOG.simple)) == 0){
    show.SMOG.simple <- FALSE
  } else {
    prt.SMOG.simple.flavour <- object@SMOG.simple$flavour
    prt.SMOG.simple.grade <- round(object@SMOG.simple$grade, digits=2)
    prt.SMOG.simple.age <- round(object@SMOG.simple$age, digits=2)
    show.SMOG.simple <- TRUE
  }

  if(sum(!is.na(object@Spache)) == 0){
    show.Spache <- FALSE
  } else {
    prt.Spache.flavour <- object@Spache$flavour
    prt.Spache.pct <- round(object@Spache$pct, digits=0)
    prt.Spache.grade <- round(object@Spache$grade, digits=2)
    show.Spache <- TRUE
  }
  if(sum(!is.na(object@Spache.old)) == 0){
    show.Spache.old <- FALSE
  } else {
    prt.Spache.old.flavour <- object@Spache.old$flavour
    prt.Spache.old.pct <- round(object@Spache.old$pct, digits=0)
    prt.Spache.old.grade <- round(object@Spache.old$grade, digits=2)
    show.Spache.old <- TRUE
  }

  if(sum(!is.na(object@Strain)) == 0){
    show.Strain <- FALSE
  } else {
    prt.Strain.flavour <- object@Strain$flavour
    prt.Strain <- round(object@Strain$index, digits=2)
    show.Strain <- TRUE
  }

  if(sum(!is.na(object@Traenkle.Bailer)) == 0){
    show.Traenkle.Bailer <- FALSE
  } else {
    prt.Traenkle.Bailer.flavour <- object@Traenkle.Bailer$flavour
    prt.Traenkle.Bailer.TB1 <- round(object@Traenkle.Bailer$TB1, digits=2)
    prt.Traenkle.Bailer.TB2 <- round(object@Traenkle.Bailer$TB2, digits=2)
    prt.Traenkle.Bailer.pct.prep <- round(object@Traenkle.Bailer$pct.prep, digits=0)
    prt.Traenkle.Bailer.pct.conj <- round(object@Traenkle.Bailer$pct.conj, digits=0)
    show.Traenkle.Bailer <- TRUE
  }

  if(sum(!is.na(object@TRI)) == 0){
    show.TRI <- FALSE
  } else {
    prt.TRI.flavour <- object@TRI$flavour
    prt.TRI.words <- object@TRI$short
    prt.TRI.punct <- object@TRI$punct
    prt.TRI.foreign <- object@TRI$foreign
    prt.TRI.score <- round(object@TRI$TRI, digits=2)
    show.TRI <- TRUE
  }

  if(sum(!is.na(object@Tuldava)) == 0){
    show.Tuldava <- FALSE
  } else {
    prt.Tuldava.flavour <- object@Tuldava$flavour
    prt.Tuldava.index <- round(object@Tuldava$Tuldava, digits=2)
    show.Tuldava <- TRUE
  }

  if(sum(!is.na(object@Wheeler.Smith)) == 0){
    show.Wheeler.Smith <- FALSE
  } else {
    prt.Wheeler.Smith.flavour <- object@Wheeler.Smith$flavour
    prt.Wheeler.Smith.score <- round(object@Wheeler.Smith$score, digits=2)
    prt.Wheeler.Smith.grade <- object@Wheeler.Smith$grade
    show.Wheeler.Smith <- TRUE
  }
  if(sum(!is.na(object@Wheeler.Smith.de)) == 0){
    show.Wheeler.Smith.de <- FALSE
  } else {
    prt.Wheeler.Smith.de.flavour <- object@Wheeler.Smith.de$flavour
    prt.Wheeler.Smith.de.score <- round(object@Wheeler.Smith.de$score, digits=2)
    prt.Wheeler.Smith.de.grade <- object@Wheeler.Smith.de$grade
    show.Wheeler.Smith.de <- TRUE
  }

  if(sum(!is.na(object@Wiener.STF)) == 0){
    show.Wiener.STF <- FALSE
  } else {
    prt.Wiener.STF.flavour <- object@Wiener.STF$flavour
    prt.WSTF1 <- round(object@Wiener.STF$nWS1, digits=2)
    prt.WSTF2 <- round(object@Wiener.STF$nWS2, digits=2)
    prt.WSTF3 <- round(object@Wiener.STF$nWS3, digits=2)
    prt.WSTF4 <- round(object@Wiener.STF$nWS4, digits=2)
    show.Wiener.STF <- TRUE
  }

  ## here we go:
#   if(show.){
#     cat("\n\n\n")
#     cat("  Parameters:", prt..flavour, "\n")
#     cat("   :", prt., "\n")
#    cat("       Index:", prt., "\n")
#    cat("       Grade:", prt., "\n")
#    cat("         Age:", prt., "\n")
#  } else {}

  if(show.ARI){
    cat("\nAutomated Readability Index (ARI)\n")
    cat("  Parameters:", prt.ARI.flavour, "\n")
    cat("       Grade:", prt.ARI, "\n\n")
  } else {}
  if(show.ARI.NRI){
    cat("\nAutomated Readability Index (ARI)\n")
    cat("  Parameters:", prt.ARI.NRI.flavour, "\n")
    cat("       Grade:", prt.ARI.NRI, "\n\n")
  } else {}
  if(show.ARI.simple){
    cat("\nAutomated Readability Index (ARI)\n")
    cat("  Parameters:", prt.ARI.simple.flavour, "\n")
    cat("       Index:", prt.ARI.simple, "\n\n")
  } else {}

  if(show.Bormuth){
    cat("\nBormuth Mean Cloze\n")
    cat("  Parameters:", prt.Bormuth.flavour, "\n")
    cat("  Fam. words: ", prt.Bormuth.pct.fam, "%\n", sep="")
    cat("  Mean Cloze:", prt.Bormuth.MC, "\n")
    if(!is.na(prt.Bormuth.grade)){
    cat("       Grade:", prt.Bormuth.grade, "\n")
    } else {}
    cat("\n")
  } else {}

  if(show.Coleman){
    cat("\nColeman Formulas\n")
    cat("  Parameters:", prt.Coleman.flavour, "\n")
    if(!is.na(prt.Coleman.pron)){
    cat("    Pronouns:", prt.Coleman.pron, "(per 100 words)\n")
    } else {}
    if(!is.na(prt.Coleman.prep)){
    cat("     Prepos.:", prt.Coleman.prep, "(per 100 words)\n")
    } else {}
    if(!is.na(prt.Coleman.C1)){
    cat("   Formula 1: ", prt.Coleman.C1, "% cloze completions\n", sep="")
    } else {}
    if(!is.na(prt.Coleman.C2)){
    cat("   Formula 2: ", prt.Coleman.C2, "% cloze completions\n", sep="")
    } else {}
    if(!is.na(prt.Coleman.C3)){
    cat("   Formula 3: ", prt.Coleman.C3, "% cloze completions\n", sep="")
    } else {}
    if(!is.na(prt.Coleman.C4)){
    cat("   Formula 4: ", prt.Coleman.C4, "% cloze completions\n", sep="")
    } else {}
    cat("\n")
  } else {}

  if(show.Coleman.Liau){
    cat("\nColeman-Liau\n")
    cat("  Parameters:", prt.Coleman.Liau.flavour, "\n")
    if(!is.na(prt.Coleman.Liau.grade)){
    cat("         ECP: ", prt.Coleman.Liau.ECP, "% (estimted cloze percentage)\n", sep="")
    cat("       Grade:", prt.Coleman.Liau.grade, "\n")
    } else {}
    if(!is.na(prt.Coleman.Liau.short)){
    cat("       Grade:", prt.Coleman.Liau.short, "(short formula)\n")
    } else {}
    cat("\n")
  } else {}

  if(show.Dale.Chall){
    cat("\nDale-Chall Readability Formula\n")
    cat("  Parameters:", prt.Dale.Chall.flavour, "\n")
    cat(" Not on list: ", prt.Dale.Chall.pct, "%\n", sep="")
    cat("   Raw value:", prt.Dale.Chall.raw, "\n")
    cat("       Grade:", prt.Dale.Chall.grade, "\n")
    cat("         Age:", prt.Dale.Chall.age, "\n\n")
  } else {}
  if(show.Dale.Chall.PSK){
    cat("\nDale-Chall Readability Formula\n")
    cat("  Parameters:", prt.Dale.Chall.PSK.flavour, "\n")
    cat(" Not on list: ", prt.Dale.Chall.PSK.pct, "%\n", sep="")
    cat("   Raw value:", prt.Dale.Chall.PSK.raw, "\n")
    cat("       Grade:", prt.Dale.Chall.PSK.grade, "\n")
    cat("         Age:", prt.Dale.Chall.PSK.age, "\n\n")
  } else {}
  if(show.Dale.Chall.old){
    cat("\nDale-Chall Readability Formula\n")
    cat("  Parameters:", prt.Dale.Chall.old.flavour, "\n")
    cat(" Not on list: ", prt.Dale.Chall.old.pct, "%\n", sep="")
    cat("   Raw value:", prt.Dale.Chall.old.raw, "\n")
    cat("       Grade:", prt.Dale.Chall.old.grade, "\n")
    cat("         Age:", prt.Dale.Chall.old.age, "\n\n")
  } else {}

  if(show.Danielson.Bryan){
    cat("\nDanielson-Bryan\n")
    cat("  Parameters:", prt.Danielson.Bryan.flavour, "\n")
    if(!is.na(prt.DB1)){
    cat("         DB1:", prt.DB1, "\n")
    } else {}
    if(!is.na(prt.DB2)){
    cat("         DB2:", prt.DB2, "\n")
    } else {}
    if(!is.na(prt.DB2.grade)){
    cat("       Grade:", prt.DB2.grade, "\n")
    } else {}
    cat("\n")
  } else {}

  if(show.Dickes.Steiwer){
    cat("\nDickes-Steiwer's Handformel\n")
    cat("  Parameters:", prt.Dickes.Steiwer.flavour, "\n")
    cat("         TTR:", prt.Dickes.Steiwer.TTR, "\n")
    cat("       Score:", prt.Dickes.Steiwer.score, "\n\n")
  } else {}

  if(show.DRP){
    cat("\nDegrees of Reading Power\n")
    cat("         DRP:", prt.DRP.score, "\n\n")
  } else {}

  if(show.ELF){
    cat("\nEasy Listening Formula\n")
    cat("  Parameters:", prt.ELF.flavour, "\n")
    cat("      Exsyls:", prt.ELF.exsyls, "\n")
    cat("       Score:", prt.ELF.score, "\n\n")
  } else {}

  if(show.Farr.Jenkins.Paterson){
    cat("\nFarr-Jenkins-Paterson\n")
    cat("  Parameters:", prt.Farr.Jenkins.Paterson.flavour, "\n")
    cat("          RE:", prt.Farr.Jenkins.Paterson, "\n")
    cat("       Grade:", prt.Farr.Jenkins.Paterson.grade, "\n\n")
  } else {}
  if(show.Farr.Jenkins.Paterson.PSK){
    cat("\nFarr-Jenkins-Paterson\n")
    cat("  Parameters:", prt.Farr.Jenkins.Paterson.PSK.flavour, "\n")
    cat("       Grade:", prt.Farr.Jenkins.Paterson.PSK, "\n\n")
  } else {}

  if(show.Flesch){
    cat("\nFlesch Reading Ease\n")
    cat("  Parameters:", prt.Flesch.flavour, "\n")
    if(!is.na(prt.Flesch)){
    cat("          RE:", prt.Flesch, "\n")
    } else {}
    if(!is.na(prt.Flesch.grade)){
    cat("       Grade:", prt.Flesch.grade, "\n")
    } else {}
    if(!is.na(prt.Flesch.age)){
    cat("         Age:", prt.Flesch.age, "\n")
    } else {}
    cat("\n")
  } else {}
  if(show.Flesch.PSK){
    cat("\nFlesch-PSK Reading Ease\n")
    cat("  Parameters:", prt.Flesch.PSK.flavour, "\n")
    if(!is.na(prt.Flesch.PSK)){
    cat("          RE:", prt.Flesch.PSK, "\n")
    } else {}
    if(!is.na(prt.Flesch.PSK.grade)){
    cat("       Grade:", prt.Flesch.PSK.grade, "\n")
    } else {}
    if(!is.na(prt.Flesch.PSK.age)){
    cat("         Age:", prt.Flesch.PSK.age, "\n")
    } else {}
    cat("\n")
  } else {}
  if(show.Flesch.Szigriszt){
    cat("\nFlesch-Szigriszt Reading Ease (es)\n")
    cat("  Parameters:", prt.Flesch.Szigriszt.flavour, "\n")
    if(!is.na(prt.Flesch.Szigriszt)){
    cat("          RE:", prt.Flesch.Szigriszt, "\n")
    } else {}
    if(!is.na(prt.Flesch.Szigriszt.grade)){
    cat("       Grade:", prt.Flesch.Szigriszt.grade, "\n")
    } else {}
    if(!is.na(prt.Flesch.Szigriszt.age)){
    cat("         Age:", prt.Flesch.Szigriszt.age, "\n")
    } else {}
    cat("\n")
  } else {}
  if(show.Flesch.de){
    cat("\nFlesch.de Reading Ease\n")
    cat("  Parameters:", prt.Flesch.de.flavour, "\n")
    if(!is.na(prt.Flesch.de)){
    cat("          RE:", prt.Flesch.de, "\n")
    } else {}
    if(!is.na(prt.Flesch.de.grade)){
    cat("       Grade:", prt.Flesch.de.grade, "\n")
    } else {}
    if(!is.na(prt.Flesch.de.age)){
    cat("         Age:", prt.Flesch.de.age, "\n")
    } else {}
    cat("\n")
  } else {}
  if(show.Flesch.es){
    cat("\nFlesch.es Reading Ease\n")
    cat("  Parameters:", prt.Flesch.es.flavour, "\n")
    if(!is.na(prt.Flesch.es)){
    cat("          RE:", prt.Flesch.es, "\n")
    } else {}
    if(!is.na(prt.Flesch.es.grade)){
    cat("       Grade:", prt.Flesch.es.grade, "\n")
    } else {}
    if(!is.na(prt.Flesch.es.age)){
    cat("         Age:", prt.Flesch.es.age, "\n")
    } else {}
    cat("\n")
  } else {}
  if(show.Flesch.fr){
    cat("\nFlesch.fr Reading Ease\n")
    cat("  Parameters:", prt.Flesch.fr.flavour, "\n")
    if(!is.na(prt.Flesch.fr)){
    cat("          RE:", prt.Flesch.fr, "\n")
    } else {}
    if(!is.na(prt.Flesch.fr.grade)){
    cat("       Grade:", prt.Flesch.fr.grade, "\n")
    } else {}
    if(!is.na(prt.Flesch.fr.age)){
    cat("         Age:", prt.Flesch.fr.age, "\n")
    } else {}
    cat("\n")
  } else {}
  if(show.Flesch.nl){
    cat("\nFlesch.nl Reading Ease\n")
    cat("  Parameters:", prt.Flesch.nl.flavour, "\n")
    if(!is.na(prt.Flesch.nl)){
    cat("          RE:", prt.Flesch.nl, "\n")
    } else {}
    if(!is.na(prt.Flesch.nl.grade)){
    cat("       Grade:", prt.Flesch.nl.grade, "\n")
    } else {}
    if(!is.na(prt.Flesch.nl.age)){
    cat("         Age:", prt.Flesch.nl.age, "\n")
    } else {}
    cat("\n")
  } else {}

  if(show.Flesch.Kincaid){
    cat("\nFlesch-Kincaid Grade Level\n")
    cat("  Parameters:", prt.Flesch.Kincaid.flavour, "\n")
    cat("       Grade:", prt.Flesch.Kincaid.grade, "\n")
    cat("         Age:", prt.Flesch.Kincaid.age, "\n\n")
  } else {}

  if(show.FOG){
    cat("\nGunning Frequency of Gobbledygook (FOG)\n")
    cat("  Parameters:", prt.FOG.flavour, "\n")
    cat("       Grade:", prt.FOG, "\n\n")
  } else {}
  if(show.FOG.PSK){
    cat("\nGunning Frequency of Gobbledygook (FOG)\n")
    cat("  Parameters:", prt.FOG.PSK.flavour, "\n")
    cat("       Grade:", prt.FOG.PSK, "\n\n")
  } else {}
  if(show.FOG.NRI){
    cat("\nGunning Frequency of Gobbledygook (FOG)\n")
    cat("  Parameters:", prt.FOG.NRI.flavour, "\n")
    cat("       Grade:", prt.FOG.NRI, "\n\n")
  } else {}

  if(show.FORCAST){
    cat("\nFORCAST\n")
    cat("  Parameters:", prt.FORCAST.flavour, "\n")
    cat("       Grade:", prt.FORCAST.grade, "\n")
    cat("         Age:", prt.FORCAST.age, "\n\n")
  } else {}
  if(show.FORCAST.RGL){
    cat("\nFORCAST\n")
    cat("  Parameters:", prt.FORCAST.RGL.flavour, "\n")
    cat("       Grade:", prt.FORCAST.RGL.grade, "\n")
    cat("         Age:", prt.FORCAST.RGL.age, "\n\n")
  } else {}

  if(show.Fucks){
    cat("\nFucks' Stilcharakteristik\n")
    cat("       Score:", prt.Fucks.score, "\n")
    cat("       Grade:", prt.Fucks.grade, "\n\n")
  } else {}

  if(show.Harris.Jacobson){
    cat("\nHarris-Jacobson\n")
    cat("  Parameters:", prt.Harris.Jacobson.flavour, "\n")
    cat(" Not on list: ", prt.Harris.Jacobson.pct, "%\n", sep="")
    if(!is.na(prt.HJ1)){
    cat("        HJ 1:", prt.HJ1, "\n")
    } else {}
    if(!is.na(prt.HJ2)){
    cat("        HJ 2:", prt.HJ2, "\n")
    } else {}
    if(!is.na(prt.HJ3)){
    cat("        HJ 3:", prt.HJ3, "\n")
    } else {}
    if(!is.na(prt.HJ4)){
    cat("        HJ 4:", prt.HJ4, "\n")
    } else {}
    if(!is.na(prt.HJ5)){
    cat("        HJ 5:", prt.HJ5, "\n")
    } else {}
    cat("\n")
  } else {}

  if(show.Linsear.Write){
    cat("\nLinsear Write\n")
    cat("  Parameters:", prt.Linsear.Write.flavour, "\n")
    cat("  Easy words:", prt.Linsear.Write.easy, "\n")
    cat("  Hard words:", prt.Linsear.Write.hard, "\n")
    cat("       Grade:", prt.Linsear.Write, "\n\n")
  } else {}

  if(show.LIX){
    cat("\nL\u00e4sbarhetsindex (LIX)\n")
    cat("  Parameters:", prt.LIX.flavour, "\n")
    cat("       Index:", prt.LIX, "\n")
    cat("      Rating:", prt.LIX.rating, "\n")
    cat("       Grade:", prt.LIX.grade, "\n\n")
  } else {}

  if(show.Wiener.STF){
    cat("\nNeue Wiener Sachtextformeln\n")
    cat("  Parameters:", prt.Wiener.STF.flavour, "\n")
    if(!is.na(prt.WSTF1)){
    cat("       nWS 1:", prt.WSTF1, "\n")
    } else {}
    if(!is.na(prt.WSTF2)){
    cat("       nWS 2:", prt.WSTF2, "\n")
    } else {}
    if(!is.na(prt.WSTF3)){
    cat("       nWS 3:", prt.WSTF3, "\n")
    } else {}
    if(!is.na(prt.WSTF4)){
    cat("       nWS 4:", prt.WSTF4, "\n")
    } else {}
    cat("\n")
  } else {}

  if(show.RIX){
    cat("\nReadability Index (RIX)\n")
    cat("  Parameters:", prt.RIX.flavour, "\n")
    cat("       Index:", prt.RIX, "\n")
    cat("       Grade:", prt.RIX.grade, "\n\n")
  } else {}

  if(show.SMOG){
    cat("\nSimple Measure of Gobbledygook (SMOG)\n")
    cat("  Parameters:", prt.SMOG.flavour, "\n")
    cat("       Grade:", prt.SMOG.grade, "\n")
    cat("         Age:", prt.SMOG.age, "\n\n")
  } else {}
  if(show.SMOG.de){
    cat("\nSimple Measure of Gobbledygook (SMOG)\n")
    cat("  Parameters:", prt.SMOG.de.flavour, "\n")
    cat("       Grade:", prt.SMOG.de.grade, "\n")
    cat("         Age:", prt.SMOG.de.age, "\n\n")
  } else {}
  if(show.SMOG.C){
    cat("\nSimple Measure of Gobbledygook (SMOG)\n")
    cat("  Parameters:", prt.SMOG.C.flavour, "\n")
    cat("       Grade:", prt.SMOG.C.grade, "\n")
    cat("         Age:", prt.SMOG.C.age, "\n\n")
  } else {}
  if(show.SMOG.simple){
    cat("\nSimple Measure of Gobbledygook (SMOG)\n")
    cat("  Parameters:", prt.SMOG.simple.flavour, "\n")
    cat("       Grade:", prt.SMOG.simple.grade, "\n")
    cat("         Age:", prt.SMOG.simple.age, "\n\n")
  } else {}

  if(show.Spache){
    cat("\nSpache Formula\n")
    cat("  Parameters:", prt.Spache.flavour, "\n")
    cat(" Not on list: ", prt.Spache.pct, "%\n", sep="")
    cat("       Grade:", prt.Spache.grade, "\n\n")
  } else {}
  if(show.Spache.old){
    cat("\nSpache Formula\n")
    cat("  Parameters:", prt.Spache.old.flavour, "\n")
    cat(" Not on list: ", prt.Spache.old.pct, "%\n", sep="")
    cat("       Grade:", prt.Spache.old.grade, "\n\n")
  } else {}

  if(show.Strain){
    cat("\nStrain Index\n")
    cat("  Parameters:", prt.Strain.flavour, "\n")
    cat("       Index:", prt.Strain, "\n\n")
  } else {}

  if(show.Traenkle.Bailer){
    cat("\nTr\u00e4nkle-Bailer Formulas\n")
    cat("   Parameters:", prt.Traenkle.Bailer.flavour, "\n")
    cat(" Prepositions: ", prt.Traenkle.Bailer.pct.prep, "%\n", sep="")
    cat(" Conjunctions: ", prt.Traenkle.Bailer.pct.conj, "%\n", sep="")
    cat("         TB 1:", prt.Traenkle.Bailer.TB1, "\n")
    cat("         TB 2:", prt.Traenkle.Bailer.TB2, "\n\n")
  } else {}

  if(show.TRI){
    cat("\nKuntzsch's Text-Redundanz-Index\n")
    cat("  Parameters:", prt.TRI.flavour, "\n")
    cat(" Short words:", prt.TRI.words, "\n")
    cat(" Punctuation:", prt.TRI.punct, "\n")
    cat("     Foreign:", prt.TRI.foreign, "\n")
    cat("       Score:", prt.TRI.score, "\n\n")
  } else {}


  if(show.Tuldava){
    cat("\nTuldava's Text Difficulty Formula\n")
    cat("  Parameters:", prt.Tuldava.flavour, "\n")
    cat("       Index:", prt.Tuldava.index, "\n\n")
  } else {}

  if(show.Wheeler.Smith){
    cat("\nWheeler-Smith\n")
    cat("  Parameters:", prt.Wheeler.Smith.flavour, "\n")
    cat("       Score:", prt.Wheeler.Smith.score, "\n")
    cat("       Grade:", prt.Wheeler.Smith.grade, "\n\n")
  } else {}
  if(show.Wheeler.Smith.de){
    cat("\nWheeler-Smith\n")
    cat("  Parameters:", prt.Wheeler.Smith.de.flavour, "\n")
    cat("       Score:", prt.Wheeler.Smith.de.score, "\n")
    cat("       Grade:", prt.Wheeler.Smith.de.grade, "\n\n")
  } else {}


   if(length(object@lang) > 0){
     cat("Text language:", object@lang, "\n")
   } else {}

})
