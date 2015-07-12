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
#' @rdname summary-methods
#' @aliases summary,kRp.TTR-method
#' @examples
#' \dontrun{
#' summary(lex.div(tagged.txt))
#' }
#' @include 00_class_02_kRp.TTR.R
#' @include 01_method_summary.kRp.lang.R
setMethod("summary", signature(object="kRp.TTR"), function(object){

  # function to add stuff to the matrix,
  # "adds" must me a named list
  add.to.sumtab <- function(table, adds){
    if("index" %in% names(adds) & !is.na(adds[["index"]])){
      add.index <- adds[["index"]]
    } else {
      add.index <- ""
    }
    if("value" %in% names(adds) & !is.na(adds[["value"]])){
      add.value <- round(adds[["value"]], digits=2)
    } else {
      add.value <- ""
    }
#     if("interp" %in% names(adds)){
#       add.interp <- adds[["interp"]]
#     } else {
#       add.interp <- ""
#     }
    res.table <- rbind(table, list(add.index, add.value))
    return(res.table)
  }

# "TTR.char"
# "C.char"
# "R.char"
# "CTTR.char"
# "U.char"
# "S.char"
# "K.char"
# "Maas.char"
# "lgV0.char"
# "HDD.char"
# "MTLD.char"
# "MTLDMA.char"

  summary.table <- data.frame(index="", value="", stringsAsFactors=FALSE)

  if(isTRUE(!is.na(object@TTR))){
    summary.table <- add.to.sumtab(summary.table, adds=list(
        index="TTR",
        value=object@TTR
      ))
  } else {}

  if(isTRUE(!is.na(object@MSTTR$MSTTR))){
    summary.table <- add.to.sumtab(summary.table, adds=list(
        index="MSTTR",
        value=object@MSTTR$MSTTR
      ))
  } else {}

  if(isTRUE(!is.na(object@MATTR$MATTR))){
    summary.table <- add.to.sumtab(summary.table, adds=list(
        index="MATTR",
        value=object@MATTR$MATTR
      ))
  } else {}

  if(isTRUE(!is.na(object@C.ld))){
    summary.table <- add.to.sumtab(summary.table, adds=list(
        index="Herdan's C",
        value=object@C.ld
      ))
  } else {}

  if(isTRUE(!is.na(object@R.ld))){
    summary.table <- add.to.sumtab(summary.table, adds=list(
        index="Root TTR",
        value=object@R.ld
      ))
  } else {}

  if(isTRUE(!is.na(object@CTTR))){
    summary.table <- add.to.sumtab(summary.table, adds=list(
        index="CTTR",
        value=object@CTTR
      ))
  } else {}

  if(isTRUE(!is.na(object@U.ld))){
    summary.table <- add.to.sumtab(summary.table, adds=list(
        index="Uber index",
        value=object@U.ld
      ))
  } else {}

  if(isTRUE(!is.na(object@S.ld))){
    summary.table <- add.to.sumtab(summary.table, adds=list(
        index="Summer",
        value=object@S.ld
      ))
  } else {}

  if(isTRUE(!is.na(object@K.ld))){
    summary.table <- add.to.sumtab(summary.table, adds=list(
        index="Yule's K",
        value=object@K.ld
      ))
  } else {}

  if(isTRUE(!is.na(object@Maas))){
    summary.table <- add.to.sumtab(summary.table, adds=list(
        index="Maas a",
        value=object@Maas
      ))
  } else {}

  if(isTRUE(!is.na(object@lgV0))){
    summary.table <- add.to.sumtab(summary.table, adds=list(
        index="Maas lgV0",
        value=object@lgV0
      ))
  } else {}

  if(isTRUE(!is.na(object@HDD$HDD))){
    summary.table <- add.to.sumtab(summary.table, adds=list(
        index="HD-D (vocd-D)",
        value=object@HDD$HDD
      ))
  } else {}

  if(isTRUE(!is.na(object@MTLD$MTLD))){
    summary.table <- add.to.sumtab(summary.table, adds=list(
        index="MTLD",
        value=object@MTLD$MTLD
      ))
  } else {}

  if(isTRUE(!is.na(object@MTLDMA$MTLDMA))){
    summary.table <- add.to.sumtab(summary.table, adds=list(
        index="MTLD-MA",
        value=object@MTLDMA$MTLDMA
      ))
  } else {}

  if(nrow(summary.table) > 0){
    # remove empty first row
    summary.table <- summary.table[-1,]
    dimnames(summary.table)[[1]] <- c(1:dim(summary.table)[[1]])
  } else {
    return(invisible(NULL))
  }

  return(summary.table)
})
