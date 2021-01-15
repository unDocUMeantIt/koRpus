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

# internal function to add stuff to the matrix,
# "adds" must me a named list
add.to.sumtab <- function(table, adds, flat=FALSE){
  if(all("index" %in% names(adds), !is.na(adds[["index"]]))){
    add.indexFlat <- add.index <- adds[["index"]]
  } else {
    add.indexFlat <- add.index <- ""
  }
  if("indexFlat" %in% names(adds)){
    add.indexFlat <- adds[["indexFlat"]]
  } else {}
  if(all("value" %in% names(adds), !is.na(adds[["value"]]))){
    add.value <- round(adds[["value"]], digits=2)
  } else {
    add.value <- ""
  }
  #     if("interp" %in% names(adds)){
  #       add.interp <- adds[["interp"]]
  #     } else {
  #       add.interp <- ""
  #     }
  
  if(isTRUE(flat)){
    table[add.indexFlat] <- add.value
    return(table)
  } else {
    res.table <- rbind(table, list(add.index, add.value))
    return(res.table)
  }
}

#' @include 01_class_02_kRp.TTR.R
#' @include 02_method_summary.kRp.lang.R
#' @export
#' @docType methods
#' @rdname summary-methods
#' @aliases summary,kRp.TTR-method
#' @example inst/examples/if_lang_en_clause_start.R
#' @example inst/examples/define_sample_file.R
#' @examples
#'   tokenized.obj <- tokenize(
#'     txt=sample_file,
#'     lang="en"
#'   )
#'   ld.results <- lex.div(tokenized.obj, char=c())
#'   summary(ld.results)
#'   summary(ld.results, flat=TRUE)
#' @example inst/examples/if_lang_en_clause_end.R
setMethod("summary", signature(object="kRp.TTR"), function(object, flat=FALSE){

  if(isTRUE(flat)){
    summary.table <- c()
  } else {
    summary.table <- data.frame(index="", value="", stringsAsFactors=FALSE)
  }

  if(isTRUE(!is.na(slot(object, "TTR")))){
    summary.table <- add.to.sumtab(summary.table, adds=list(
        index="TTR",
        value=slot(object, "TTR")
      ),
      flat=flat
    )
  } else {}

  if(isTRUE(!is.na(slot(object, "MSTTR")[["MSTTR"]]))){
    summary.table <- add.to.sumtab(summary.table, adds=list(
        index="MSTTR",
        value=slot(object, "MSTTR")[["MSTTR"]]
      ),
      flat=flat
    )
  } else {}

  if(isTRUE(!is.na(slot(object, "MATTR")[["MATTR"]]))){
    summary.table <- add.to.sumtab(summary.table, adds=list(
        index="MATTR",
        value=slot(object, "MATTR")[["MATTR"]]
      ),
      flat=flat
    )
  } else {}

  if(isTRUE(!is.na(slot(object, "C.ld")))){
    summary.table <- add.to.sumtab(summary.table, adds=list(
        index="Herdan's C",
        indexFlat="C",
        value=slot(object, "C.ld")
      ),
      flat=flat
    )
  } else {}

  if(isTRUE(!is.na(slot(object, "R.ld")))){
    summary.table <- add.to.sumtab(summary.table, adds=list(
        index="Root TTR",
        indexFlat="R",
        value=slot(object, "R.ld")
      ),
      flat=flat
    )
  } else {}

  if(isTRUE(!is.na(slot(object, "CTTR")))){
    summary.table <- add.to.sumtab(summary.table, adds=list(
        index="CTTR",
        value=slot(object, "CTTR")
      ),
      flat=flat
    )
  } else {}

  if(isTRUE(!is.na(slot(object, "U.ld")))){
    summary.table <- add.to.sumtab(summary.table, adds=list(
        index="Uber index",
        indexFlat="U",
        value=slot(object, "U.ld")
      ),
      flat=flat
    )
  } else {}

  if(isTRUE(!is.na(slot(object, "S.ld")))){
    summary.table <- add.to.sumtab(summary.table, adds=list(
        index="Summer",
        indexFlat="S",
        value=slot(object, "S.ld")
      ),
      flat=flat
    )
  } else {}

  if(isTRUE(!is.na(slot(object, "K.ld")))){
    summary.table <- add.to.sumtab(summary.table, adds=list(
        index="Yule's K",
        indexFlat="K",
        value=slot(object, "K.ld")
      ),
      flat=flat
    )
  } else {}

  if(isTRUE(!is.na(slot(object, "Maas")))){
    summary.table <- add.to.sumtab(summary.table, adds=list(
        index="Maas a",
        indexFlat="a",
        value=slot(object, "Maas")
      ),
      flat=flat
    )
  } else {}

  if(isTRUE(!is.na(slot(object, "lgV0")))){
    summary.table <- add.to.sumtab(summary.table, adds=list(
        index="Maas lgV0",
        indexFlat="lgV0",
        value=slot(object, "lgV0")
      ),
      flat=flat
    )
  } else {}

  if(isTRUE(!is.na(slot(object, "HDD")[["HDD"]]))){
    summary.table <- add.to.sumtab(summary.table, adds=list(
        index="HD-D (vocd-D)",
        indexFlat="HDD",
        value=slot(object, "HDD")[["HDD"]]
      ),
      flat=flat
    )
  } else {}

  if(isTRUE(!is.na(slot(object, "MTLD")[["MTLD"]]))){
    summary.table <- add.to.sumtab(summary.table, adds=list(
        index="MTLD",
        value=slot(object, "MTLD")[["MTLD"]]
      ),
      flat=flat
    )
  } else {}

  if(isTRUE(!is.na(slot(object, "MTLDMA")[["MTLDMA"]]))){
    summary.table <- add.to.sumtab(summary.table, adds=list(
        index="MTLD-MA",
        indexFlat="MTLDMA",
        value=slot(object, "MTLDMA")[["MTLDMA"]]
      ),
      flat=flat
    )
  } else {}

  if(!isTRUE(flat)){
    if(nrow(summary.table) > 0){
        # remove empty first row
        summary.table <- summary.table[-1,]
        dimnames(summary.table)[[1]] <- c(1:dim(summary.table)[[1]])
    } else {
      return(invisible(NULL))
    }
  } else if(!length(summary.table) > 0){
    return(invisible(NULL))
  }

  return(summary.table)
})
