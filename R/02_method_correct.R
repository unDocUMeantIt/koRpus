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


#' Methods to correct koRpus objects
#' 
#' The method \code{correct.tag} can be used to alter objects of class \code{\link[koRpus:kRp.text-class]{kRp.text}}.
#'
#' Although automatic POS tagging and lemmatization are remarkably accurate, the algorithms do ususally produce
#' some errors. If you want to correct for these flaws, this method can be of help, because it might prevent you from
#' introducing new errors. That is, it will do some sanitiy checks before the object is actually manipulated and returned.
#'
#' \code{correct.tag} will read the \code{lang} slot from the given object and check whether the \code{tag}
#' provided is actually valid. If so, it will not only change the \code{tag} field in the object, but also update
#' \code{wclass} and \code{desc} accordingly.
#'
#' If \code{check.token} is set it must also match \code{token} in the given row(s). Note that no check is done on the lemmata.
#'
#' @param obj An object of class \code{\link[koRpus:kRp.text-class]{kRp.text}}.
#' @param row Integer, the row number of the entry to be changed. Can be an integer vector
#'    to change several rows in one go.
#' @param tag A character string with a valid POS tag to replace the current tag entry.
#'    If \code{NULL} (the default) the entry remains unchanged.
#' @param lemma A character string naming the lemma to to replace the current lemma entry.
#'    If \code{NULL} (the default) the entry remains unchanged.
#' @param check.token A character string naming the token you expect to be in this row.
#'    If not \code{NULL}, \code{correct} will stop with an error if this values don't match.
#' @return An object of the same class as \code{obj}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords methods
#' @seealso \code{\link[koRpus:kRp.text-class]{kRp.text}}, \code{\link[koRpus:treetag]{treetag}},
#'    \code{\link[koRpus:kRp.POS.tags]{kRp.POS.tags}}.
#' @examples
#' \dontrun{
#' tagged.txt <- correct.tag(tagged.txt, row=21, tag="NN")
#' }
#' @export
#' @docType methods
#' @rdname correct-methods
setGeneric("correct.tag", function(obj, row, tag=NULL, lemma=NULL, check.token=NULL, quiet=TRUE){standardGeneric("correct.tag")})

#' @export
#' @docType methods
#' @rdname correct-methods
#' @aliases correct.tag correct.tag,kRp.text-method
#' @include 01_class_01_kRp.text.R
#' @include koRpus-internal.R
setMethod("correct.tag",
    signature(obj="kRp.text"),
    function(
      obj,
      row,
      tag=NULL,
      lemma=NULL,
      check.token=NULL,
      quiet=TRUE
    ){

      if(!is.numeric(row)){
        stop(simpleError("Not a valid row number!"))
      } else {}

      local.obj.copy <- obj
      lang <- language(obj)

      if(!is.null(tag)){
        # before we attempt anything, let's check if this is a valid tag
        valid.POS.tags <- kRp.POS.tags(lang, list.tags=TRUE)
        if(is.na(match(tag, valid.POS.tags))){
          stop(simpleError(paste0("Not a valid POS tag for language \"", lang, "\": ", tag)))
        } else {}
        all.POS.tags <- kRp.POS.tags(lang)
        if(all(is.na(local.obj.copy[["desc"]]))){
          # drop all tag descriptions
          all.POS.tags[,"desc"] <- NA
        } else {}
        # this object will hold the columns "tag", "wclass" and "desc" for our tag
        new.tag <- all.POS.tags[all.POS.tags[,"tag"] == tag, ]
        for (cur.row in row){
          if(!is.null(check.token) & !identical(local.obj.copy[cur.row, "token"], check.token)){
            stop(simpleError(paste0("In row ", cur.row,", expected \"", check.token,"\" but got \"", local.obj.copy[cur.row, "token"],"\"!")))
          } else {}
          local.obj.copy[cur.row, c("tag","wclass","desc")] <- new.tag[c("tag","wclass","desc")]
        }
      } else {}
      if(!is.null(lemma)){
        for (cur.row in row){
          if(!is.null(check.token) & !identical(local.obj.copy[cur.row, "token"], check.token)){
            stop(simpleError(paste0("In row ", cur.row,", expected \"", check.token,"\" but got \"", local.obj.copy[cur.row, "token"],"\"!")))
          } else {}
          local.obj.copy[cur.row, "lemma"] <- lemma
        }
      } else {}

      # update descriptive statistics
      describe(local.obj.copy) <- lapply(split_by_doc_id(local.obj.copy),
        function(this_obj){
          return(basic.tagged.descriptives(
            this_obj,
            lang=lang,
            desc=describe(this_obj),
            update.desc=TRUE,
            doc_id=doc_id(this_obj)
          ))
        }
      )

      if(!isTRUE(quiet)){
        cat("Changed\n\n")
        print(obj[row, ])
        cat("\n  into\n\n")
        print(local.obj.copy[row, ])
      } else {}

      return(local.obj.copy)
    }
)
