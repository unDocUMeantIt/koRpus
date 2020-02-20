# Copyright 2019-2020 Meik Michalke <meik.michalke@hhu.de>
#
# This file is part of the R package tm.plugin.koRpus.
#
# tm.plugin.koRpus is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# tm.plugin.koRpus is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with tm.plugin.koRpus.  If not, see <http://www.gnu.org/licenses/>.


#' Turn a multi-document kRp.text object into a list of kRp.text objects
#' 
#' For some analysis steps it might be important to have individual tagged texts
#' instead of one large corpus object. This method produces just that.
#'
#' @param obj An object of class \code{\link[koRpus:kRp.text-class]{kRp.text}}.
#' @param keepFeatures Either logical, whether to keep all features or drop them, or a character vector
#'    of names of features to keep if present.
#' @return A named list of objects of class \code{\link[koRpus:kRp.text-class]{kRp.text}}.
#'    Elements are named by their \code{doc_id}.
#' @export
#' @docType methods
#' @rdname split_by_doc_id
#' @examples
#' \dontrun{
#' myCorpusList <- split_by_doc_id(myCorpus)
#' }
setGeneric("split_by_doc_id", function(obj, keepFeatures=TRUE) standardGeneric("split_by_doc_id"))

#' @rdname split_by_doc_id
#' @docType methods
#' @export
#' @aliases
#'    split_by_doc_id,-methods
#'    split_by_doc_id,kRp.corpus-method
#' @include 01_class_01_kRp.text.R
setMethod("split_by_doc_id",
  signature=signature(obj="kRp.text"),
  function(obj, keepFeatures=TRUE){
    all_features <- hasFeature(obj)
    if(is.character(keepFeatures)){
      drop_features <- all_features[!all_features %in% keepFeatures]
      if(length(drop_features) > 0){
        for (this_feature in drop_features){
          feature(obj, this_feature) <- NULL
        }
      } else {}
      all_features <- hasFeature(obj)
      keepFeatures <- TRUE
    } else {}
    if(length(doc_id(obj)) > 1){
      tt_desc <- describe(obj)
      tt_lang <- language(obj)
      tt_tagged <- taggedText(obj)
      tt_list <- split(tt_tagged, tt_tagged[["doc_id"]])
      if(isTRUE(keepFeatures)){
        feature_list <- sapply(
          all_features,
          function(this_feature){
            return(
              identical(
                sort(names(feature(obj, this_feature))),
                sort(names(tt_list))
              )
            )
          },
          USE.NAMES=FALSE
        )
        feature_to_split <- all_features[feature_list]
        feature_as_is <- all_features[!feature_list]
      } else {}
      result <- lapply(
        names(tt_list),
        function(thisText){
          result <- kRp_text(
            lang=tt_lang,
            tokens=tt_list[[thisText]]
          )
          describe(result, doc_id=thisText) <- tt_desc[[thisText]]
          if(isTRUE(keepFeatures)){
            for(this_feature in feature_as_is){
              feature(result, this_feature) <- feature(obj, this_feature)
            }
            for(this_feature in feature_to_split){
              feature(result, this_feature) <- feature(obj, this_feature)[[thisText]]
            }
          } else {}
          return(result)
        }
      )
      names(result) <- names(tt_list)
    } else {
      result <- list(obj)
      names(result) <- doc_id(obj)
    }
    return(result)
  }
) ## end method split_by_doc_id()
