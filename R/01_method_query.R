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


#' A method to get information out of koRpus objects
#'
#' The method \code{query} returns query information from objects of classes \code{\link[koRpus]{kRp.corp.freq-class}} and
#' \code{\link[koRpus]{kRp.tagged-class}}.
#'
#' \emph{kRp.corp.freq:} Depending on the setting of the \code{var} parameter, will return entries with a matching character (\code{var="word"}),
#' or all entries of the desired frequency (see the examples). A special case is the need for a range of frequencies,
#' which can be achieved by providing a nomerical vector of two values as the \code{query} value, for start and end of
#' the range, respectively. In these cases, if \code{rel} is set to \code{"gt"} or \code{"lt"},
#' the given range borders are excluded, otherwise they will be included as true matches.
#'
#' \emph{kRp.tagged:} \code{var} can be any of the variables in slot \code{TT.res}. For \code{rel} currently only
#' "eq" and "num" are implemented. The latter isn't a relation, but will return a vector with the row numbers in which
#' the query was found.
#'
#' @section Query lists: You can combine an arbitrary number of queries in a simple way by providing a list of named lists to the
#' \code{query} parameter, where each list contains one query request. In each list, the first element name represents the
#' \code{var} value of the request, and its value is taken as the \code{query} argument. You can also assign \code{rel}, 
#' \code{ignore.case} and \code{perl} for each request individually, and if you don't, the settings of the main query call are 
#' taken as default (\code{as.df} only applies to the final query). The filters will be applied in the order given, i.e., the
#' second query will be made to the results of the first.
#'
#' This method calls \code{\link[base]{subset}}, which might actually be even more flexible if you need more control.
#'
#' @param obj An object of class \code{\link[koRpus]{kRp.corp.freq-class}}.
#' @param var A character string naming a variable in the object (i.e., colname). If set to
#'    \code{"regexp"}, \code{grepl} is called on the \code{word} column of corpus frequency
#'    objects.
#' @param query A character vector (for words), regular expression, or single number naming values to be matched in the variable.
#'    Can also be a vector of two numbers to query a range of frequency data, or a list of named lists for multiple queries (see
#'    "Query lists" section in details).
#' @param rel A character string defining the relation of the queried value and desired results.
#'    Must either be \code{"eq"} (equal, the default), \code{"gt"} (greater than), \code{"ge"} (greater of equal),
#'    \code{"lt"} (less than) or \code{"le"} (less or equal). If \code{var="word"}, is always interpreted as \code{"eq"}
#' @param as.df Logical, if \code{TRUE}, returns a data.frame, otherwise an object of
#'    the input class.
#' @param ignore.case Logical, passed through to \code{grepl} if \code{var="regexp"}.
#' @param perl Logical, passed through to \code{grepl} if \code{var="regexp"}.
#' @param ... Optional arguments, see above.
#' @return Depending on the arguments, might include whole objects, lists, single values etc.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords methods
#' @seealso \code{\link[koRpus]{kRp.corp.freq-class}}, \code{\link[base]{subset}}
#' @examples
#' \dontrun{
#' # look up frequencies for the word "aber"
#' query(LCC.data, var="word", query="aber")
#'
#' # show all entries with a frequency of exactly 3000 in the corpus
#' query(LCC.data, "freq", 3000)
#'
#' # now, which words appear more than 40000 times in a million?
#' query(LCC.data, "pmio", 40000, "gt")
#'
#' # example for a range request: words with a log10 between 2 and 2.1
#' # (including these two values)
#' query(LCC.data, "log10", c(2, 2.1))
#' # (and without them)
#' query(LCC.data, "log10", c(2, 2.1), "gt")
#'
#' # example for a list of queries: get words with a frequency between
#' # 700 and 750 per million and at least five letters
#' query(LCC.data, query=list(
#'   list(pmio=c(700,750)),
#'   list(lttr=5, rel="ge"))
#' )
#'
#' # get all "he" lemmata in a previously tagged text object
#' query(tagged.txt, "lemma", "he")
#' }
#' @export
#' @docType methods
#' @rdname query-methods
setGeneric("query", function(obj, ...) standardGeneric("query"))

#' @export
#' @docType methods
#' @rdname query-methods
#' @aliases query,kRp.corp.freq-method
#' @include 00_class_06_kRp.corp.freq.R
setMethod("query",
    signature(obj="kRp.corp.freq"),
    function (obj, var=NULL, query, rel="eq", as.df=TRUE, ignore.case=TRUE, perl=FALSE){
      # if query is a list, invoke queryList()
      if(is.list(query)){
        obj <- queryList(obj=obj, var=var, query=query, rel=rel, as.df=as.df, ignore.case=ignore.case, perl=perl)
        return(obj)
      } else {}

      ## only run if query is not a list:
      stopifnot(length(var) == 1)
      results <- ""
      in.obj <- slot(obj, "words")
      # basically, we have to see if we're looking for a word or for frequencies
      # we'll estimate that by looking at the data class of the column queried
      if(isTRUE(is.character(in.obj[[var]]))){
        # see if we have fixed character
        if(identical(var, "regexp")){
          # get all entries where word matches regexp
          results <- in.obj[grepl(query, in.obj[["word"]], ignore.case=ignore.case, perl=perl),]
        } else {
          results <- subset(in.obj, eval(parse(text=paste(var, "%in% query"))))
        }
      } else{}
      # in case we're looking for anything frequency related
      if(isTRUE(is.numeric(in.obj[[var]]))){
        if(length(query) == 1){
          if(identical(rel, "eq")){
            results <- subset(in.obj, eval(parse(text=paste(var, "== query"))))
          } else{}
          if(identical(rel, "gt")){
            results <- subset(in.obj, eval(parse(text=paste(var, "> query"))))
          } else{}
          if(identical(rel, "ge")){
            results <- subset(in.obj, eval(parse(text=paste(var, ">= query"))))
          } else{}
          if(identical(rel, "lt")){
            results <- subset(in.obj, eval(parse(text=paste(var, "< query"))))
          } else{}
          if(identical(rel, "le")){
            results <- subset(in.obj, eval(parse(text=paste(var, "<= query"))))
          } else{}
        } else if(length(query) == 2){
          if(rel %in% c("gt", "lt")){
            results <- subset(in.obj, eval(parse(text=paste("(", var, "> query[1] ) & (", var, "< query[2] )"))))
          } else{
            results <- subset(in.obj, eval(parse(text=paste("(", var, ">= query[1] ) & (", var, "<= query[2] )"))))
          }
        } else{}
      } else{}

    if(identical(results, "")){
      stop(simpleError("Unable to comply."))
    } else {
      if(isTRUE(as.df)){
        return(results)
      } else {
        # write results back to the originating object
        slot(obj, "words") <- results
      }
    }
    return(obj)
    }
)

#' @export
#' @docType methods
#' @rdname query-methods
#' @aliases query,kRp.tagged-method
#' @include 00_class_01_kRp.tagged.R
setMethod("query",
    signature(obj="kRp.tagged"),
    function (obj, var, query, rel="eq", as.df=TRUE, ignore.case=TRUE, perl=FALSE){
      if(!rel %in% c("eq", "num")){
        stop(simpleError(paste("Invalid rel for class kRp.tagged:", rel)))
      } else {}
      # if query is a list, invoke queryList()
      if(is.list(query)){
        obj <- queryList(obj=obj, var=var, query=query, rel=rel, as.df=as.df, ignore.case=ignore.case, perl=perl)
        return(obj)
      } else {}

      ## only run if query is not a list:
      stopifnot(length(var) == 1)
      in.obj <- slot(obj, "TT.res")
      if(var %in% names(in.obj)){
        num.findings <- which(in.obj[[var]] %in% query)
        if(identical(rel, "num")){
          results <- num.findings
        } else {
          results <- in.obj[num.findings, ]
        }
      } else if(identical(var, "regexp")){
        # see if we have fixed character
        # get all entries where word matches regexp
        results <- in.obj[grepl(query, in.obj[["token"]], ignore.case=ignore.case, perl=perl),]
      } else{}
{
        stop(simpleError(paste("Invalid var for class kRp.tagged:", var)))
      }
    if(identical(results, "")){
      stop(simpleError("Unable to comply."))
    } else{
      if(isTRUE(as.df)){
        return(results)
      } else {
        # write results back to the originating object
        slot(obj, "TT.res") <- results
      }
    }
    return(results)
    }
)
