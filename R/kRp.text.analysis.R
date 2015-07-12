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


#' Analyze texts using TreeTagger and word frequencies
#'
#' The function \code{kRp.text.analysis} analyzes texts in various ways.
#'
#' The function is basically a wrapper for \code{treetag()},\code{freq.analysis()} and \code{lex.div()}.
#'
#' By default, if the text has to be tagged yet, the language definition is queried by calling \code{get.kRp.env(lang=TRUE)} internally.
#' Or, if \code{txt.file} has already been tagged, by default the language definition of that tagged object is read
#' and used. Set \code{force.lang=get.kRp.env(lang=TRUE)} or to any other valid value, if you want to forcibly overwrite this
#' default behaviour, and only then. See \code{\link[koRpus:kRp.POS.tags]{kRp.POS.tags}} for all supported languages.
#'
#' @param txt.file Either an object of class \code{\link[koRpus]{kRp.tagged-class}}, \code{\link[koRpus]{kRp.txt.freq-class}},
#'    \code{\link[koRpus]{kRp.analysis-class}} or \code{\link[koRpus]{kRp.txt.trans-class}}, or
#'    a character vector which must be be a valid path to a file containing the text to be analyzed.
#' @param tagger A character string defining the tokenizer/tagger command you want to use for basic text analysis. Can be omitted if
#'    \code{txt.file} is already of class \code{kRp.tagged-class}. Defaults to \code{"kRp.env"} to get the settings by
#'    \code{\link[koRpus:get.kRp.env]{get.kRp.env}}. Set to \code{"tokenize"} to use \code{\link[koRpus:tokenize]{tokenize}}.
#' @param force.lang A character string defining the language to be assumed for the text, by force.
#' @param desc.stat Logical, whether a descriptive statistical analysis should be performed.
#' @param lex.div Logical, whether some lexical diversity analysis should be performed, using \code{\link[koRpus:lex.div]{lex.div}}.
#' @param corp.freq An object of class \code{\link[koRpus]{kRp.corp.freq-class}}. If present, a frequency index for the analyzed text is computed (see details).
#' @param corp.rm.class A character vector with word classes which should be ignored for frequency analysis. The default value
#'    \code{"nonpunct"} has special meaning and will cause the result of
#'    \code{kRp.POS.tags(lang, c("punct","sentc"), list.classes=TRUE)} to be used.
#' @param corp.rm.tag A character vector with POS tags which should be ignored for frequency analysis.
#' @param ... Additional options to be passed through to the function defined with \code{tagger}.
#' @return An object of class \code{\link[koRpus]{kRp.analysis-class}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords misc
#' @seealso \code{\link[koRpus:set.kRp.env]{set.kRp.env}}, \code{\link[koRpus:get.kRp.env]{get.kRp.env}},
#'    \code{\link[koRpus:kRp.POS.tags]{kRp.POS.tags}}, \code{\link[koRpus:lex.div]{lex.div}}
#' @references [1] \url{http://www.ims.uni-stuttgart.de/projekte/corplex/TreeTagger/DecisionTreeTagger.html}
#' @export
#' @examples
#' \dontrun{
#' kRp.text.analysis("/some/text.txt")
#' }

kRp.text.analysis <- function(txt.file, tagger="kRp.env", force.lang=NULL,
                       desc.stat=TRUE, lex.div=TRUE, corp.freq=NULL, corp.rm.class="nonpunct",
                       corp.rm.tag=c(), ...){

  if("lang" %in% names(list(...))){
    # since 'lang' is a valid argument for treetag(), it might have been set
    stop(simpleError("You defined 'lang' in the '...' argument. This is confusing me! Use 'force.lang' instead."))
  } else {}

  # the internal function tag.kRp.txt() will return the object unchanged if it
  # is already tagged, so it's safe to call it with the lang set here
  tagged.text <- tag.kRp.txt(txt.file, tagger=tagger, lang=force.lang, objects.only=FALSE, ...)
  # set the language definition
  lang <- language.setting(tagged.text, force.lang)

  if(identical(corp.rm.class, "nonpunct")){
    corp.rm.class <- kRp.POS.tags(lang, tags=c("punct","sentc"), list.classes=TRUE)
  } else {}

  if(isTRUE(desc.stat) | !is.null(corp.freq)){
    frequency.pre <- freq.analysis(txt.file=tagged.text, corp.freq=corp.freq, desc.stat=desc.stat, force.lang=lang,
                       tagger=tagger, corp.rm.class=corp.rm.class, corp.rm.tag=corp.rm.tag)
    # commented will be overwritten with a new version containing percentages for each word
    tagged.text@TT.res <- frequency.pre@TT.res
    frequency.res <- frequency.pre@freq.analysis
    desc.stat.res <- frequency.pre@desc
  } else {
    frequency.res <- list(NA)
    desc.stat.res <- list(NA)
  }

  if(isTRUE(lex.div)){
    lex.div.res <- lex.div(tagged.text, char=c(), force.lang=lang)
  } else {
    lex.div.res <- list(NA)
  }
  

  results <- new("kRp.analysis", lang=lang, TT.res=tagged.text@TT.res, desc=desc.stat.res, lex.div=lex.div.res, freq.analysis=frequency.res)
  return(results)
}
