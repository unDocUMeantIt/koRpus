# Copyright 2010-2015 Meik Michalke <meik.michalke@hhu.de>
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

# this internal function does the real frequency analysis,
# so it's mostly called by freq.analysis()

###############################################################
## if this signature changes, check freq.analysis() as well! ##
###############################################################

#' @include 00_class_03_kRp.txt.freq.R
kRp.freq.analysis.calc <- function(txt.file, corp.freq=NULL, desc.stat=TRUE, force.lang=NULL,
                       tagger="kRp.env", corp.rm.class="nonpunct",
                       corp.rm.tag=c(), tfidf=TRUE, ...){

  if("lang" %in% names(list(...))){
    # since 'lang' is a valid argument for treetag(), it might have been set
    stop(simpleError("You defined 'lang' in the '...' argument. This is confusing me! Use 'force.lang' instead."))
  } else {}
  # for backward compatibility
  if("treetagger" %in% names(list(...))){
    stop(simpleError("The option 'treetagger' is deprecated and was removed. Use 'tagger' instead."))
  } else {}

  # the internal function tag.kRp.txt() will return the object unchanged if it
  # is already tagged, so it's safe to call it with the lang set here
  tagged.text <- tag.kRp.txt(txt.file, tagger=tagger, lang=force.lang, objects.only=FALSE, ...)
  # set the language definition
  lang <- language.setting(tagged.text, force.lang)
  commented <- slot(tagged.text, "TT.res")

  if(identical(corp.rm.class, "nonpunct")){
    corp.rm.class <- kRp.POS.tags(lang, tags=c("punct","sentc"), list.classes=TRUE)
  } else {}

  if(!is.null(corp.freq)){
    # before we even start, check if we're alright:
    stopifnot(inherits(corp.freq, "kRp.corp.freq"))
    frequency.pre <- text.freq.analysis(
      txt.commented=commented,
      corp.freq=corp.freq,
      corp.rm.class=corp.rm.class,
      corp.rm.tag=corp.rm.tag,
      lang=lang,
      tfidf=tfidf)
    # commented will be overwritten with a new version containing percentages for each word
    commented <- frequency.pre[["commented"]]
    frequency.res <- frequency.pre[["freq.analysis"]]
  } else {
    frequency.res <- list(NA)
  }

  if(isTRUE(desc.stat)){
    desc.stat.res <- text.analysis(commented, lang=lang, corp.rm.class=corp.rm.class, corp.rm.tag=corp.rm.tag, desc=slot(tagged.text, "desc"))
  } else {
    desc.stat.res <- slot(tagged.text, "desc")
  }

  results <- new("kRp.txt.freq", lang=lang, TT.res=commented, desc=desc.stat.res, freq.analysis=frequency.res)
  return(results)
}
