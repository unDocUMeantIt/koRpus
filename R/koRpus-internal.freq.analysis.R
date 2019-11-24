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

# this internal function does the real frequency analysis,
# so it's mostly called by freq.analysis()

###############################################################
## if this signature changes, check freq.analysis() as well! ##
###############################################################

kRp.freq.analysis.calc <- function(
  txt.file,
  corp.freq=NULL,
  desc.stat=TRUE,
  corp.rm.class="nonpunct",
  corp.rm.tag=c(),
  tfidf=TRUE
){
  lang <- language(txt.file)

  if(identical(corp.rm.class, "nonpunct")){
    corp.rm.class <- kRp.POS.tags(lang, tags=c("punct","sentc"), list.classes=TRUE)
  } else {}

  if(all(hasFeature(txt.file, "corp_freq"), is.null(corp.freq))){
    corp.freq <- corpusCorpFreq(txt.file)
  } else {}

  if(!is.null(corp.freq)){
    # before we even start, check if we're alright:
    stopifnot(inherits(corp.freq, "kRp.corp.freq"))
    frequency.pre <- text.freq.analysis(
      txt.commented=taggedText(txt.file),
      corp.freq=corp.freq,
      corp.rm.class=corp.rm.class,
      corp.rm.tag=corp.rm.tag,
      lang=lang,
      tfidf=tfidf)
    # commented will be overwritten with a new version containing percentages for each word
    taggedText(txt.file) <- frequency.pre[["commented"]]
    corpusFreq(txt.file) <- frequency.pre[["freq.analysis"]]
  } else {
    corpusFreq(txt.file) <- list(NA)
  }

  if(isTRUE(desc.stat)){
    describe(txt.file) <- text.analysis(frequency.pre[["commented"]], lang=lang, corp.rm.class=corp.rm.class, corp.rm.tag=corp.rm.tag, desc=describe(txt.file))
  } else {}

  return(txt.file)
}
