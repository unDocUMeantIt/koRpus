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


#' Work in (early) progress. Probably don't even look at it. Consider it pure magic that is not to be tempered with.
#'
#' In some future release, this might evolve into a function to help comparing several texts by features like average
#' sentece length, word length, lexical diversity, and so forth. The idea behind it is to conduct a cluster analysis,
#' to discover which texts out of several are similar to (or very different from) each other. This can be useful, e.g., if
#' you need texts for an experiment which are different in content, but similar regarding syntactic features, like
#' listed above.
#'
#' It is included in this package not really to be used, but to maybe inspire you, to toy around with the code and help me to
#' come up with something useful in the end...
#' @param txts A character vector with paths to texts to analyze.
#' @param lang A character string with a valid Language identifier.
#' @param TT.path A character string, path to TreeTagger installation.
#' @param TT.preset A character string naming the TreeTagger preset to use.
#' @export
kRp.cluster <- function(txts, lang, TT.path, TT.preset){
  ## TODO: sanity checks for about everything!

  # how many texts will we have to analyze?
  num.texts <- length(txts)
  analysis.res <- sapply(1:num.texts, function(txt.idx){
    txt <- txts[txt.idx]
    message(paste0("Analyzing text: ", txt, " [", txt.idx, "/", num.texts, "]...\n"))
    POS.analysis.res <- treetag(txt, treetagger="manual", lang=lang, TT.options=list(path=TT.path, preset=TT.preset))
    frq.analysis.res <- freq.analysis(POS.analysis.res)
    # get relative token class distribution
    frq.vector <- c(frq.analysis.res@desc$freq.token) / sum(frq.analysis.res@desc$freq.token)
    # trigger hyphenation
    hyph.analysis.res <- hyphen(POS.analysis.res)
    rdb.vector <- c(
      POS.analysis.res@desc$avg.sentc.length,
      POS.analysis.res@desc$avg.word.length,
      hyph.analysis.res@desc$avg.syll.word
#      hyph.analysis.res@desc$syll.distrib["num",]/POS.analysis.res@desc$words, # relative distribution of syllables
      )
    lxd.analysis.res <- lex.div(frq.analysis.res, measure=c("MATTR", "HD-D", "MTLD"), char=c())
#    all.res <- list(frq.analysis.res, summary(rdb.analysis.res, flat=TRUE), summary(lxd.analysis.res))
#    all.res <- c(as.numeric(summary(rdb.analysis.res, flat=TRUE)), as.numeric(summary(lxd.analysis.res)[,2]))
    res.vector.names <- c(names(frq.vector), "avg.sntc.len", "avg.word.len", "avg.syll.word", "MATTR", "HDD", "MTLD")
    all.res <- c(
      as.numeric(frq.vector),
      as.numeric(rdb.vector),
      lxd.analysis.res@MATTR[["MATTR"]],
      lxd.analysis.res@HDD[["HDD"]],
      lxd.analysis.res@MTLD[["MTLD"]])
    names(all.res) <- res.vector.names
    return(all.res)
    })
  # next steps to cluster analysis:
  # hclust(dist(t(analysis.res)))
  return(t(analysis.res))
}
