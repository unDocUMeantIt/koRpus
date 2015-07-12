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


#' Analyze lexical diversity
#' 
#' These methods analyze the lexical diversity/complexity of a text corpus.
#'
#' \code{lex.div} calculates a variety of proposed indices for lexical diversity. In the following formulae, \eqn{N} refers to
#' the total number of tokens, and \eqn{V} to the number of types:
#' \describe{
#'  \item{\code{"TTR"}:}{The ordinary \emph{Type-Token Ratio}: \deqn{TTR = \frac{V}{N}}{TTR =  V / N}
#'    Wrapper function: \code{\link[koRpus:TTR]{TTR}}}
#'  \item{\code{"MSTTR"}:}{For the \emph{Mean Segmental Type-Token Ratio} (sometimes referred to as \emph{Split TTR}) tokens are split up into 
#'    segments of the given size, TTR for each segment is calculated and the mean of these values returned. Tokens at the end which do 
#'    not make a full segment are ignored. The number of dropped tokens is reported.
#'
#'    Wrapper function: \code{\link[koRpus:MSTTR]{MSTTR}}}
#'  \item{\code{"MATTR"}:}{The \emph{Moving-Average Type-Token Ratio} (Covington & McFall, 2010) calculates TTRs for a defined number of tokens
#'    (called the "window"), starting at the beginning of the text and moving this window over the text, until the last token is reached.
#'    The mean of these TTRs is the MATTR.
#'
#'    Wrapper function: \code{\link[koRpus:MATTR]{MATTR}}}
#'  \item{\code{"C"}:}{Herdan's \emph{C} (Herdan, 1960, as cited in Tweedie & Baayen, 1998; sometimes referred to as \emph{LogTTR}): \deqn{C = \frac{\lg{V}}{\lg{N}}}{C = lg(V) / lg(N)}}
#'
#'    Wrapper function: \code{\link[koRpus:C.ld]{C.ld}}
#'  \item{\code{"R"}:}{Guiraud's \emph{Root TTR} (Guiraud, 1954, as cited in Tweedie & Baayen, 1998): \deqn{R = \frac{V}{\sqrt{N}}}{R = V / sqrt(N)}}
#'
#'    Wrapper function: \code{\link[koRpus:R.ld]{R.ld}}
#'  \item{\code{"CTTR"}:}{Carroll's \emph{Corrected TTR}: \deqn{CTTR = \frac{V}{\sqrt{2N}}}{CTTR = V / sqrt(2N)}}
#'
#'    Wrapper function: \code{\link[koRpus:CTTR]{CTTR}}
#'  \item{\code{"U"}:}{Dugast's \emph{Uber Index}  (Dugast, 1978, as cited in Tweedie & Baayen, 1998): \deqn{U = \frac{(\lg{N})^2}{\lg{N} - \lg{V}}}{U = lg(N)^2 / lg(N) - lg(V)}}
#'
#'    Wrapper function: \code{\link[koRpus:U.ld]{U.ld}}
#'  \item{\code{"S"}:}{Summer's index: \deqn{S = \frac{\lg{\lg{V}}}{\lg{\lg{N}}}}{S = lg(lg(V)) / lg(lg(N))}}
#'
#'    Wrapper function: \code{\link[koRpus:S.ld]{S.ld}}
#'  \item{\code{"K"}:}{Yule's \emph{K}  (Yule, 1944, as cited in Tweedie & Baayen, 1998) is calculated by: \deqn{K = 10^4 \times \frac{(\sum_{X=1}^{X}{{f_X}X^2}) - N}{N^2}}{K = 10^4 * (sum(fX*X^2) - N) / N^2}
#'    where \eqn{N} is the number of tokens, \eqn{X} is a vector with the frequencies of each type, and \eqn{f_X}{fX} is
#'    the frequencies for each X.
#'
#'    Wrapper function: \code{\link[koRpus:K.ld]{K.ld}}}
#'  \item{\code{"Maas"}:}{Maas' indices (\eqn{a}, \eqn{\lg{V_0}} & \eqn{\lg{}_{e}{V_0}}): \deqn{a^2 = \frac{\lg{N} - \lg{V}}{\lg{N}^2}}{a^2 = lg(N) - lg(V) / lg(N)^2}
#'  \deqn{\lg{V_0} = \frac{\lg{V}}{\sqrt{1 - \frac{\lg{V}}{\lg{N}}^2}}}{lg(V0) = lg(V) / sqrt(1 - (lg(V) / lg(N)^2))}
#'    Earlier versions (\code{koRpus} < 0.04-12) reported \eqn{a^2}, and not \eqn{a}. The measure was derived from a formula by M\"uller (1969, as cited in Maas, 1972).
#'    \eqn{\lg{}_{e}{V_0}} is equivalent to \eqn{\lg{V_0}}, only with \eqn{e} as the base for the logarithms. Also calculated are \eqn{a}, \eqn{\lg{V_0}} (both not the same
#'    as before) and \eqn{V'} as measures of relative vocabulary growth while the text progresses. To calculate these measures, the first half of the text and the full text
#'    will be examined (see Maas, 1972, p. 67 ff. for details).
#'
#'    Wrapper function: \code{\link[koRpus:maas]{maas}}}
#'  \item{\code{"MTLD"}:}{For the \emph{Measure of Textual Lexical Diversity} (McCarthy & Jarvis, 2010) so called factors are counted. Each factor is a subsequent stream of 
#'    tokens which ends (and is then counted as a full factor) when the TTR value falls below the given factor size. The value of
#'    remaining partial factors is estimated by the ratio of their current TTR to the factor size threshold. The MTLD is the total number 
#'    of tokens divided by the number of factors. The procedure is done twice, both forward and backward for all tokens, and the mean of 
#'    both calculations is the final MTLD result.
#'
#'    Wrapper function: \code{\link[koRpus:MTLD]{MTLD}}}
#'  \item{\code{"MTLD-MA"}:}{The \emph{Moving-Average Measure of Textual Lexical Diversity} (Jarvis, no year) combines factor counting and a moving
#'    window similar to MATTR: After each full factor the the next one is calculated from one token after the last starting point. This is repeated
#'    until the end of text is reached for the first time. The average of all full factor lengths is the final MTLD-MA result. Factors below the
#'    \code{min.tokens} threshold are dropped.
#'
#'    Wrapper function: \code{\link[koRpus:MTLD]{MTLD}}}
#'  \item{\code{"HD-D"}:}{The \emph{HD-D} value can be interpreted as the idealized version of \emph{vocd-D} (see McCarthy & Jarvis, 2007). For each type,
#'    the probability is computed (using the hypergeometric distribution) of drawing it at least one time when drawing randomly a certain
#'    number of tokens from the text -- 42 by default. The sum of these probabilities make up the HD-D value. The sum of probabilities relative to
#'    the drawn sample size (ATTR) is also reported.
#'
#'    Wrapper function: \code{\link[koRpus:HDD]{HDD}}}
#' }
#'
#' By default, if the text has to be tagged yet, the language definition is queried by calling \code{get.kRp.env(lang=TRUE)} 
#' internally.
#' Or, if \code{txt} has already been tagged, by default the language definition of that tagged object is read
#' and used. Set \code{force.lang=get.kRp.env(lang=TRUE)} or to any other valid value, if you want to forcibly overwrite this
#' default behaviour, and only then. See \code{\link[koRpus:kRp.POS.tags]{kRp.POS.tags}} for all supported languages.
#'
#' @param txt An object of either class \code{\link[koRpus]{kRp.tagged-class}}, \code{\link[koRpus]{kRp.txt.freq-class}},
#'    \code{\link[koRpus]{kRp.analysis-class}} or  \code{\link[koRpus]{kRp.txt.trans-class}}, containing the tagged text to be analyzed.
#' @param segment An integer value for MSTTR, defining how many tokens should form one segment.
#' @param factor.size A real number between 0 and 1, defining the MTLD factor size.
#' @param min.tokens An integer value, how many tokens a full factor must at least have to be considered for the MTLD-MA result.
#' @param rand.sample An integer value, how many tokens should be assumed to be drawn for calculating HD-D.
#' @param window An integer value for MATTR, defining how many tokens the moving window should include.
#' @param case.sens Logical, whether types should be counted case sensitive.
#' @param lemmatize Logical, whether analysis should be carried out on the lemmatized tokens rather than all running word forms.
#' @param detailed Logical, whether full details of the analysis should be calculated. This currently affects MTLD and MTLD-MA, defining
#'    if all factors should be kept in the object. This slows down calculations considerably.
#' @param measure A character vector defining the measures which should be calculated. Valid elements are "TTR", "MSTTR", "MATTR", "C", "R", 
#'    "CTTR", "U", "S", "K", "Maas", "HD-D", "MTLD" and "MTLD-MA".
#' @param char A character vector defining whether data for plotting characteristic curves should be calculated. Valid elements are 
#'    "TTR","MATTR", "C", "R", "CTTR", "U", "S", "K", "Maas", "HD-D", "MTLD" and "MTLD-MA".
#' @param char.steps An integer value defining the stepwidth for characteristic curves, in tokens.
#' @param log.base A numeric value defining the base of the logarithm. See \code{\link[base:log]{log}} for details.
#' @param force.lang A character string defining the language to be assumed for the text, by force. See details.
#' @param keep.tokens Logical. If \code{TRUE} all raw tokens and types will be preserved in the resulting object, in a slot called 
#'    \code{tt}. For the types, also their frequency in the analyzed text will be listed.
#' @param corp.rm.class A character vector with word classes which should be dropped. The default value
#'    \code{"nonpunct"} has special meaning and will cause the result of
#'    \code{kRp.POS.tags(lang, c("punct","sentc"), list.classes=TRUE)} to be used.
#' @param corp.rm.tag A character vector with POS tags which should be dropped.
#' @param quiet Logical. If \code{FALSE}, short status messages will be shown.
#'    \code{TRUE} will also suppress all potential warnings regarding the validation status of measures.
#' @return An object of class \code{\link[koRpus]{kRp.TTR-class}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords LD
#' @seealso \code{\link[koRpus:kRp.POS.tags]{kRp.POS.tags}},
#'    \code{\link[koRpus]{kRp.tagged-class}}, \code{\link[koRpus]{kRp.TTR-class}}
#' @references
#'    Covington, M.A. & McFall, J.D. (2010). Cutting the Gordian Knot: The Moving-Average Type-Token Ratio (MATTR). 
#'      \emph{Journal of Quantitative Linguistics}, 17(2), 94--100.
#'
#'    Maas, H.-D., (1972). \"Uber den Zusammenhang zwischen Wortschatzumfang und L\"ange eines Textes. \emph{Zeitschrift f\"ur 
#'      Literaturwissenschaft und Linguistik}, 2(8), 73--96.
#'
#'   McCarthy, P.M. & Jarvis, S. (2007). vocd: A theoretical and empirical evaluation. \emph{Language Testing}, 24(4), 459--488.
#'
#'    McCarthy, P.M. & Jarvis, S. (2010). MTLD, vocd-D, and HD-D: A validation study of sophisticated approaces to lexical diversity 
#'      assessment. \emph{Behaviour Research Methods}, 42(2), 381--392.
#'
#'    Tweedie. F.J. & Baayen, R.H. (1998). How Variable May a Constant Be? Measures of Lexical Richness in Perspective.
#'     \emph{Computers and the Humanities}, 32(5), 323--352.
#' @import methods
#' @rdname lex.div-methods
#' @export
#' @examples
#' \dontrun{
#' lex.div(tagged.text)
#' }

#' @param ... Only used for the method generic.
setGeneric("lex.div", function(txt, ...) standardGeneric("lex.div"))

######################################################################
## if this signature changes, check kRp.lex.div.formulae() as well! ##
######################################################################

#' @export
#' @include 00_class_01_kRp.tagged.R
#' @include 00_class_03_kRp.txt.freq.R
#' @include 00_class_04_kRp.txt.trans.R
#' @include 00_class_05_kRp.analysis.R
#' @include koRpus-internal.R
#' @aliases lex.div lex.div,kRp.taggedText-method
#' @rdname lex.div-methods
setMethod("lex.div", signature(txt="kRp.taggedText"), function(txt, segment=100,
    factor.size=0.72, min.tokens=9, rand.sample=42, window=100,
    case.sens=FALSE, lemmatize=FALSE, detailed=FALSE,
    measure=c("TTR","MSTTR","MATTR","C","R","CTTR","U","S","K","Maas","HD-D","MTLD","MTLD-MA"),
    char=c("TTR","MATTR","C","R","CTTR","U","S","K","Maas","HD-D","MTLD","MTLD-MA"),
    char.steps=5, log.base=10,
    force.lang=NULL,
    keep.tokens=FALSE,
    corp.rm.class="nonpunct",
    corp.rm.tag=c(), quiet=FALSE){

    lex.div.results <- kRp.lex.div.formulae(txt=txt, segment=segment, factor.size=factor.size, min.tokens=min.tokens,
      rand.sample=rand.sample, window=window, case.sens=case.sens, lemmatize=lemmatize, detailed=detailed,
      measure=measure, char=char, char.steps=char.steps, log.base=log.base, force.lang=force.lang,
      keep.tokens=keep.tokens, corp.rm.class=corp.rm.class, corp.rm.tag=corp.rm.tag, quiet=quiet)

    return(lex.div.results)
  }
)

#' @export
#' @aliases lex.div,character-method
#' @rdname lex.div-methods
setMethod("lex.div", signature(txt="character"), function(txt, segment=100,
    factor.size=0.72, min.tokens=9, rand.sample=42, window=100,
    case.sens=FALSE, lemmatize=FALSE, detailed=FALSE,
    measure=c("TTR","MSTTR","MATTR","C","R","CTTR","U","S","K","Maas","HD-D","MTLD","MTLD-MA"),
    char=c("TTR","MATTR","C","R","CTTR","U","S","K","Maas","HD-D","MTLD","MTLD-MA"),
    char.steps=5, log.base=10,
    force.lang=NULL,
    keep.tokens=FALSE,
    corp.rm.class="nonpunct",
    corp.rm.tag=c(), quiet=FALSE){

    lex.div.results <- kRp.lex.div.formulae(txt=txt, segment=segment, factor.size=factor.size, min.tokens=min.tokens,
      rand.sample=rand.sample, window=window, case.sens=case.sens, lemmatize=lemmatize, detailed=detailed,
      measure=measure, char=char, char.steps=char.steps, log.base=log.base, force.lang=force.lang,
      keep.tokens=keep.tokens, corp.rm.class=corp.rm.class, corp.rm.tag=corp.rm.tag, quiet=quiet)

    return(lex.div.results)
  }
)
