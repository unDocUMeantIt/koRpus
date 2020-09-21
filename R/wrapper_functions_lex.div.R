# Copyright 2010-2017 Meik Michalke <meik.michalke@hhu.de>
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


## C.ld()
#' Lexical diversity: Herdan's C
#' 
#' This is just a convenient wrapper function for \code{\link[koRpus:lex.div]{lex.div}}.
#'
#' Calculates Herdan's C. In contrast to
#' \code{\link[koRpus:lex.div]{lex.div}}, which by default calculates all possible measures and
#' their progressing characteristics, this function will only calculate the C value, and characteristics are
#' off by default.
#'
#' @param txt An object of class \code{\link[koRpus:kRp.text-class]{kRp.text}} containing the tagged text to be analyzed.
#' @param char Logical, defining whether data for plotting characteristic curves should be calculated.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:lex.div]{lex.div}} for details.
#' @return An object of class \code{\link[koRpus:kRp.TTR-class]{kRp.TTR}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords LD
#' @seealso \code{\link[koRpus:kRp.POS.tags]{kRp.POS.tags}},
#'  \code{\link[koRpus:kRp.text-class]{kRp.text}}, \code{\link[koRpus:kRp.TTR-class]{kRp.TTR}}
#' @export
#' @examples
#' \dontrun{
#' C.ld(tagged.text)
#' }

C.ld <- function(txt, char=FALSE, ...){
  if(isTRUE(char)){
    char.value <- "C"
  } else {
    char.value <- c()
  }
  results <- lex.div(txt=txt, measure="C", char=char.value, ...)
  return(results)
}
## end C.ld()


## CTTR()
#' Lexical diversity: Carroll's corrected TTR (CTTR)
#' 
#' This is just a convenient wrapper function for \code{\link[koRpus:lex.div]{lex.div}}.
#'
#' Calculates Carroll's corrected TTR (CTTR). In contrast to
#' \code{\link[koRpus:lex.div]{lex.div}}, which by default calculates all possible measures and
#' their progressing characteristics, this function will only calculate the CTTR value, and characteristics are
#' off by default.
#'
#' @param txt An object of class \code{\link[koRpus:kRp.text-class]{kRp.text}} containing the tagged text to be analyzed.
#' @param char Logical, defining whether data for plotting characteristic curves should be calculated.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:lex.div]{lex.div}} for details.
#' @return An object of class \code{\link[koRpus:kRp.TTR-class]{kRp.TTR}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords LD
#' @seealso \code{\link[koRpus:kRp.POS.tags]{kRp.POS.tags}},
#'  \code{\link[koRpus:kRp.text-class]{kRp.text}}, \code{\link[koRpus:kRp.TTR-class]{kRp.TTR}}
#' @export
#' @examples
#' \dontrun{
#' CTTR(tagged.text)
#' }

CTTR <- function(txt, char=FALSE, ...){
  if(isTRUE(char)){
    char.value <- "CTTR"
  } else {
    char.value <- c()
  }
  results <- lex.div(txt=txt, measure="CTTR", char=char.value, ...)
  return(results)
}
## end CTTR()


## HDD()
#' Lexical diversity: HD-D (vocd-d)
#' 
#' This is just a convenient wrapper function for \code{\link[koRpus:lex.div]{lex.div}}.
#'
#' This function calculates HD-D, an idealized version of vocd-d (see McCarthy & Jarvis, 2007). In contrast to
#' \code{\link[koRpus:lex.div]{lex.div}}, which by default calculates all possible measures and
#' their progressing characteristics, this function will only calculate the HD-D value, and characteristics are
#' off by default.
#'
#' @param txt An object of class \code{\link[koRpus:kRp.text-class]{kRp.text}} containing the tagged text to be analyzed.
#' @param rand.sample An integer value, how many tokens should be assumed to be drawn for calculating HD-D.
#' @param char Logical, defining whether data for plotting characteristic curves should be calculated.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:lex.div]{lex.div}} for details.
#' @return An object of class \code{\link[koRpus:kRp.TTR-class]{kRp.TTR}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @references
#'   McCarthy, P.M. & Jarvis, S. (2007). vocd: A theoretical and empirical evaluation. \emph{Language Testing}, 24(4), 459--488.
#' @keywords LD
#' @seealso \code{\link[koRpus:kRp.POS.tags]{kRp.POS.tags}},
#'  \code{\link[koRpus:kRp.text-class]{kRp.text}}, \code{\link[koRpus:kRp.TTR-class]{kRp.TTR}}
#' @export
#' @examples
#' \dontrun{
#' HDD(tagged.text)
#' }

HDD <- function(txt, rand.sample=42, char=FALSE, ...){
  if(isTRUE(char)){
    char.value <- "HD-D"
  } else {
    char.value <- c()
  }
  results <- lex.div(txt=txt, rand.sample=rand.sample, measure="HD-D", char=char.value, ...)
  return(results)
}
## end HDD()


## K.ld()
#' Lexical diversity: Yule's K
#' 
#' This is just a convenient wrapper function for \code{\link[koRpus:lex.div]{lex.div}}.
#'
#' This function calculates Yule's K. In contrast to
#' \code{\link[koRpus:lex.div]{lex.div}}, which by default calculates all possible measures and
#' their progressing characteristics, this function will only calculate the K value, and characteristics are
#' off by default.
#'
#' @param txt An object of class \code{\link[koRpus:kRp.text-class]{kRp.text}} containing the tagged text to be analyzed.
#' @param char Logical, defining whether data for plotting characteristic curves should be calculated.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:lex.div]{lex.div}} for details.
#' @return An object of class \code{\link[koRpus:kRp.TTR-class]{kRp.TTR}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords LD
#' @seealso \code{\link[koRpus:kRp.POS.tags]{kRp.POS.tags}},
#'  \code{\link[koRpus:kRp.text-class]{kRp.text}}, \code{\link[koRpus:kRp.TTR-class]{kRp.TTR}}
#' @export
#' @examples
#' \dontrun{
#' K.ld(tagged.text)
#' }

K.ld <- function(txt, char=FALSE, ...){
  if(isTRUE(char)){
    char.value <- "K"
  } else {
    char.value <- c()
  }
  results <- lex.div(txt=txt, measure="K", char=char.value, ...)
  return(results)
}
## end K.ld()


## maas()
#' Lexical diversity: Maas' indices
#' 
#' This is just a convenient wrapper function for \code{\link[koRpus:lex.div]{lex.div}}.
#'
#' This function calculates Maas' indices (\eqn{a^2} & \eqn{\lg{V_0}}). In contrast to
#' \code{\link[koRpus:lex.div]{lex.div}}, which by default calculates all possible measures and
#' their progressing characteristics, this function will only calculate the index values, and characteristics are
#' off by default.
#'
#' @param txt An object of class \code{\link[koRpus:kRp.text-class]{kRp.text}} containing the tagged text to be analyzed.
#' @param char Logical, defining whether data for plotting characteristic curves should be calculated.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:lex.div]{lex.div}} for details.
#' @return An object of class \code{\link[koRpus:kRp.TTR-class]{kRp.TTR}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords LD
#' @seealso \code{\link[koRpus:kRp.POS.tags]{kRp.POS.tags}},
#'  \code{\link[koRpus:kRp.text-class]{kRp.text}}, \code{\link[koRpus:kRp.TTR-class]{kRp.TTR}}
#' @export
#' @examples
#' \dontrun{
#' maas(tagged.text)
#' }

maas <- function(txt, char=FALSE, ...){
  if(isTRUE(char)){
    char.value <- "Maas"
  } else {
    char.value <- c()
  }
  results <- lex.div(txt=txt, measure="Maas", char=char.value, ...)
  return(results)
}
## end maas()


## MATTR()
#' Lexical diversity: Moving-Average Type-Token Ratio (MATTR)
#'
#' This is just a convenient wrapper function for \code{\link[koRpus:lex.div]{lex.div}}.
#'
#' This function calculates the moving-average type-token ratio (MATTR). In contrast to
#' \code{\link[koRpus:lex.div]{lex.div}}, which by default calculates all possible measures and
#' their progressing characteristics, this function will only calculate the MATTR value.
#'
#' @param txt An object of class \code{\link[koRpus:kRp.text-class]{kRp.text}} containing the tagged text to be analyzed.
#' @param window An integer value for MATTR, defining how many tokens the moving window should include.
#' @param char Logical, defining whether data for plotting characteristic curves should be calculated.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:lex.div]{lex.div}} for details.
#' @return An object of class \code{\link[koRpus:kRp.TTR-class]{kRp.TTR}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords LD
#' @seealso \code{\link[koRpus:kRp.POS.tags]{kRp.POS.tags}},
#'  \code{\link[koRpus:kRp.text-class]{kRp.text}}, \code{\link[koRpus:kRp.TTR-class]{kRp.TTR}}
#' @references
#'    Covington, M.A. & McFall, J.D. (2010). Cutting the Gordian Knot: The Moving-Average Type-Token Ratio (MATTR).
#'      \emph{Journal of Quantitative Linguistics}, 17(2), 94--100.
#' @export
#' @examples
#' \dontrun{
#' MATTR(tagged.text)
#' }

MATTR <- function(txt, window=100, char=FALSE, ...){
  if(isTRUE(char)){
    char.value <- "MATTR"
  } else {
    char.value <- c()
  }

  results <- lex.div(txt=txt, window=window, measure="MATTR", char=char.value, ...)
  return(results)
}
## end MATTR()


## MSTTR()
#' Lexical diversity: Mean Segmental Type-Token Ratio (MSTTR)
#'
#' This is just a convenient wrapper function for \code{\link[koRpus:lex.div]{lex.div}}.
#'
#' This function calculates the mean segmental type-token ratio (MSTTR). In contrast to
#' \code{\link[koRpus:lex.div]{lex.div}}, which by default calculates all possible measures and
#' their progressing characteristics, this function will only calculate the MSTTR value.
#'
#' @param txt An object of class \code{\link[koRpus:kRp.text-class]{kRp.text}} containing the tagged text to be analyzed.
#' @param segment An integer value, defining how many tokens should form one segment.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:lex.div]{lex.div}} for details.
#' @return An object of class \code{\link[koRpus:kRp.TTR-class]{kRp.TTR}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords LD
#' @seealso \code{\link[koRpus:kRp.POS.tags]{kRp.POS.tags}},
#'  \code{\link[koRpus:kRp.text-class]{kRp.text}}, \code{\link[koRpus:kRp.TTR-class]{kRp.TTR}}
#' @export
#' @examples
#' \dontrun{
#' MSTTR(tagged.text)
#' }

MSTTR <- function(txt, segment=100, ...){
  results <- lex.div(txt=txt, segment=segment, measure="MSTTR", char=c(), ...)
  return(results)
}
## end MSTTR()


## MTLD()
#' Lexical diversity: Measure of Textual Lexical Diversity (MTLD)
#'
#' This is just a convenient wrapper function for \code{\link[koRpus:lex.div]{lex.div}}.
#'
#' This function calculates the measure of textual lexical diversity (MTLD; see McCarthy & Jarvis, 2010). In contrast to
#' \code{\link[koRpus:lex.div]{lex.div}}, which by default calculates all possible measures and
#' their progressing characteristics, this function will only calculate the MTLD value, and characteristics are
#' off by default.
#' 
#' If you set \code{MA=TRUE}, the newer MTLD-MA (moving-average method) is used instead of the classic MTLD.
#'
#' @param txt An object of class \code{\link[koRpus:kRp.text-class]{kRp.text}} containing the tagged text to be analyzed.
#' @param factor.size A real number between 0 and 1, defining the MTLD factor size.
#' @param min.tokens An integer value, how many tokens a full factor must at least have to be considered for the MTLD-MA result.
#' @param steps An integer value for MTLD-MA, defining the step size for the moving window, in tokens. The original proposal
#'    uses an incremet of 1. If you increase this value, computation will be faster, but your value can only remain a good estimate if
#'    the text is long enough.
#' @param detailed Logical, whether full details of the analysis should be calculated. It defines
#'    if all factors should be kept in the object. This slows down calculations considerably.
#' @param char Logical, defining whether data for plotting characteristic curves should be calculated.
#' @param MA Logical, defining whether the newer moving-average algorithm (MTLD-MA) should be calculated.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:lex.div]{lex.div}} for details.
#' @return An object of class \code{\link[koRpus:kRp.TTR-class]{kRp.TTR}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords LD
#' @seealso \code{\link[koRpus:kRp.POS.tags]{kRp.POS.tags}},
#'  \code{\link[koRpus:kRp.text-class]{kRp.text}}, \code{\link[koRpus:kRp.TTR-class]{kRp.TTR}}
#' @references McCarthy, P. M. & Jarvis, S. (2010). MTLD, vocd-D, and HD-D: A validation study of sophisticated approaces to lexical diversity assessment.
#'    \emph{Behaviour Research Methods}, 42(2), 381--392.
#' @export
#' @examples
#' \dontrun{
#' MTLD(tagged.text)
#' }

MTLD <- function(txt, factor.size=0.72, min.tokens=9, detailed=FALSE, char=FALSE, MA=FALSE, steps=1, ...){
  if(isTRUE(MA)){
    measure <- "MTLD-MA"
  } else {
    measure <- "MTLD"
  }

  if(isTRUE(char)){
    char.value <- measure
  } else {
    char.value <- c()
  }

  results <- lex.div(txt=txt, factor.size=factor.size, min.tokens=min.tokens, MTLDMA.steps=steps, detailed=detailed, measure=measure, char=char.value, ...)
  return(results)
}
## end MTLD()


## R.ld()
#' Lexical diversity: Guiraud's R
#' 
#' This is just a convenient wrapper function for \code{\link[koRpus:lex.div]{lex.div}}.
#'
#' This function calculates Guiraud's R. In contrast to
#' \code{\link[koRpus:lex.div]{lex.div}}, which by default calculates all possible measures and
#' their progressing characteristics, this function will only calculate the R value, and characteristics are
#' off by default.
#'
#' @param txt An object of class \code{\link[koRpus:kRp.text-class]{kRp.text}} containing the tagged text to be analyzed.
#' @param char Logical, defining whether data for plotting characteristic curves should be calculated.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:lex.div]{lex.div}} for details.
#' @return An object of class \code{\link[koRpus:kRp.TTR-class]{kRp.TTR}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords LD
#' @seealso \code{\link[koRpus:kRp.POS.tags]{kRp.POS.tags}},
#'  \code{\link[koRpus:kRp.text-class]{kRp.text}}, \code{\link[koRpus:kRp.TTR-class]{kRp.TTR}}
#' @export
#' @examples
#' \dontrun{
#' R.ld(tagged.text)
#' }

R.ld <- function(txt, char=FALSE, ...){
  if(isTRUE(char)){
    char.value <- "R"
  } else {
    char.value <- c()
  }
  results <- lex.div(txt=txt, measure="R", char=char.value, ...)
  return(results)
}
## end R.ld()


## S.ld()
#' Lexical diversity: Summer's S
#'
#' This is just a convenient wrapper function for \code{\link[koRpus:lex.div]{lex.div}}.
#'
#' This function calculates Summer's S. In contrast to
#' \code{\link[koRpus:lex.div]{lex.div}}, which by default calculates all possible measures and
#' their progressing characteristics, this function will only calculate the S value, and characteristics are
#' off by default.
#'
#' @param txt An object of class \code{\link[koRpus:kRp.text-class]{kRp.text}} containing the tagged text to be analyzed.
#' @param char Logical, defining whether data for plotting characteristic curves should be calculated.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:lex.div]{lex.div}} for details.
#' @return An object of class \code{\link[koRpus:kRp.TTR-class]{kRp.TTR}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords LD
#' @seealso \code{\link[koRpus:kRp.POS.tags]{kRp.POS.tags}},
#'  \code{\link[koRpus:kRp.text-class]{kRp.text}}, \code{\link[koRpus:kRp.TTR-class]{kRp.TTR}}
#' @export
#' @examples
#' \dontrun{
#' S.ld(tagged.text)
#' }

S.ld <- function(txt, char=FALSE, ...){
  if(isTRUE(char)){
    char.value <- "S"
  } else {
    char.value <- c()
  }
  results <- lex.div(txt=txt, measure="S", char=char.value, ...)
  return(results)
}
## end S.ld()


## TTR()
#' Lexical diversity: Type-Token Ratio
#'
#' This is just a convenient wrapper function for \code{\link[koRpus:lex.div]{lex.div}}.
#'
#' This function calculates the classic type-token ratio (TTR). In contrast to
#' \code{\link[koRpus:lex.div]{lex.div}}, which by default calculates all possible measures and
#' their progressing characteristics, this function will only calculate the TTR value, and characteristics are
#' off by default.
#'
#' @param txt An object of class \code{\link[koRpus:kRp.text-class]{kRp.text}} containing the tagged text to be analyzed.
#' @param char Logical, defining whether data for plotting characteristic curves should be calculated.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:lex.div]{lex.div}} for details.
#' @return An object of class \code{\link[koRpus:kRp.TTR-class]{kRp.TTR}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords LD
#' @seealso \code{\link[koRpus:kRp.POS.tags]{kRp.POS.tags}},
#'  \code{\link[koRpus:kRp.text-class]{kRp.text}}, \code{\link[koRpus:kRp.TTR-class]{kRp.TTR}}
#' @export
#' @examples
#' \dontrun{
#' TTR(tagged.text)
#' }

TTR <- function(txt, char=FALSE, ...){
  if(isTRUE(char)){
    char.value <- "TTR"
  } else {
    char.value <- c()
  }
  results <- lex.div(txt=txt, measure="TTR", char=char.value, ...)
  return(results)
}
## end TTR()


## U.ld()
#' Lexical diversity: Uber Index (U)
#'
#' This is just a convenient wrapper function for \code{\link[koRpus:lex.div]{lex.div}}.
#'
#' This function calculates the Uber Index (U). In contrast to
#' \code{\link[koRpus:lex.div]{lex.div}}, which by default calculates all possible measures and
#' their progressing characteristics, this function will only calculate the U value, and characteristics are
#' off by default.
#'
#' @param txt An object of class \code{\link[koRpus:kRp.text-class]{kRp.text}} containing the tagged text to be analyzed.
#' @param char Logical, defining whether data for plotting characteristic curves should be calculated.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:lex.div]{lex.div}} for details.
#' @return An object of class \code{\link[koRpus:kRp.TTR-class]{kRp.TTR}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords LD
#' @seealso \code{\link[koRpus:kRp.POS.tags]{kRp.POS.tags}},
#'  \code{\link[koRpus:kRp.text-class]{kRp.text}}, \code{\link[koRpus:kRp.TTR-class]{kRp.TTR}}
#' @export
#' @examples
#' \dontrun{
#' U.ld(tagged.text)
#' }

U.ld <- function(txt, char=FALSE, ...){
  if(isTRUE(char)){
    char.value <- "U"
  } else {
    char.value <- c()
  }
  results <- lex.div(txt=txt, measure="U", char=char.value, ...)
  return(results)
}
## end U.ld()
