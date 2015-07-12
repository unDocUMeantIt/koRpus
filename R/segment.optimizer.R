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


#' A function to optimize MSTTR segment sizes
#'
#' This function calculates an optimized segment size for \code{\link[koRpus:MSTTR]{MSTTR}}.
#'
#' When calculating the mean segmental type-token ratio (MSTTR), tokens are divided into
#' segments of a given size and analyzed. If at the end text is left over which won't fill another
#' full segment, it is discarded, i.e. information is lost. For interpretation it is debatable
#' which is worse: Dropping more or less actual token material, or variance in segment size between
#' analyzed texts. If you'd prefer the latter, this function might prove helpful.
#' 
#' Starting with a given text length, segment size and range to investigate, \code{segment.optimizer}
#' iterates through possible segment values. It returns the segment size which would drop the fewest
#' tokens (zero, if you're lucky). Should more than one value fulfill this demand, the one nearest to
#' the segment start value is taken. In cases, where still two values are equally far away from the
#' start value, it depends on the setting of \code{favour.min} if the smaller or larger segment size
#' is returned.
#'
#' @param txtlgth Integer value, size of text in tokens.
#' @param segment Integer value, start value of the segment size.
#' @param range Integer value, range around \code{segment} to search for better fitting sizes.
#' @param favour.min Logical, whether as a last ressort smaller or larger segment sizes should be
#'   prefered, if in doubt.
#' @return A numeric vector with two elements:
#'   \item{seg}{The optimized segment size}
#'   \item{drop}{The number of tokens that would be dropped using this segment size}
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords LD
#' @seealso \code{\link[koRpus:lex.div]{lex.div}}, \code{\link[koRpus:MSTTR]{MSTTR}}
#' @export
#' @examples
#' segment.optimizer(2014, favour.min=FALSE)

segment.optimizer <- function(txtlgth, segment=100, range=20, favour.min=TRUE){
  drops <- c()
  for (x in 0:(range %/% 2)) {
      new.segs <- c((segment - x), c(segment + x))
      # check whether smaller segments should be favoured to get the order of tests right
      if(!isTRUE(favour.min)){
        new.segs <- rev(new.segs)
      } else {}
      # modulo, to find optimal segment size
      dropped  <- txtlgth %% new.segs
        if(dropped[1] == 0){
          return(c(seg=new.segs[1], drop=dropped[1]))
        } else {}
        if(dropped[2] == 0){
          return(c(seg=new.segs[2], drop=dropped[2]))
        } else {}
      drops <- rbind(drops, c(seg=new.segs[1], drop=dropped[1]), c(seg=new.segs[2], drop=dropped[2]))
    }

  smallest.drops <- drops[which(drops[,"drop"] == min(drops[,"drop"])), ]

  # is there only one smallest value?
  if(!is.null(dim(smallest.drops))){
    # look for the segment size which is nearest to the start value
    segs.less.segment <- abs(smallest.drops[,"drop"] - segment)
    smallest.drops <- smallest.drops[which(segs.less.segment == min(segs.less.segment)), ]
    # in case there's still two values, take the favoured oneway.test
    if(!is.null(dim(smallest.drops))){
      if(isTRUE(favour.min)){
        smallest.drops <- smallest.drops[which.min(smallest.drops[,"seg"]), ]
      } else {
        smallest.drops <- smallest.drops[which.max(smallest.drops[,"seg"]), ]
      }
    } else {}
  } else {}

  return(smallest.drops)
}
