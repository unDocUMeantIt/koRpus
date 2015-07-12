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


#' Measure readability
#'
#' These methods calculate several readability indices.
#'
#' In the following formulae, \eqn{W} stands for the number of words, \eqn{St} for the number of sentences, \eqn{C} for the number of
#' characters (usually meaning letters), \eqn{Sy} for the number of syllables, \eqn{W_{3Sy}} for the number of words with at least three syllables,
#' \eqn{W_{<3Sy}} for the number of words with less than three syllables, \eqn{W^{1Sy}}
#' for words with exactly one syllable, \eqn{W_{6C}} for the number of words with at least six letters, and \eqn{W_{-WL}} for the number
#' of words which are not on a certain word list (explained where needed).
#' \describe{
#'    \item{\code{"ARI"}:}{\emph{Automated Readability Index}:
#'      \deqn{ARI = 0.5 \times \frac{W}{St} + 4.71 \times \frac{C}{W} - 21.43}
#'      If \code{parameters} is set to \code{ARI="NRI"}, the revised parameters from the Navy Readability Indexes are used:
#'      \deqn{ARI_{NRI} = 0.4 \times \frac{W}{St} + 6 \times \frac{C}{W} - 27.4}
#'      If \code{parameters} is set to \code{ARI="simple"}, the simplified formula is calculated:
#'      \deqn{ARI_{simple} = \frac{W}{St} + 9 \times \frac{C}{W}}
#'
#'      Wrapper function: \code{\link[koRpus:ARI]{ARI}}
#'    }
#'    \item{\code{"Bormuth"}:}{\emph{Bormuth Mean Cloze} & Grade Placement:
#'      \deqn{
#'      B_{MC} = 0.886593 - \left( 0.08364 \times \frac{C}{W} \right) +  0.161911 \times \left(\frac{W_{-WL}}{W} \right)^3
#'      }
#'      \deqn{
#'       - 0.21401 \times \left(\frac{W}{St} \right) + 0.000577 \times \left(\frac{W}{St} \right)^2
#'      }
#'      \deqn{
#'       - 0.000005 \times \left(\frac{W}{St} \right)^3
#'      }
#'      \strong{Note:} This index needs the long Dale-Chall list of 3000 familiar (english) words to compute \eqn{W_{-WL}}. That is, you must have a copy of
#'      this word list and provide it via the \code{word.lists=list(Bormuth=<your.list>)} parameter!
#'      \deqn{B_{GP} = 4.275 + 12.881 \times B_{MC} - (34.934 \times B_{MC}^2) + (20.388 \times B_{MC}^3)
#'      }
#'      \deqn{
#'       + (26.194C - 2.046 C_{CS}^2) - (11.767 C_{CS}^3) - (44.285  \times B_{MC} \times C_{CS})
#'      }
#'      \deqn{
#'       + (97.620 \times (B_{MC} \times C_{CS})^2) - (59.538 \times (B_{MC} \times C_{CS})^3)}
#'      Where \eqn{C_{CS}} represents the cloze criterion score (35\% by default).
#'
#'      Wrapper function: \code{\link[koRpus:bormuth]{bormuth}}
#'    }
#'    \item{\code{"Coleman"}:}{\emph{Coleman's Readability Formulas}:
#'      \deqn{C_1 = 1.29 \times \left( \frac{100 \times W^{1Sy}}{W} \right) - 38.45}
#'      \deqn{C_2 = 1.16 \times \left( \frac{100 \times W^{1Sy}}{W} \right) + 1.48 \times \left( \frac{100 \times St}{W} \right) - 37.95}
#'      \deqn{C_3 = 1.07 \times \left( \frac{100 \times W^{1Sy}}{W} \right) + 1.18 \times \left( \frac{100 \times St}{W} \right)
#'        + 0.76 \times \left( \frac{100 \times W_{pron}}{W} \right) - 34.02}
#'      \deqn{C_4 = 1.04 \times \left( \frac{100 \times W^{1Sy}}{W} \right) + 1.06 \times \left( \frac{100 \times St}{W} \right) \\
#'        + 0.56 \times \left( \frac{100 \times W_{pron}}{W} \right) - 0.36  \times \left( \frac{100 \times W_{prep}}{W} \right) - 26.01}
#'      Where \eqn{W_{pron}} is the number of pronouns, and \eqn{W_{prep}} the number of prepositions.
#'
#'      Wrapper function: \code{\link[koRpus:coleman]{coleman}}
#'    }
#'    \item{\code{"Coleman.Liau"}:}{First estimates cloze percentage, then calculates grade equivalent:
#'      \deqn{CL_{ECP} = 141.8401 - 0.214590 \times \frac{100 \times C}{W} + 1.079812 \times \frac{100 \times St}{W}}
#'      \deqn{CL_{grade} = -27.4004 \times \frac{CL_{ECP}}{100} + 23.06395}
#'      The short form is also calculated:
#'      \deqn{CL_{short} = 5.88 \times \frac{C}{W} - 29.6 \times \frac{St}{W} - 15.8}
#'
#'      Wrapper function: \code{\link[koRpus:coleman.liau]{coleman.liau}}
#'    }
#'    \item{\code{"Dale.Chall"}:}{\emph{New Dale-Chall Readability Formula}. By default the revised formula (1995) is calculated:
#'      \deqn{DC_{new} = 64 - 0.95 \times{} \frac{100 \times{} W_{-WL}}{W} - 0.69 \times{} \frac{W}{St} }
#'      This will result in a cloze score which is then looked up in a grading table. If \code{parameters} is set to \code{Dale.Chall="old"},
#'      the original formula (1948) is used:
#'      \deqn{DC_{old} = 0.1579 \times{} \frac{100 \times{} W_{-WL}}{W} + 0.0496 \times{} \frac{W}{St} + 3.6365 }
#'      If \code{parameters} is set to \code{Dale.Chall="PSK"}, the revised parameters by Powers-Sumner-Kearl (1958) are used:
#'      \deqn{DC_{PSK} =  0.1155 \times{} \frac{100 \times{} W_{-WL}}{W} + 0.0596  \times{} \frac{W}{St} + 3.2672 }
#'      \strong{Note:} This index needs the long Dale-Chall list of 3000 familiar (english) words to compute \eqn{W_{-WL}}. That is, you must have a copy of
#'      this word list and provide it via the \code{word.lists=list(Dale.Chall=<your.list>)} parameter!
#'
#'      Wrapper function: \code{\link[koRpus:dale.chall]{dale.chall}}
#'    }
#'    \item{\code{"Danielson.Bryan"}:}{
#'      \deqn{DB_1 = \left( 1.0364 \times \frac{C}{Bl} \right) + \left( 0.0194 \times \frac{C}{St} \right) - 0.6059}
#'      \deqn{DB_2 = 131.059 - \left( 10.364 \times \frac{C}{Bl} \right) - \left( 0.194 \times \frac{C}{St} \right)}
#'      Where \eqn{Bl} means blanks between words, which is not really counted in this implementation, but estimated
#'      by \eqn{words - 1}. \eqn{C} is interpreted as literally all characters.
#'
#'      Wrapper function: \code{\link[koRpus:danielson.bryan]{danielson.bryan}}
#'    }
#'    \item{\code{"Dickes.Steiwer"}:}{\emph{Dickes-Steiwer Handformel}:
#'      \deqn{DS = 235.95993 - \left( 73.021 \times \frac{C}{W} \right) - \left(12.56438 \times \frac{W}{St} \right) - \left(50.03293 \times TTR \right)}
#'      Where \eqn{TTR} refers to the type-token ratio, which will be calculated case-insensitive by default.
#'
#'      Wrapper function: \code{\link[koRpus:dickes.steiwer]{dickes.steiwer}}
#'    }
#'    \item{\code{"DRP"}:}{\emph{Degrees of Reading Power}. Uses the Bormuth Mean Cloze Score:
#'      \deqn{DRP = (1 - B_{MC}) \times 100}
#'      This formula itself has no parameters.
#'      \strong{Note:} The Bormuth index needs the long Dale-Chall list of 3000 familiar (english) words to compute \eqn{W_{-WL}}.
#'      That is, you must have a copy of this word list and provide it via the \code{word.lists=list(Bormuth=<your.list>)} parameter!
#'      Wrapper function: \code{\link[koRpus:DRP]{DRP}}
#'    }
#'    \item{\code{"ELF"}:}{Fang's \emph{Easy Listening Formula}:
#'      \deqn{ELF = \frac{W_{2Sy}}{St}}
#'
#'      Wrapper function: \code{\link[koRpus:ELF]{ELF}}
#'    }
#'    \item{\code{"Farr.Jenkins.Paterson"}:}{A simplified version of Flesch Reading Ease:
#'      \deqn{-31.517 - 1.015 \times \frac{W}{St} + 1.599 \times \frac{{W^{1Sy}}}{W}}
#'      If \code{parameters} is set to \code{Farr.Jenkins.Paterson="PSK"}, the revised parameters by Powers-Sumner-Kearl (1958) are used:
#'      \deqn{8.4335 + 0.0923 \times \frac{W}{St} - 0.0648 \times \frac{{W^{1Sy}}}{W}}
#'      Wrapper function: \code{\link[koRpus:farr.jenkins.paterson]{farr.jenkins.paterson}}
#'   }
#'   \item{\code{"Flesch"}:}{\emph{Flesch Reading Ease}:
#'      \deqn{ 206.835 - 1.015 \times \frac{W}{St} - 84.6 \times \frac{Sy}{W}}
#'      Certain internationalisations of the parameters are also implemented. They can be used by setting
#'      the \code{Flesch} parameter to \code{"es"} (Fernandez-Huerta),  \code{"es-s"} (Szigriszt), \code{"nl"} (Douma),
#'      \code{"de"} (Amstad's Verst\"andlichkeitsindex), or \code{"fr"} (Kandel-Moles).
#'      If \code{parameters} is set to \code{Flesch="PSK"}, the revised parameters by Powers-Sumner-Kearl (1958) are used
#'      to calculate a grade level:
#'      \deqn{Flesch_{PSK} = 0.0778 \times \frac{W}{St} + 4.55 \times \frac{Sy}{W} - 2.2029}
#'
#'      Wrapper function: \code{\link[koRpus:flesch]{flesch}}
#'    }
#'    \item{\code{"Flesch.Kincaid"}:}{\emph{Flesch-Kincaid Grade Level}:
#'      \deqn{0.39 \times \frac{W}{St} + 11.8 \times \frac{Sy}{W} - 15.59}
#'
#'      Wrapper function: \code{\link[koRpus:flesch.kincaid]{flesch.kincaid}}
#'    }
#'    \item{\code{"FOG"}:}{Gunning \emph{Frequency of Gobbledygook}:
#'      \deqn{FOG = 0.4 \times \left( \frac{W}{St} + \frac{100 \times W_{3Sy}}{W} \right)}
#'      If \code{parameters} is set to \code{FOG="PSK"}, the revised parameters by Powers-Sumner-Kearl (1958) are used:
#'      \deqn{FOG_{PSK} = 3.0680 + \left( 0.0877 \times \frac{W}{St} \right) + \left(0.0984 \times \frac{100 \times W_{3Sy}}{W} \right)}
#'      If \code{parameters} is set to \code{FOG="NRI"}, the new FOG count from the Navy Readability Indexes is used:
#'      \deqn{FOG_{new} = \frac{\frac{W_{<3Sy} + (3 * W_{3Sy})}{\frac{100 \times St}{W}} - 3}{2}}
#'      If the text was POS-tagged accordingly, proper nouns and combinations of only easy words will not be counted as hard words,
#'      and the syllables of verbs ending in "-ed", "-es" or "-ing" will be counted without these suffixes.
#'
#'      Wrapper function: \code{\link[koRpus:FOG]{FOG}}
#'    }
#'    \item{\code{"FORCAST"}:}{
#'      \deqn{FORCAST = 20 - \frac{W^{1Sy} \times \frac{150}{W}}{10}}
#'      If \code{parameters} is set to \code{FORCAST="RGL"}, the parameters for the precise reading grade level are used (see Klare, 1975, pp. 84--85):
#'      \deqn{FORCAST_{RGL} = 20.43 - 0.11 \times W^{1Sy} \times \frac{150}{W}}
#'
#'      Wrapper function: \code{\link[koRpus:FORCAST]{FORCAST}}
#'    }
#'    \item{\code{"Fucks"}:}{Fucks' \emph{Stilcharakteristik}:
#'      \deqn{Fucks = \frac{C}{W} \times \frac{W}{St}}
#'      This simple formula has no parameters.
#'
#'      Wrapper function: \code{\link[koRpus:fucks]{fucks}}
#'    }
#'    \item{\code{"Harris.Jacobson"}:}{\emph{Revised Harris-Jacobson Readability Formulas} (Harris & Jacobson, 1974):
#'      For primary-grade material:
#'      \deqn{HJ_1 = 0.094 \times \frac{100 \times{} W_{-WL}}{W} + 0.168 \times \frac{W}{St} + 0.502}
#'      For material above third grade:
#'      \deqn{HJ_2 = 0.140 \times \frac{100 \times{} W_{-WL}}{W} + 0.153 \times \frac{W}{St} + 0.560}
#'      For material below forth grade:
#'      \deqn{HJ_3 = 0.158 \times \frac{W}{St} + 0.055 \times \frac{100 \times{} W_{6C}}{W} + 0.355}
#'      For material below forth grade:
#'      \deqn{HJ_4 = 0.070 \times \frac{100 \times{} W_{-WL}}{W} + 0.125 \times \frac{W}{St} + 0.037 \times \frac{100 \times{} W_{6C}}{W} + 0.497}
#'      For material above third grade:
#'      \deqn{HJ_5 = 0.118 \times \frac{100 \times{} W_{-WL}}{W} + 0.134 \times \frac{W}{St} + 0.032 \times \frac{100 \times{} W_{6C}}{W} + 0.424}
#'      \strong{Note:} This index needs the short Harris-Jacobson word list for grades 1 and 2 (english) to compute \eqn{W_{-WL}}. That is, you must have a copy of
#'      this word list and provide it via the \code{word.lists=list(Harris.Jacobson=<your.list>)} parameter!
#'
#'      Wrapper function: \code{\link[koRpus:harris.jacobson]{harris.jacobson}}
#'    }
#'    \item{\code{"Linsear.Write"} (O'Hayre, undated, see Klare, 1975, p. 85):}{
#'      \deqn{LW_{raw} = \frac{100 - \frac{100 \times W_{<3Sy}}{W} + \left( 3 \times \frac{100 \times W_{3Sy}}{W} \right)}{\frac{100 \times St}{W}}}
#'      \deqn{LW(LW_{raw} \leq 20) = \frac{LW_{raw} - 2}{2}}
#'      \deqn{LW(LW_{raw} > 20) = \frac{LW_{raw}}{2}}
#'
#'      Wrapper function: \code{\link[koRpus:linsear.write]{linsear.write}}
#'    }
#'   \item{\code{"LIX"}}{Bj\"ornsson's \emph{L\"asbarhetsindex}. Originally proposed for Swedish texts, calculated by:
#'      \deqn{\frac{W}{St} + \frac{100 \times{} W_{7C}}{W}}{LIX = W7C / St + (L*100) / W}
#'      Texts with a LIX < 25 are considered very easy, around 40 normal, and > 55 very difficult to read.
#'
#'      Wrapper function: \code{\link[koRpus:LIX]{LIX}}
#'    }
#'    \item{\code{"nWS"}:}{\emph{Neue Wiener Sachtextformeln} (Bamberger & Vanecek, 1984):
#'      \deqn{nWS_1 = 19.35 \times \frac{W_{3Sy}}{W} + 0.1672 \times \frac{W}{St} + 12.97 \times \frac{W_{6C}}{W} - 3.27 \times \frac{W^{1Sy}}{W} - 0.875}
#'      \deqn{nWS_2 = 20.07 \times \frac{W_{3Sy}}{W} + 0.1682 \times \frac{W}{St} + 13.73 \times \frac{W_{6C}}{W} - 2.779}
#'      \deqn{nWS_3 = 29.63 \times \frac{W_{3Sy}}{W} + 0.1905 \times \frac{W}{St} - 1.1144}
#'      \deqn{nWS_4 = 27.44 \times \frac{W_{3Sy}}{W} + 0.2656 \times \frac{W}{St} - 1.693}
#'
#'      Wrapper function: \code{\link[koRpus:nWS]{nWS}}
#'    }
#'    \item{\code{"RIX"}}{Anderson's \emph{Readability Index}. A simplified version of LIX:
#'      \deqn{\frac{W_{7C}}{St}}{RIX = W7C / St}
#'      Texts with a RIX < 1.8 are considered very easy, around 3.7 normal, and > 7.2 very difficult to read.
#'
#'      Wrapper function: \code{\link[koRpus:RIX]{RIX}}
#'    }
#'    \item{\code{"SMOG"}:}{\emph{Simple Measure of Gobbledygook}. By default calculates formula D by McLaughlin (1969):
#'      \deqn{SMOG = 1.043 \times \sqrt{W_{3Sy} \times \frac{30}{St}} + 3.1291}
#'      If \code{parameters} is set to \code{SMOG="C"}, formula C will be calculated:
#'      \deqn{SMOG_{C} = 0.9986 \times \sqrt{W_{3Sy} \times \frac{30}{St} + 5} + 2.8795}
#'      If \code{parameters} is set to \code{SMOG="simple"}, the simplified formula is used:
#'      \deqn{SMOG_{simple} = \sqrt{W_{3Sy} \times \frac{30}{St}} + 3}
#'      If \code{parameters} is set to \code{SMOG="de"}, the formula adapted to German texts ("Qu", Bamberger & Vanecek, 1984, p. 78) is used:
#'      \deqn{SMOG_{de} = \sqrt{W_{3Sy} \times \frac{30}{St}} - 2}
#'
#'      Wrapper function: \code{\link[koRpus:SMOG]{SMOG}}
#'    }
#'    \item{\code{"Spache"}:}{\emph{Spache Revised Formula (1974)}:
#'      \deqn{Spache = 0.121 \times \frac{W}{St} + 0.082 \times{} \frac{100 \times{} W_{-WL}}{W} + 0.659}
#'      If \code{parameters} is set to \code{Spache="old"}, the original parameters (Spache, 1953) are used:
#'      \deqn{Spache_{old} = 0.141 \times \frac{W}{St} + 0.086 \times{} \frac{100 \times{} W_{-WL}}{W} + 0.839}
#'      \strong{Note:} The revised index needs the revised Spache word list (see Klare, 1975, p. 73), and the old index the short Dale-Chall list of
#'      769 familiar (english) words to compute \eqn{W_{-WL}}. That is, you must have a copy of this word list and provide it via the
#'      \code{word.lists=list(Spache=<your.list>)} parameter!
#'
#'      Wrapper function: \code{\link[koRpus:spache]{spache}}
#'    }
#'    \item{\code{"Strain"}:}{\emph{Strain Index}. This index was proposed in [1]:
#'      \deqn{Sy \times{} \frac{1}{St / 3} \times{} \frac{1}{10}}
#'
#'      Wrapper function: \code{\link[koRpus:strain]{strain}}
#'    }
#'    \item{\code{"Traenkle.Bailer"}:}{\emph{Tr\"anke-Bailer Formeln}. These two formulas were the result of a re-examination of the ones proposed
#'      by Dickes-Steiwer. They try to avoid the usage of the type-token ratio, which is dependent on text length (Tr\"ankle, & Bailer, 1984):
#'      \deqn{TB1 = 224.6814 - \left(79.8304 \times \frac{C}{W} \right) - \left(12.24032 \times \frac{W}{St} \right) - \left(1.292857 \times \frac{100 \times{} W_{prep}}{W} \right)}
#'      \deqn{TB2 = 234.1063 - \left(96.11069 \times \frac{C}{W} \right) - \left(2.05444 \times \frac{100 \times{} W_{prep}}{W} \right) - \left(1.02805 \times \frac{100 \times{} W_{conj}}{W} \right)}
#'      Where \eqn{W_{prep}} refers to the number of prepositions, and \eqn{W_{conj}} to the number of conjunctions.
#'
#'      Wrapper function: \code{\link[koRpus:traenkle.bailer]{traenkle.bailer}}
#'    }
#'    \item{\code{"TRI"}:}{Kuntzsch's \emph{Text-Redundanz-Index}. Intended mainly for German newspaper comments.
#'      \deqn{TRI = \left(0.449 \times W^{1Sy}\right) - \left(2.467 \times Ptn\right) - \left(0.937 \times Frg\right) - 14.417}
#'      Where \eqn{Ptn} is the number of punctuation marks and \eqn{Frg} the number of foreign words.
#'
#'      Wrapper function: \code{\link[koRpus:TRI]{TRI}}
#'    }
#'    \item{\code{"Tuldava"}:}{Tuldava's \emph{Text Difficulty Formula}. Supposed to be rather independent of specific languages (Grzybek, 2010).
#'      \deqn{TD = \frac{Sy}{W} \times ln\left( \frac{W}{St} \right)}
#'
#'      Wrapper function: \code{\link[koRpus:tuldava]{tuldava}}
#'    }
#'    \item{\code{"Wheeler.Smith"}:}{Intended for english texts in primary grades 1--4 (Wheeler & Smith, 1954):
#'      \deqn{WS = \frac{W}{St} \times \frac{10 \times{} W_{2Sy}}{W}}
#'      If \code{parameters} is set to \code{Wheeler.Smith="de"}, the calculation stays the same, but grade placement
#'      is done according to Bamberger & Vanecek (1984), that is for german texts.
#'
#'      Wrapper function: \code{\link[koRpus:wheeler.smith]{wheeler.smith}}
#'    }
#' }
#'
#' By default, if the text has to be tagged yet, the language definition is queried by calling \code{get.kRp.env(lang=TRUE)} internally.
#' Or, if \code{txt} has already been tagged, by default the language definition of that tagged object is read
#' and used. Set \code{force.lang=get.kRp.env(lang=TRUE)} or to any other valid value, if you want to forcibly overwrite this
#' default behaviour, and only then. See \code{\link[koRpus:kRp.POS.tags]{kRp.POS.tags}} for all supported languages.
#'
#' @note To get a printout of the default parameters like they're set if no other parameters are specified, call \code{readability(parameters="dput")}.
#' In case you want to provide different parameters, you must provide a complete set for an index, or special parameters that are
#' mentioned in the index descriptions above (e.g., "PSK", if appropriate).
#'
#' @param txt.file Either an object of class \code{\link[koRpus]{kRp.tagged-class}}, \code{\link[koRpus]{kRp.txt.freq-class}},
#'    \code{\link[koRpus]{kRp.analysis-class}} or \code{\link[koRpus]{kRp.txt.trans-class}}, or a character vector which must be be
#'    a valid path to a file containing the text to be analyzed. If the latter, \code{force.lang} must be set as well, and
#'    the language specified must be supported by both \code{\link[koRpus:treetag]{treetag}} and \code{\link[koRpus:hyphen]{hyphen}}
#' @param hyphen An object of class kRp.hyphen. If \code{NULL}, the text will be hyphenated automatically. All syllable handling will
#'    be skipped automatically if it's not needed for the selected indices.
#' @param index A character vector, indicating which indices should actually be computed.
#' @param parameters A list with named magic numbers, defining the relevant parameters for each index. If none are given,
#'    the default values are used.
#' @param word.lists A named list providing the word lists for indices which need one. If \code{NULL} or missing, the indices will be
#'    skipped and a warning is giving. Actual word lists can be provided as either a vector (or matrix or data.frame with only one column),
#'    or as a file name, where this file must contain one word per line. Alternatively, you can provide the number of words which are not
#'    on the list, directly.
#' @param fileEncoding A character string naming the encoding of the word list files (if they are files). "ISO_8859-1" or "UTF-8" should work
#'    in most cases.
#' @param tagger A character string pointing to the tokenizer/tagger command you want to use for basic text analysis. Can be omitted if
#'    \code{txt.file} is already of class \code{kRp.tagged-class}. Defaults to \code{tagger="kRp.env"} to get the settings by
#'    \code{\link[koRpus:get.kRp.env]{get.kRp.env}}. Set to \code{"tokenize"} to use \code{\link[koRpus:tokenize]{tokenize}}.
#' @param force.lang A character string defining the language to be assumed for the text, by force.
#' @param sentc.tag A character vector with POS tags which indicate a sentence ending. The default value \code{"sentc"} has special meaning and
#'    will cause the result of \code{kRp.POS.tags(lang, tags="sentc", list.tags=TRUE)} to be used.
#' @param nonword.class A character vector with word classes which should be ignored for readability analysis. The default value
#'    \code{"nonpunct"} has special meaning and will cause the result of \code{kRp.POS.tags(lang, c("punct","sentc"), list.classes=TRUE)}
#'    to be used. Will only be of consequence if \code{hyphen} is not set!
#' @param nonword.tag A character vector with POS tags which should be ignored for readability analysis. Will only be
#'    of consequence if \code{hyphen} is not set!
#' @param quiet Logical. If \code{FALSE}, short status messages will be shown.
#'    \code{TRUE} will also suppress all potential warnings regarding the validation status of measures.
#' @param ... Additional options for the specified \code{tagger} function
#' @return An object of class \code{\link[koRpus]{kRp.readability-class}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords readability
#' @references
#'    Anderson, J. (1981). Analysing the readability of english and non-english texts in the classroom with Lix. In
#'      \emph{Annual Meeting of the Australian Reading Association}, Darwin, Australia.
#'
#'    Anderson, J. (1983). Lix and Rix: Variations on a little-known readability index. \emph{Journal of Reading}, 26(6), 490--496.
#'
#'    Bamberger, R. & Vanecek, E. (1984). \emph{Lesen--Verstehen--Lernen--Schreiben}. Wien: Jugend und Volk.
#'
#'    Coleman, M. & Liau, T.L. (1975). A computer readability formula designed for machine scoring, \emph{Journal of Applied Psychology}, 60(2), 283--284.
#'
#'    Dickes, P. & Steiwer, L. (1977). Ausarbeitung von Lesbarkeitsformeln f\"ur die deutsche Sprache.
#'      \emph{Zeitschrift f\"ur Entwicklungspsychologie und P\"adagogische Psychologie}, 9(1), 20--28.
#'
#'    DuBay, W.H. (2004). \emph{The Principles of Readability}. Costa Mesa: Impact Information.
#'      WWW: \url{http://www.impact-information.com/impactinfo/readability02.pdf}; 22.03.2011.
#'
#'    Farr, J.N., Jenkins, J.J. & Paterson, D.G. (1951). Simplification of Flesch Reading Ease formula. \emph{Journal of Applied Psychology}, 35(5), 333--337.
#'
#'    Flesch, R. (1948). A new readability yardstick. \emph{Journal of Applied Psychology}, 32(3), 221--233.
#'
#'    Fucks, W. (1955). Der Unterschied des Prosastils von Dichtern und anderen Schriftstellern. \emph{Sprachforum}, 1, 233--244.
#'    
#'    Grzybek, P. (2010). Text difficulty and the Arens-Altmann law. In Peter Grzybek, Emmerich Kelih, \enc{Ján}{Jan} \enc{Mačutek}{Macutek} (Eds.),
#'      \emph{Text and Language. Structures -- Functions -- Interrelations. Quantitative Perspectives}. Wien: Praesens, 57--70.
#'
#'    Harris, A.J. & Jacobson, M.D. (1974). Revised Harris-Jacobson readability formulas. In \emph{18th Annual Meeting of the College Reading Association}, Bethesda.
#'
#'    Klare, G.R. (1975). Assessing readability. \emph{Reading Research Quarterly}, 10(1), 62--102.
#'
#'    McLaughlin, G.H. (1969). SMOG grading -- A new readability formula. \emph{Journal of Reading}, 12(8), 639--646.
#'
#'    Powers, R.D, Sumner, W.A, & Kearl, B.E. (1958). A recalculation of four adult readability formulas, \emph{Journal of Educational Psychology}, 49(2), 99--105.
#'
#'    Smith, E.A. & Senter, R.J. (1967). \emph{Automated readability index}. AMRL-TR-66-22. Wright-Paterson AFB, Ohio: Aerospace Medical Division.
#'
#'    Spache, G. (1953). A new readability formula for primary-grade reading materials. \emph{The Elementary School Journal}, 53, 410--413.
#'
#'    Tr\"ankle, U. & Bailer, H. (1984). Kreuzvalidierung und Neuberechnung von Lesbarkeitsformeln f\"ur die deutsche Sprache.
#'      \emph{Zeitschrift f\"ur Entwicklungspsychologie und P\"adagogische Psychologie}, 16(3), 231--244.
#'
#'    Wheeler, L.R. & Smith, E.H. (1954). A practical readability formula for the classroom teacher in the primary grades. \emph{Elementary English},
#'      31, 397--399.
#'
#'    [1] \url{http://strainindex.wordpress.com/2007/09/25/hello-world/}
#' @import methods
#' @rdname readability-methods
#' @export
#' @examples
#' \dontrun{
#' readability(tagged.text)
#' }

setGeneric("readability", function(txt.file, ...) standardGeneric("readability"))

##################################################################
## if this signature changes, check kRp.rdb.formulae() as well! ##
##################################################################

#' @export
#' @include 00_class_01_kRp.tagged.R
#' @include 00_class_03_kRp.txt.freq.R
#' @include 00_class_04_kRp.txt.trans.R
#' @include 00_class_05_kRp.analysis.R
#' @include koRpus-internal.R
#' @aliases readability,kRp.taggedText-method
#' @rdname readability-methods
setMethod("readability", signature(txt.file="kRp.taggedText"), function(txt.file, hyphen=NULL,
      index=c("ARI", "Bormuth", "Coleman", "Coleman.Liau",
        "Dale.Chall", "Danielson.Bryan", "Dickes.Steiwer","DRP",
        "ELF", "Farr.Jenkins.Paterson", "Flesch", "Flesch.Kincaid",
        "FOG", "FORCAST", "Fucks", "Harris.Jacobson", "Linsear.Write", "LIX", "nWS",
        "RIX", "SMOG", "Spache", "Strain", "Traenkle.Bailer", "TRI", "Tuldava",
        "Wheeler.Smith"),
      parameters=list(),
      word.lists=list(Bormuth=NULL, Dale.Chall=NULL, Harris.Jacobson=NULL, Spache=NULL),
      fileEncoding="UTF-8",
      tagger="kRp.env",
      force.lang=NULL,
      sentc.tag="sentc",
      nonword.class="nonpunct",
      nonword.tag=c(),
      quiet=FALSE, ...){

    # all the actual calculations have been moved to an internal function, to be able to re-use
    # the formulas for calculation without the actual text, but only its key values, in other functions
    all.results <- kRp.rdb.formulae(
        txt.file=txt.file,
        hyphen=hyphen,
        index=index,
        parameters=parameters,
        word.lists=word.lists,
        fileEncoding=fileEncoding,
        tagger=tagger,
        force.lang=force.lang,
        sentc.tag=sentc.tag,
        nonword.class=nonword.class,
        nonword.tag=nonword.tag,
        quiet=quiet, 
        analyze.text=TRUE, ...)

    return(all.results)
  }
)

#' @export
#' @aliases readability,character-method
#' @rdname readability-methods
setMethod("readability", signature(txt.file="character"), function(txt.file, hyphen=NULL,
      index=c("ARI", "Bormuth", "Coleman", "Coleman.Liau",
        "Dale.Chall", "Danielson.Bryan", "Dickes.Steiwer","DRP",
        "ELF", "Farr.Jenkins.Paterson", "Flesch", "Flesch.Kincaid",
        "FOG", "FORCAST", "Fucks", "Harris.Jacobson", "Linsear.Write", "LIX", "nWS",
        "RIX", "SMOG", "Spache", "Strain", "Traenkle.Bailer", "TRI", "Tuldava",
        "Wheeler.Smith"),
      parameters=list(),
      word.lists=list(Bormuth=NULL, Dale.Chall=NULL, Harris.Jacobson=NULL, Spache=NULL),
      fileEncoding="UTF-8",
      tagger="kRp.env",
      force.lang=NULL,
      sentc.tag="sentc",
      nonword.class="nonpunct",
      nonword.tag=c(),
      quiet=FALSE, ...){

    # all the actual calculations have been moved to an internal function, to be able to re-use
    # the formulas for calculation without the actual text, but only its key values, in other functions
    all.results <- kRp.rdb.formulae(
        txt.file=txt.file,
        hyphen=hyphen,
        index=index,
        parameters=parameters,
        word.lists=word.lists,
        fileEncoding=fileEncoding,
        tagger=tagger,
        force.lang=force.lang,
        sentc.tag=sentc.tag,
        nonword.class=nonword.class,
        nonword.tag=nonword.tag,
        quiet=quiet, 
        analyze.text=TRUE, ...)

    return(all.results)
  }
)
