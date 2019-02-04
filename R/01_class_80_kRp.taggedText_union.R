# define class union to make life easier
#' @include 01_class_01_kRp.tagged.R
#' @include 01_class_02_kRp.TTR.R
#' @include 01_class_03_kRp.txt.freq.R
#' @include 01_class_04_kRp.txt.trans.R
#' @include 01_class_05_kRp.analysis.R
#' @include 01_class_06_kRp.corp.freq.R
#' @include 01_class_09_kRp.lang.R
#' @include 01_class_10_kRp.readability.R
setClassUnion("kRp.taggedText", members=c("kRp.tagged", "kRp.analysis", "kRp.txt.freq", "kRp.txt.trans"))
