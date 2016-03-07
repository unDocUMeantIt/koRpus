#' An R Package for Text Analysis.
#'
#' \tabular{ll}{
#' Package: \tab koRpus\cr
#' Type: \tab Package\cr
#' Version: \tab 0.06-4\cr
#' Date: \tab 2016-03-07\cr
#' Depends: \tab R (>= 2.10.0),methods\cr
#' Enhances: \tab rkward\cr
#' Encoding: \tab UTF-8\cr
#' License: \tab GPL (>= 3)\cr
#' LazyLoad: \tab yes\cr
#' URL: \tab http://reaktanz.de/?c=hacking&s=koRpus\cr
#' }
#'
#' A set of tools to analyze texts. Includes, amongst others, functions for automatic language detection,
#' hyphenation, several indices of lexical diversity (e.g., type token ratio, HD-D/vocd-D, MTLD)
#' and readability (e.g., Flesch, SMOG, LIX, Dale-Chall). Basic import functions for language corpora
#' are also provided, to enable frequency analyses (supports Celex and Leipzig Corpora Collection file formats)
#' and measures like tf-idf. Support for additional languages can be added on-the-fly or by plugin packages.
#' Note: For full functionality a local installation of TreeTagger is recommended. 'koRpus' also includes a plugin
#' for the R GUI and IDE RKWard, providing graphical dialogs for its basic features. The respective R package
#' 'rkward' cannot be installed directly from a repository, as it is a part of RKWard. To make full use of this
#' feature, please install RKWard from https://rkward.kde.org (plugins are detected automatically).
#' Due to some restrictions on CRAN, the full package sources are only available from the project homepage.
#' To ask for help, report bugs, suggest feature improvements, or discuss the global development of the package,
#' please subscribe to the koRpus-dev mailing list (https://ml06.ispgateway.de/mailman/listinfo/korpus-dev_r.reaktanz.de).
#'
#' @aliases koRpus-package
#' @name koRpus-package
#' @docType package
#' @title The koRpus Package
#' @author m.eik michalke, with contributions from Earl Brown, Alberto Mirisola, Alexandre Brulet, Laura Hauser
#' @keywords package
NULL
