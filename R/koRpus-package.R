#' An R Package for Text Analysis.
#'
#' \tabular{ll}{
#' Package: \tab koRpus\cr
#' Type: \tab Package\cr
#' Version: \tab 0.11-4\cr
#' Date: \tab 2018-05-10\cr
#' Depends: \tab R (>= 3.0.0),sylly (>= 0.1-4)\cr
#' Enhances: \tab rkward\cr
#' Encoding: \tab UTF-8\cr
#' License: \tab GPL (>= 3)\cr
#' LazyLoad: \tab yes\cr
#' URL: \tab https://reaktanz.de/?c=hacking&s=koRpus\cr
#' }
#'
#' A set of tools to analyze texts. Includes, amongst others, functions for automatic
#' language detection, hyphenation, several indices of lexical diversity (e.g., type token ratio,
#' HD-D/vocd-D, MTLD) and readability (e.g., Flesch, SMOG, LIX, Dale-Chall). Basic import
#' functions for language corpora are also provided, to enable frequency analyses (supports Celex
#' and Leipzig Corpora Collection file formats) and measures like tf-idf. Note: For full
#' functionality a local installation of TreeTagger is recommended. It is also recommended to
#' not load this package directly, but by loading one of the available language support
#' packages from the 'l10n' repository <https://undocumeantit.github.io/repos/l10n>. 'koRpus' also
#' includes a plugin for the R GUI and IDE RKWard, providing graphical dialogs for its basic
#' features. The respective R package 'rkward' cannot be installed directly from a repository,
#' as it is a part of RKWard. To make full use of this feature, please install RKWard from
#' <https://rkward.kde.org> (plugins are detected automatically). Due to some restrictions on
#' CRAN, the full package sources are only available from the project homepage. To ask for
#' help, report bugs, request features, or discuss the development of the package, please
#' subscribe to the koRpus-dev mailing list (<http://korpusml.reaktanz.de>).
#'
#' @aliases koRpus-package
#' @name koRpus-package
#' @docType package
#' @title The koRpus Package
#' @author Meik Michalke, with contributions from Earl Brown, Alberto Mirisola, Alexandre Brulet, Laura Hauser
#' @keywords package
NULL
