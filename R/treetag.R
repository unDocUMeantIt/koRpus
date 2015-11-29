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


#' A function to call TreeTagger
#'
#' This function calls a local installation of TreeTagger[1] to tokenize and POS tag the given text.
#'
#' Note that the value of \code{lang} must match a valid language supported by \code{\link[koRpus:kRp.POS.tags]{kRp.POS.tags}}.
#' It will also get stored in the resulting object and might be used by other functions at a later point.
#' E.g., \code{treetag} is being called by \code{\link[koRpus:freq.analysis]{freq.analysis}}, which
#' will by default query this language definition, unless explicitly told otherwise. The rationale behind this
#' is to comfortably make it possible to have tokenized and POS tagged objects of various languages around
#' in your workspace, and not worry about that too much.
#'
#' @param file Either a connection or a character vector, valid path to a file, containing the text to be analyzed.
#'    If \code{file} is a connection, its contents will be written to a temporary file, since TreeTagger can't read from
#'    R connection objects.
#' @param treetagger A character vector giving the TreeTagger script to be called. If set to \code{"kRp.env"} this is got from \code{\link[koRpus:get.kRp.env]{get.kRp.env}}.
#'    Only if set to \code{"manual"}, it is assumend not to be a wrapper script that can work the given text file, but that you would like
#'    to manually tweak options for tokenizing and POS tagging yourself. In that case, you need to provide a full set of options with the \code{TT.options}
#'    parameter.
#' @param rm.sgml Logical, whether SGML tags should be ignored and removed from output
#' @param lang A character string naming the language of the analyzed corpus. See \code{\link[koRpus:kRp.POS.tags]{kRp.POS.tags}} for all supported languages.
#'    If set to \code{"kRp.env"} this is got from \code{\link[koRpus:get.kRp.env]{get.kRp.env}}.
#' @param apply.sentc.end Logical, whethter the tokens defined in \code{sentc.end} should be searched and set to a sentence ending tag.
#' @param sentc.end A character vector with tokens indicating a sentence ending. This adds to TreeTaggers results, it doesn't really replace them.
#' @param encoding A character string defining the character encoding of the input file, like  \code{"Latin1"} or \code{"UTF-8"}. If \code{NULL},
#'    the encoding will either be taken from a preset (if defined in \code{TT.options}), or fall back to \code{""}. Hence you can overwrite the preset encoding with this parameter.
#' @param TT.options A list of options to configure how TreeTagger is called. You have two basic choices: Either you choose one of the pre-defined presets
#'    or you give a full set of valid options:
#'    \itemize{
#'      \item {\code{path}} {Mandatory: The absolute path to the TreeTagger root directory. That is where its subfolders \code{bin}, \code{cmd} and \code{lib} are located.}
#'      \item {\code{preset}} {Optional: If you choose one of the pre-defined presets here:
#'      \itemize{
#'        \item {\code{"de-utf8"}} {German, UTF-8}
#'        \item {\code{"de"}} {German}
#'        \item {\code{"en"}} {English}
#'        \item {\code{"es-utf8"}} {Spanish, UTF-8}
#'        \item {\code{"es"}} {Spanish}
#'        \item {\code{"fr-utf8"}} {French, UTF-8}
#'        \item {\code{"fr"}} {French}
#'        \item {\code{"it-utf8"}} {Italian, UTF-8}
#'        \item {\code{"it"}} {Italian}
#'        \item {\code{"ru"}} {Russian, UTF-8}
#'      }
#'        you can omit all the following elements, because they will be filled with defaults. Of course this only makes sense if you have a
#'        working default installation.}
#'      \item {\code{tokenizer}} {Mandatory: A character string, naming the tokenizer to be called. Interpreted relative to \code{path/cmd/}.}
#'      \item {\code{tknz.opts}} {Optional: A character string with the options to hand over to the tokenizer. You don't need to specify "-a"
#'        if \code{abbrev} is given. If \code{TT.tknz=FALSE}, you can pass configurational options to \code{\link[koRpus:tokenize]{tokenize}}
#'        by provinding them as a named list (instead of a character string) here.}
#'      \item {\code{tagger}} {Mandatory: A character string, naming the tagger-command to be called. Interpreted relative to \code{path/bin/}.}
#'      \item {\code{abbrev}} {Optional: A character string, naming the abbreviation list to be used. Interpreted relative to \code{path/lib/}.}
#'      \item {\code{params}} {Mandatory: A character string, naming the parameter file to be used. Interpreted relative to \code{path/lib/}.}
#'      \item {\code{lexicon}} {Optional: A character string, naming the lexicon file to be used. Interpreted relative to \code{path/lib/}.}
#'      \item {\code{lookup}} {Optional: A character string, naming the lexicon lookup command. Interpreted relative to \code{path/cmd/}.}
#'      \item {\code{filter}} {Optional: A character string, naming the output filter to be used. Interpreted relative to \code{path/cmd/}.}
#'      \item {\code{no.unknown}} {Optional: Logical, can be used to toggle the \code{"-no-unknown"} option of TreeTagger (defaults of \code{FALSE}.}
#'    }
#' You can also set these options globally using \code{\link[koRpus:set.kRp.env]{set.kRp.env}},
#' and then force \code{treetag} to use them by setting \code{TT.options="kRp.env"} here. Note: 
#' If you use the \code{treetagger} setting from kRp.env and it's set to \code{TT.cmd="manual"},
#' \code{treetag} will treat \code{TT.options=NULL} like \code{TT.options="kRp.env"} 
#' automatically.
#' @param debug Logical. Especially in cases where the presets wouldn't work as expected, this switch can be used to examine the values \code{treetag}
#'    is assuming.
#' @param TT.tknz Logical, if \code{FALSE} TreeTagger's tokenzier script will be replaced by \code{koRpus}' function \code{\link[koRpus:tokenize]{tokenize}}.
#'    To accomplish this, its results will be written to a temporal file which is automatically deleted afterwards (if \code{debug=FALSE}). Note that
#'    this option only has an effect if \code{treetagger="manual"}.
#' @param format Either "file" or "obj", depending on whether you want to scan files or analyze the text in a given object, like
#'    a character vector. If the latter, it will be written to a temporary file (see \code{file}).
#' @param stopwords A character vector to be used for stopword detection. Comparison is done in lower case. You can also simply set 
#'    \code{stopwords=tm::stopwords("en")} to use the english stopwords provided by the \code{tm} package.
#' @param stemmer A function or method to perform stemming. For instance, you can set \code{SnowballC::wordStem} if you have
#'    the \code{SnowballC} package installed. As of now, you cannot provide further arguments to this function.
#' @return An object of class \code{\link[koRpus]{kRp.tagged-class}}. If \code{debug=TRUE}, prints internal variable settings and attempts to return the
#'    original output if the TreeTagger system call in a matrix.
#' @author m.eik michalke \email{meik.michalke@@hhu.de}, support for various laguages was contributed by Earl Brown (Spanish), Alberto Mirisola (Italian) and
#'    Alexandre Brulet (French).
#' @keywords misc
#' @seealso \code{\link[koRpus:freq.analysis]{freq.analysis}}, \code{\link[koRpus:get.kRp.env]{get.kRp.env}},
#' \code{\link[koRpus]{kRp.tagged-class}}
#' @references
#' Schmid, H. (1994). Probabilistic part-of-speec tagging using decision trees. In
#'    \emph{International Conference on New Methods in Language Processing}, Manchester, UK, 44--49.
#'
#' [1] \url{http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/}
#' @export
#' @examples
#' \dontrun{
#' # first way to invoke POS tagging, using a built-in preset:
#' tagged.results <- treetag("~/my.data/speech.txt", treetagger="manual", lang="en",
#'    TT.options=list(path="~/bin/treetagger", preset="en"))
#' # second way, use one of the batch scripts that come with TreeTagger:
#' tagged.results <- treetag("~/my.data/speech.txt",
#'    treetagger="~/bin/treetagger/cmd/tree-tagger-english", lang="en")
#' # third option, set the above batch script in an environment object first:
#' set.kRp.env(TT.cmd="~/bin/treetagger/cmd/tree-tagger-english", lang="en")
#' tagged.results <- treetag("~/my.data/speech.txt")
#'
#' # after tagging, use the resulting object with other functions in this package:
#' readability(tagged.results)
#' lex.div(tagged.results)
#' 
#' ## enabling stopword detection and stemming
#' # if you also installed the packages tm and SnowballC,
#' # you can use some of their features with koRpus:
#' set.kRp.env(TT.cmd="manual", lang="en", TT.options=list(path="~/bin/treetagger",
#'    preset="en"))
#' tagged.results <- treetag("~/my.data/speech.txt",
#'    stopwords=tm::stopwords("en"),
#'    stemmer=SnowballC::wordStem)
#'
#' # removing all stopwords now is simple:
#' tagged.noStopWords <- kRp.filter.wclass(tagged.results, "stopword")
#' }

treetag <- function(file, treetagger="kRp.env", rm.sgml=TRUE, lang="kRp.env",
  apply.sentc.end=TRUE, sentc.end=c(".","!","?",";",":"), encoding=NULL, TT.options=NULL, debug=FALSE, TT.tknz=TRUE,
  format="file", stopwords=NULL, stemmer=NULL){

  # TreeTagger uses slightly different presets on windows and unix machines,
  # so we'll need to check the OS first
  if(identical(base::.Platform[["OS.type"]], "windows")){
    unix.OS <- FALSE
  } else {
    unix.OS <- TRUE
  }

  # check on TT options
  if(identical(treetagger, "kRp.env")){
    treetagger <- get.kRp.env(TT.cmd=TRUE)
    if(is.null(TT.options) & identical(treetagger, "manual")){
      TT.options <- get.kRp.env(TT.options=TRUE)
    } else {}
  } else {}
  if(identical(TT.options, "kRp.env")){
    TT.options <- get.kRp.env(TT.options=TRUE)
  } else if(!is.null(TT.options) && !is.list(TT.options)){
    warning("You provided \"TT.options\", but not as a list!")
  } else {}

  if(identical(lang, "kRp.env")){
    lang <- get.kRp.env(lang=TRUE)
  } else {}

  if(identical(treetagger, "tokenize")){
    stop(simpleError("Sorry, you can't use treetag() and tokenize() at the same time!"))
  } else {}

  # TreeTagger won't be able to use a connection object, so to make these usable,
  # we have to write its content to a temporary file first
  if(inherits(file, "connection") | identical(format, "obj")){
    conn.tempfile <- tempfile(pattern="tempTextFromObject", fileext=".txt")
    if(!isTRUE(debug)){
      on.exit(unlink(conn.tempfile), add=TRUE)
    } else {}
    if(inherits(file, "connection")){
      writeLines(readLines(file, encoding=ifelse(is.null(encoding), "", encoding)), con=conn.tempfile)
    } else {
      writeLines(file, con=conn.tempfile)
    }
    takeAsFile <- conn.tempfile
  } else {
    # does the text file exist?
    takeAsFile <- normalizePath(file)
  }
  check.file(takeAsFile, mode="exist")

  # TODO: move TT.options checks to internal function to call it here
  manual.config <- identical(treetagger, "manual")
  checkedOptions <- checkTTOptions(TT.options=TT.options, manual.config=manual.config, TT.tknz=TT.tknz)

  if(isTRUE(manual.config)){
    # specify basic paths
    TT.path <- checkedOptions[["TT.path"]]
    TT.bin <- checkedOptions[["TT.bin"]]
    TT.cmd <- checkedOptions[["TT.cmd"]]
    TT.lib <- checkedOptions[["TT.lib"]]

    # basic options
    TT.opts <- checkedOptions[["TT.opts"]]

    have.preset <- FALSE
    if("preset" %in% names(TT.options)){
      ## minimum requirements for new presets:
      #  TT.tokenizer    <- file.path(TT.cmd, "...")
      #  TT.tagger      <- file.path(TT.bin, "...")
      #  TT.abbrev      <- c()
      #  TT.params      <- file.path(TT.lib, "...")
      #  TT.tknz.opts.def  <- c()
      #  TT.tknz.opts    <- c()
      #  TT.lookup.command  <- c()
      #  TT.filter.command  <- c()
      preset.definition <- checkedOptions[["preset"]]
      # check for matching language definitions
      matching.lang(lang=lang, lang.preset=preset.definition[["lang"]])
      preset.list <- preset.definition[["preset"]](TT.cmd=TT.cmd, TT.bin=TT.bin, TT.lib=TT.lib, unix.OS=unix.OS)

      TT.tokenizer    <- preset.list[["TT.tokenizer"]]
      TT.tagger      <- preset.list[["TT.tagger"]]
      TT.abbrev      <- preset.list[["TT.abbrev"]]
      TT.params      <- preset.list[["TT.params"]]
      TT.lexicon      <- preset.list[["TT.lexicon"]]
      TT.lookup      <- preset.list[["TT.lookup"]]
      TT.filter      <- preset.list[["TT.filter"]]

      TT.tknz.opts.def  <- preset.list[["TT.tknz.opts.def"]]
      TT.tknz.opts    <- preset.list[["TT.tknz.opts"]]
      TT.lookup.command  <- preset.list[["TT.lookup.command"]]
      TT.filter.command  <- preset.list[["TT.filter.command"]]
    } else {}

    if("tokenizer" %in% names(TT.options)){
      TT.tokenizer    <- file.path(TT.cmd, TT.options[["tokenizer"]])
    } else {}
    # check if path works
    check.file(TT.tokenizer, mode="exist")
    if("tagger" %in% names(TT.options)){
      TT.tagger      <- file.path(TT.bin, TT.options[["tagger"]])
    } else {}
    # check if path works
    check.file(TT.tagger, mode="exist")
    if("params" %in% names(TT.options)){
      TT.params      <- file.path(TT.lib, TT.options[["params"]])
    } else {}
    # check if path works
    check.file(TT.params, mode="exist")

    # check the input encoding
    input.enc <- ifelse(
      is.null(encoding),
        ifelse(
          identical(treetagger, "manual") & ("preset" %in% names(TT.options)),
            preset.definition[["encoding"]],
            ""),
        encoding)

    if("tknz.opts" %in% names(TT.options)){
      TT.tknz.opts    <- TT.options[["tknz.opts"]]
    } else {
      if(!isTRUE(have.preset)){
        TT.tknz.opts  <- c()
      } else {
        TT.tknz.opts  <- TT.tknz.opts.def
      }
    }

    if("abbrev" %in% names(TT.options)){
      TT.abbrev      <- file.path(TT.lib, TT.options[["abbrev"]])
      check.file(TT.abbrev, mode="exist")
      TT.tknz.opts    <- paste(TT.tknz.opts, "-a", TT.abbrev)
    } else {
      if(isTRUE(have.preset) & !identical(TT.abbrev, c())){
        TT.tknz.opts    <- paste(TT.tknz.opts, "-a", TT.abbrev)
        check.file(TT.abbrev, mode="exist")
      } else {
        TT.abbrev <- eval(formals(tokenize)[["abbrev"]])
      }
    }

    ## probably replacing the tokenizer
    if(!isTRUE(TT.tknz)){
      if(!is.list(TT.tknz.opts)){
        TT.tknz.opts <- list()
      } else {}
      given.tknz.options <- names(TT.tknz.opts)
      tokenize.options <- c("split", "ign.comp", "heuristics", "heur.fix", "abbrev",
        "sentc.end", "detect", "clean.raw", "perl")
      for (this.opt in tokenize.options){
        if(!this.opt %in% given.tknz.options) {
          TT.tknz.opts[[this.opt]] <- eval(formals(tokenize)[[this.opt]])
        } else {}
      }
      if(!"abbrev" %in% given.tknz.options) {
        TT.tknz.opts[["abbrev"]] <- TT.abbrev
      } else {}

      # set this just for the debug printout
      TT.tokenizer  <- "koRpus::tokenize()"
      # call tokenize() and write results to tempfile
      tknz.tempfile <- tempfile(pattern="tokenize", fileext=".txt")
      tknz.results <- tokenize(
        takeAsFile,
        format="file",
        fileEncoding=input.enc,
        split=TT.tknz.opts[["split"]],
        ign.comp=TT.tknz.opts[["ign.comp"]],
        heuristics=TT.tknz.opts[["heuristics"]],
        heur.fix=TT.tknz.opts[["heur.fix"]],
        abbrev=TT.tknz.opts[["abbrev"]],
        tag=FALSE,
        lang=lang,
        sentc.end=TT.tknz.opts[["sentc.end"]],
        detect=TT.tknz.opts[["detect"]],
        clean.raw=TT.tknz.opts[["clean.raw"]],
        perl=TT.tknz.opts[["perl"]]
      )
      # TreeTagger can produce mixed encoded results if fed with UTF-8 in Latin1 mode
      tknz.results <- iconv(tknz.results, from="UTF-8", to=input.enc)
      message(paste0("Assuming '", input.enc, "' as encoding for the input file. If the results turn out to be erroneous, check the file for invalid characters, e.g. em.dashes or fancy quotes, and/or consider setting 'encoding' manually."))
      cat(paste(tknz.results, collapse="\n"), file=tknz.tempfile)
      if(!isTRUE(debug)){
        on.exit(unlink(tknz.tempfile), add=TRUE)
      } else {}
    } else {}

    if("lexicon" %in% names(TT.options)){
      if(!isTRUE(have.preset) & !"lookup" %in% names(TT.options)){
        TT.lookup.command  <- c()
        warning("Manual TreeTagger configuration: Defined a \"lexicon\" without a \"lookup\" command, hence omitted!")
      } else {
        if(!isTRUE(have.preset)){
          TT.lookup    <- file.path(TT.cmd, TT.options[["lookup"]])
        } else {}
        TT.lexicon      <- file.path(TT.lib, TT.options[["lexicon"]])
        check.file(TT.lookup, mode="exist")
        check.file(TT.lexicon, mode="exist")
        TT.lookup.command  <- paste(TT.lookup, TT.lexicon, "|")
      }
    } else {
      if(!isTRUE(have.preset)){
        TT.lookup.command  <- c()
      } else {
        if(!identical(TT.lookup.command, c())){
          check.file(TT.lookup, mode="exist")
          check.file(TT.lexicon, mode="exist")
        } else {}
      }
    }

    if("filter" %in% names(TT.options)){
      TT.filter      <- file.path(TT.cmd, TT.options[["filter"]])
      TT.filter.command  <- paste("|", TT.filter)
    } else {
      if(!isTRUE(have.preset)){
        TT.filter.command  <- c()
      } else {}
    }

    # create system call for unix and windows
    if(isTRUE(unix.OS)){
      if(isTRUE(TT.tknz)){
        sys.tt.call <- paste(TT.tokenizer, TT.tknz.opts, paste0("\"", takeAsFile, "\""), "|",
          TT.lookup.command, TT.tagger, TT.opts, TT.params, TT.filter.command)
      } else {
        sys.tt.call <- paste("cat ", tknz.tempfile, "|",
          TT.lookup.command, TT.tagger, TT.opts, TT.params, TT.filter.command)
      }
    } else {
      if(isTRUE(TT.tknz)){
        sys.tt.call <- paste("perl ", TT.tokenizer, TT.tknz.opts, paste0("\"", takeAsFile, "\""), "|",
          TT.lookup.command, TT.tagger, TT.params, TT.opts, TT.filter.command)
      } else {
        sys.tt.call <- paste("type ", tknz.tempfile, "|",
          TT.lookup.command, TT.tagger, TT.params, TT.opts, TT.filter.command)
      }
    }

  } else {
    input.enc <- ifelse(
      is.null(encoding),
        "",
        encoding)

    check.file(treetagger, mode="exec")

    sys.tt.call <- paste(treetagger, takeAsFile)
  }

  ## uncomment for debugging
  if(isTRUE(debug)){
    if(isTRUE(manual.config)){
      cat(paste("
        TT.tokenizer: ",TT.tokenizer,
        ifelse(isTRUE(TT.tknz),
          paste0("\n\t\t\t\tTT.tknz.opts: ",TT.tknz.opts),
          paste0("\n\t\t\t\ttempfile: ",tknz.tempfile)),"
        file: ",takeAsFile,"
        TT.lookup.command: ",TT.lookup.command,"
        TT.tagger: ",TT.tagger,"
        TT.opts: ",TT.opts,"
        TT.params: ",TT.params,"
        TT.filter.command: ",TT.filter.command,"\n
        sys.tt.call: ",sys.tt.call,"\n"))
    } else {
      cat(paste("
        file: ",takeAsFile,"
        sys.tt.call: ",sys.tt.call,"\n"))
    }
  } else {}

  ## do the system call
  if(isTRUE(unix.OS)){
    tagged.text <- system(sys.tt.call, ignore.stderr=TRUE, intern=TRUE)
  } else {
    tagged.text <- shell(sys.tt.call, translate=TRUE, ignore.stderr=TRUE, intern=TRUE)
  }
  tagged.text <- enc2utf8(tagged.text)

  ## workaround
  # in seldom cases TreeTagger seems to return duplicate tab stops
  # we'll try to correct for that here
  tagged.text <- gsub("\t\t", "\t", tagged.text)

  if(isTRUE(rm.sgml)){
    tagged.text <- tagged.text[grep("^[^<]", tagged.text)]
  } else {}

  tagged.mtrx <- matrix(unlist(strsplit(tagged.text, "\t")), ncol=3, byrow=TRUE, dimnames=list(c(),c("token","tag","lemma")))

  # add sentence endings as defined
  if(isTRUE(apply.sentc.end)){
    sntc.end.tag <- kRp.POS.tags(lang, tags="sentc", list.tags=TRUE)[[1]]
    matched.sentc.tokens <- tagged.mtrx[, "token"] %in% sentc.end
    tagged.mtrx[matched.sentc.tokens, "tag"] <- sntc.end.tag
  } else {}
  ## for debugging:
  if(isTRUE(debug)){
    return(tagged.mtrx)
  } else {}

  # add word classes, comments and numer of letters ("wclass", "desc", "lttr")
  tagged.mtrx <- treetag.com(tagged.mtrx, lang=lang)

  # probably apply stopword detection and stemming
  tagged.mtrx <- stopAndStem(tagged.mtrx, stopwords=stopwords, stemmer=stemmer, lowercase=TRUE)

  results <- new("kRp.tagged", lang=lang, TT.res=tagged.mtrx)
  ## descriptive statistics
  if(is.null(encoding)){
    encoding <- ""
  } else {}
  txt.vector <- readLines(takeAsFile, encoding=encoding)
  # force text into UTF-8 format
  txt.vector <- enc2utf8(txt.vector)
  results@desc <- basic.tagged.descriptives(results, lang=lang, txt.vector=txt.vector)

  return(results)
}
