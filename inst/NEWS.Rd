\name{NEWS}
\title{News for Package 'koRpus'}
\encoding{UTF-8}
\section{Changes in koRpus version 0.10-2 (2017-04-04)}{
  \subsection{fixed}{
    \itemize{
      \item leftover typo in lang.support-en.R referencing "utf8-tokenize.pl" instead
        of "utf8-tokenize.perl" in the windows preset and a call to grep that is
        not present in Treetagger's *.bat file
      \item \code{readability()}: fixed a minor issue with the internal handling of wrongly
        tagged dashes in the FOG formula (shouldn't have any effect on results)
    }
  }
  \subsection{changed}{
    \itemize{
      \item if no encoding is provided and \code{treetag()} needs to write temporary files,
        output file encoding is now forced into UTF-8
      \item \code{hyphen()}: caching now uses an environment instead of a data.frame. this
        means that old cache files will need to be changed as well. \code{hyphen()} will
        try to convert them on the fly, but if this fails you should remove the old
        files
      \item \code{hyphen()}: cached results are now looked up much more efficient, speeding
        up the process drastically (about 100 times faster in my benchmarks!)
      \item \code{hyphen()}: hyphenation patterns are now internally converted to
        environments which speeds up uncached runs (or first runs with cache) noticeably
      \item \code{readability()}: default parameters are now always fetched by the internal
        function \code{default.params()}, individually for each index
      \item source code: moved all wrapper functions for \code{readability()} and \code{lex.div()}
        from individual source files to one wrapper file, respectively. the source
        tree became a bit overcrowded over the years
    }
  }
  \subsection{added}{
    \itemize{
      \item new options redability(index=\code{"validation"}) and
        lex.div(measure=\code{"validation"}) show current the status of validation. this info was previously only
        available as comments in the source code and is now directly available.
    }
  }
  \subsection{removed}{
    \itemize{
      \item \code{WSFT()}: deprecated wrapper, was replaced by \code{nWS()} in 2012
    }
  }
}
\section{Changes in koRpus version 0.10-1 (2017-03-01)}{
  \subsection{fixed}{
    \itemize{
      \item windows users could run into an error of an undefined object
        (TT.call.file) when using \code{treetag()}
    }
  }
  \subsection{changed}{
    \itemize{
      \item CRAN doesn't accept leading zeroes in version numbers any longer and
        asked me to change 0.07 into 0.7. i'd rather play this safe, so i'm jumping
        right to 0.10 to keep the versioning consistent fo all users. the reason for
        this policy change was not explained to me, could be anything from "we
        think it looks ugly" to "it breaks our build systems".
      \item allowing \code{treetag()} to run even when a defined lexicon file is not found.
        this previously resulted in an error and now causes only a warning message.
    }
  }
}
\section{Changes in koRpus version 0.07-2 (2016-12-21)}{
  \subsection{fixed}{
    \itemize{
      \item the show method for Flesch Brouwer was not working properly
      \item if a cache file for hyphen is set but not existing, it will be created
        automatically
      \item the manual page for the wrapper function \code{ELF()} attributed the index to
        Farr, when it was in fact Fang (as correctly said in ?readability);
        vigilantly spotted by Mario Martinez
      \item calling \code{lex.div()} on untagged character vectors didn't really work yet
      \item \code{guess.lang()} had problems with newer UDHR files which included comments
        in the index.xml file
      \item shiny app: was omitting the row names of tables in newer versions of shiny
      \item \code{treetag()} appended the abbreviation list two times in english preset
      \item TT.options checks in \code{treetag()} do no longer ask for mandatory options if
        TT.cmd is not \code{"manual"}
    }
  }
  \subsection{changed}{
    \itemize{
      \item updated shiny app: disabling FOG by default (faster), adding Brouwer and
        MTLDMA.steps options, adding dutch and portuguese by default, disabled
        language selection in language guessing tab
      \item shiny app: using \code{fluidPage()} now
      \item shiny app: set tables to use bootstrap striped layout
      \item reaktanz.de supports HTTPS now, updated references
    }
  }
  \subsection{added}{
    \itemize{
      \item new \code{summary()} method for kRp.hyph objects
      \item new \code{show()} methods for kRp.hyph and kRp.taggedText objects
      \item new methods \code{tokens()} and \code{types()} to quickly get tokens and types of a text
    }
  }
}
\section{Changes in koRpus version 0.07-1 (2016-07-11)}{
  \subsection{fixed}{
    \itemize{
      \item the \code{treetag()} function actually omittet options for the tokenizer due to
        a never updated variable and a wrong setting later on; this has been the
        case for years -- interesting that no-one ever noticed this
      \item \code{read.corp.LCC()} can now digest newer LCC archives, omitting the
        *-meta.txt file if none is present, and also supporting *-words.txt files with
        duplicate columns
      \item some typos in the ChangeLog...
      \item fixed manual page for class kRp.corp.freq
    }
  }
  \subsection{changed}{
    \itemize{
      \item the support for non-UTF-8 presets for was removed, since TreeTagger is
        only endorsing UTF-8 encoding itself for a while; the old preset names will
        continue to work for the time being, but if possible you should already
        rename them from "<lang>-utf8" into just "<lang>" in your scripts
      \item removed options corp.rm.class and corp.rm.tag from method \code{hyphen()} for
        character strings
      \item massively improved the speed of hyphen by using a new method for
        exploding words into their sub-parts. in benchmark tests (text with ~30.000 words)
        the new method only takes about 15\% of the time without cache, and about
        50\% with cache
      \item massively improved the speed of \code{lex.div()} by reducing unnecessary
        computations. in benchmark tests (see above) the new method is more than 100
        times faster, which also makes \code{readability()} three times as fast with standard
        indices. if you disable the FOG index, \code{readability()} is now finished in
        an instant, too. see the new index=\code{"fast"} option below
      \item \code{tokenize()} now uses \code{data.table()} instead of \code{data.frame()} internally,
        leading to an increase in speed of about 20\%
      \item new slots \code{"bigrams"} and \code{"cooccur"} in S4 class kRp.corp.freq
      \item cleaned up code
      \item removed the never used variable TT.tknz.opts.def in the language support
      \item \code{set.lang.support()} now checks for duplicate tag definitions and throws an
        error if any were found
      \item renamed class and method files to set some environment first
      \item moved several internal hyphenation functions to koRpus-internal.hyphen.R
      \item moved several internal readability functions to
        koRpus-internal.rdb.formulae.R
    }
  }
  \subsection{added}{
    \itemize{
      \item \code{read.corp.LCC()} can now import the information on bigrams and
        co-occurences of tokens in a sentence
      \item language support now also uses TT.splitter, TT.splitter.opts, and
        TT.pre.tagger, which was needed mostly to implement the TreeTagger script for
        portuguese (available in the separate package koRpus.lang.pt), but also for
        updates of languages that were already supported
      \item updated the RKWard plugin (UTF-8 defaults, added dutch and portuguese,
        added Brouwer formula)
      \item new unit tests for \code{lex.div()}, \code{tokenize()} and \code{readability()}
      \item new options to set index=\code{"fast"} in \code{readability()} to drop FPG from the
        defaults for faster calculations
      \item new option MTLDMA.steps to increase the step size for MTLD-MA. this
        diverts from the original proposal, but if your text is long enough, you will
        get a very good estimate and only need a fraction of the computing time
    }
  }
}
\section{Changes in koRpus version 0.06-5 (2016-06-05)}{
  \subsection{fixed}{
    \itemize{
      \item fixed the Douma formula: based on available literature, the factor for
        average sentence length was set to 0.33, but the original paper reported it
        as 0.93
      \item fixed the documentation for \code{tokenize()}, roxygen2 had problems with an
        escaped double quote
      \item corrected some problems with umlauts in the docs
    }
  }
  \subsection{added}{
    \itemize{
      \item new template for a roxyPackage script to make it easy to build packages
        from language support scripts
      \item additional validation for ARI, flesch (en), flesch-kincaid, SMOG and FOG,
        via http://wordscount.info/wc/jsp/clear/analyze_readability.jsp
      \item new Flesch parameters to calculate readability according to Brouwer (NL),
        can be invoked as index "Flesch.nl-b", \code{"Flesch.Brouwer"}, or Flesch
        paremeters set to "nl-b"
      \item now the manual is actually documenting all the various Flesch formulas,
        i.e., listing all parameter values, so that it's easier for users to check
        what is being calculated
    }
  }
}
\section{Changes in koRpus version 0.06-4 (2016-03-07)}{
  \subsection{fixed}{
    \itemize{
      \item workaround for missing POS tag \code{"NS"} for english texts
      \item made \code{guess.lang()} compatible with recent format of UDHR archives, now
        using ISO 639-3 codes as language identifier
      \item \code{tokenize()} and \code{treetag()} weren't able to cope with text that only
        consisted of a single token
      \item declared import from graphics package to satisfy CRAN checks
    }
  }
  \subsection{changed}{
    \itemize{
      \item updated rkwarddev script according to recent development in the rkwarddev
        package
      \item some basic validity checks of treetag()s \code{"TT.options"} moved to an
        internal function \code{checkTTOptions()}, which is now also called by \code{set.kRp.env()}
      \item \code{guess.lang()} doesn't warn about missing EOL in the UDHR texts any longer
    }
  }
  \subsection{added}{
    \itemize{
      \item added a README.md file
      \item new option \code{"no.unknown"} can be passed to the \code{"TT.options"} of \code{treetag()},
        to toggle the "-no-unknown" switch of TreeTagger
      \item new option \code{"validate"} for \code{set.kRp.env()} to enable/disable checks
    }
  }
}
\section{Changes in koRpus version 0.06-3 (2015-11-02)}{
  \subsection{fixed}{
    \itemize{
      \item actually query for supported POS tags in internal function
        \code{is.supported.lang()}. the function previously looked for supported languages in the
        available presets, which failed if there was no preset named like the language
        abberviation
      \item made \code{hyphen()} not split words after first or before last character,
        therefore min.length was increased to 4 accordingly
      \item adjusted test standards to changed hyphen results
    }
  }
  \subsection{added}{
    \itemize{
      \item \code{read.tagged()} does now also accept matrix objects, see
        https://github.com/unDocUMeantIt/koRpus/issues/1
    }
  }
}
\section{Changes in koRpus version 0.06-2 (2015-09-21)}{
  \subsection{fixed}{
    \itemize{
      \item \code{read.corp.custom()} calculated the in-document frequency wrong if analysis
        was performed case insensitive
      \item updated some more links in the docs (?kRp.POS.tags)
    }
  }
  \subsection{changed}{
    \itemize{
      \item \code{correct.tag()} now accepts all objects of class union kRp.taggedText
      \item \code{query()} now uses "\%in\%" instead of "==" to match character strings
        against \code{"query"}
      \item exported the previously internal function \code{set.lang.support()}, to prepare
        for the possibility of third party package to add new languages
    }
  }
  \subsection{added}{
    \itemize{
      \item initial support to manually extend the languages supported by the
        package. you can now add new languages on-the-fly in a running session, or in a
        more sustainable manner by providing a language package (using the same
        methods, basically). key to this is the now globally available function
        \code{set.lang.support()}, and there's also two commented template scripts installed
        with the package, see the \code{"templates"} folder
    }
  }
}
\section{Changes in koRpus version 0.06-1 (2015-07-08)}{
  \subsection{fixed}{
    \itemize{
      \item \code{read.corp.custom()} was buggy when dealing with tagged objects
      \item suppress message stating text language in \code{summary()} for readability
        objects if "flat=TRUE"
    }
  }
  \subsection{changed}{
    \itemize{
      \item changed the following functions into S4 methods: \code{readability()},
        \code{lex.div()}, \code{hyphen()}, \code{read.corp.custom()} and \code{freq.analysis()}
      \item removed long since deprecated function \code{kRp.freq.analysis()}
      \item splitted the code of the monolithic internal function for
        \code{read.corp.custom()} into several subfunctions to get more flexibility
      \item \code{read.corp.custom()} now also supports analysis of lists of tagged objects
      \item removed option \code{"fileEncoding"} from the signature of \code{read.corp.custom()},
        but it can still be used as part of the \code{"..."} options; this was neccessary
        because \code{treetag()} uses \code{"encoding"} instead
    }
  }
  \subsection{added}{
    \itemize{
      \item new option \code{"tagger"} now also available in \code{read.corp.custom()}
      \item there is now a mailing list to discuss the koRpus development:
        https://ml06.ispgateway.de/mailman/listinfo/korpus-dev_r.reaktanz.de
    }
  }
}
\section{Changes in koRpus version 0.05-6 (2015-06-30)}{
  \subsection{fixed}{
    \itemize{
      \item changed \code{"selected"} values of \code{checkboxGroupInput()} in the shiny file ui.R
        to comply with the changes made in shiny 0.9.0
      \item function \code{kRp.text.transform()} was missing some columns in TT.res
      \item fixing this ChangeLog: the parameter for Szigriszt (Flesch ES) is not
        \code{"es2"}, as reported in the log to koRpus 0.05.3, but "es-s"!
      \item calling readability for \code{"ARI.NRI"} without hyphenation didn't work,
        allthough ARI doesn't need syllables
      \item updated some broken links in the docs (?kRp.POS.tags, ?guess.lang)
      \item added imports for 'utils' and 'stats' packages to comply with new CRAN
        checks
      \item added a otherwise useless definition of \code{"text"} to the body of
        \code{guess.lang()}, also to satisfy R CMD check
    }
  }
  \subsection{changed}{
    \itemize{
      \item replaced the RKWard plugin with a modularized rewrite (rkwarddev script)
      \item some code cleaning in internal function \code{kRp.rdb.formulae()} and
        \code{freq.analysis()}, mostly replacing @ by \code{slot()}
    }
  }
  \subsection{added}{
    \itemize{
      \item new readability formula \code{tuldava()}, kindly suggested by peter grzybek
      \item the shiny app has gained support for Tuldava and Szigriszt (Flesch ES)
        formulae and log.base parameter (lexical diversity)
      \item \code{set.kRp.env()} does now check whether a language preset is valid
    }
  }
}
\section{Changes in koRpus version 0.05-5 (2014-03-19)}{
  \subsection{changed}{
    \itemize{
      \item removed Snowball from the list of suggested packages, as it is deprecated
        and fully replaced by SnowballC
      \item re-generated all docs with roxygen2 3.1.0, which can now handle S4 class
        definitions properly
      \item replaced all tabs in the source code by two space characters
    }
  }
  \subsection{added}{
    \itemize{
      \item new tf-idf feature: \code{read.corp.custom()} now calculates idf, then
        \code{freq.analysis()} can use that to calculate tf-idf, kindly suggested by sandro tsang
      \item new columns \code{"inDocs"} and \code{"idf"} in slot \code{"words"} of class kRp.corp.freq
      \item new columns \code{"tf"}, \code{"idf"} and \code{"tfidf"} in slot \code{"words"} of class kRp.txt.freq
    }
  }
}
\section{Changes in koRpus version 0.05-4 (2014-01-22)}{
  \subsection{fixed}{
    \itemize{
      \item PCRE 8.34 caused the tests to fail because of problems with regular
        expressions in internal tokenizing function \code{tokenz()}; fixed by ensuring that
        "-" is being escaped as "\\\\-"
    }
  }
}
\section{Changes in koRpus version 0.05-3 (2013-12-21)}{
  \subsection{fixed}{
    \itemize{
      \item due to a logical bug in calls to internal functions, the \code{"lemmatize"}
        argument if \code{lex.div()} didn't really have any effect
      \item using file names with \code{readability()} and its wrappers was broken, works
        again now
    }
  }
  \subsection{changed}{
    \itemize{
      \item the \code{"tt"} slot in class kRp.TTR gained two new entries, \code{"lemmas"} and
        \code{"num.lemmas"}, kindly suggested by roberto trunfio
      \item \code{show()} method for kRp.TTR objects now also lists the number of lemmas (if
        found)
      \item parameters of Flesch formulae were slightly changed to be more accurate
        (from rounded values of 206.84 to 206.835) where applicable
      \item Flesch-Szigriszt and Fernandez-Huerta have been validated against INFLESZ
        v1.0, so the warning was removed
      \item \code{readability.num()} now gracefully accepts a single number of syllables for
        formulae who don't need to know more
      \item added a proper GPL notice at the beginning of each R file
      \item adjustet tests according to the changes made
    }
  }
  \subsection{added}{
    \itemize{
      \item alternative Flesch parameters for spanish texts according to Szigriszt
        were added as parameters=\code{"es2"}, kindly suggested by carlos ortega
    }
  }
  \subsection{removed}{
    \itemize{
      \item this is the first version of the package with slightly reduced sources on
        CRAN -- the debian directory, GPL license file and hyphenation pattern
        ChangeLog had to be removed. if you want the full sources to this package,
        please use the packages provided at http://reaktanz.de/?c=hacking&s=koRpus
    }
  }
}
\section{Changes in koRpus version 0.05-2 (2013-10-27)}{
  \subsection{fixed}{
    \itemize{
      \item added two previously undocumented (and hence missing) italian tags \code{"FW"}
        and \code{"LS"}
      \item removed some ::: operators which were not neccessary
      \item updated slot \code{"param"} of kRp.TTR objects to include \code{"min.tokens"},
        \code{"rand.sample"}, \code{"window"} and \code{"log.base"}
    }
  }
  \subsection{changed}{
    \itemize{
      \item moved some parts of \code{treetag()} and \code{kRp.text.paste()} to internal functions
        for easier re-use of its functionality
    }
  }
  \subsection{added}{
    \itemize{
      \item support for marco baroni's TreeTagger tagset for italian was added
      \item added SnowballC to the suggested packages, as \code{tokenize()} and \code{treetag()}
        can also use \code{SnowballC::wordStem()} for stemming
      \item new function \code{read.tagged()} can be used to import already tagged texts
      \item new argument \code{"apply.sentc.end"} in function \code{treetag()}
      \item new argument \code{"log.base"} in functions \code{lex.div()} and \code{lex.div.num()}
    }
  }
}
\section{Changes in koRpus version 0.05-1 (2013-05-05)}{
  \subsection{fixed}{
    \itemize{
      \item \code{DRP()} readability formula tried to fetch a non-existing variable and
        hence didn't calculate; this also fixed a problem with \code{summary()}, if DRP
        results were expected in the object; tests had to be corrected as well
      \item \code{textFeatures()} gets number of letters and TTR again
      \item MTLD calculation (\code{lex.div()}) now counts a factor as full if it is <
        factor.size, it was implemented as <= factor.size before (thanks to scott
        jarvis for insight on the details)
      \item \code{summary()} for kRp.TTR objects always showed MTLD, even if it was empty
    }
  }
  \subsection{changed}{
    \itemize{
      \item vignette now describes the use of \code{taggedText()} and \code{describe()}, instead of
        direct access to slots
      \item \code{readability()} now assumes that if there's any text, it represents at
        least one sentence, even if no sentence ending punctuation can be found
      \item "quiet=TRUE" in \code{readability()}, \code{readability.num()}, \code{lex.div()} and
        \code{lex.div.num()} will now also suppress all warnings regarding validation status
      \item MTLD calculation (\code{lex.div()}) was optimized and takes less than half of
        the time it used to. it also gained a new boolean argument \code{"detailed"}, which
        is FALSE by default. this means that the full factor results are skipped
        now, which boosts performance even more (six times as fast as before)
      \item the caching mechanism for \code{hyphen()} was restructured into internal
        functions, allowing for better access to the cached data
      \item \code{set.kRp.env()} and \code{get.kRp.env()} have new signatures, namely, all
        previously hardcoded parameters have been replaced by the more flexible \code{"..."}.
        usage stays the same, so there's no need to change any scripts, as long as
        you called all parameters by name, not only by position!
      \item object class kRp.corp.freq can now have additional columns in slots
        \code{"words"} and \code{"desc"}. this flexibility allows for using this class with valence
        data as well
      \item \code{query()} now examines the desired columns to decide whether character or
        numeric operations are to be done
      \item performance of \code{hyphen()} has been massively improved if cache=TRUE
      \item \code{guess.lang()} now also standardizes the difference values; this was added
        to the respective \code{summary()} method, which also produces nicer output
      \item the source code was re-organized a bit, to ensure classes and methods are
        found in an appropriate order; the collate roclet of roxygen2 had
        problems with this when running in R 3.0.0
    }
  }
  \subsection{added}{
    \itemize{
      \item new function \code{read.BAWL()} to import BAWL-R data
      \item new demo application for use with the \code{"shiny"} package, can be found in
        $SRC/inst/shiny
      \item \code{lex.div()} now supports a new method for calculating MTLD (MTLDMA,
        moving-average)
      \item new getter method \code{hyphenText()} to access the \code{"hyphen"} slot in kRp.hyphen
        objects
      \item getter methods \code{language()} and \code{describe()} for kRp.hyphen objects also added
      \item added \code{"quiet"} argument to \code{lex.div.num()}
      \item \code{guess.lang()} can now analyze a given text directly, not only from files
      \item \code{set.kRp.env()} can now explicitly unset parameters in the environment
      \item \code{set.kRp.env()} and \code{get.kRp.env()} know a new parameter,
        \code{"hyphen.cache.file"}, which can be set to a file name to read from/write to the hyphenation
        cache. this way you can easily restore cached hyphenation rules over
        sessions. if this parameter is set, it will be used by \code{hyphen()} automatically if
        called with "cache=TRUE"
    }
  }
}
\section{Changes in koRpus version 0.04-40 (2013-04-07)}{
  \subsection{fixed}{
    \itemize{
      \item removed some non-ASCII characters, mostly from comments, to keep the
        package on CRAN; some author names are now spelled wrong, though...
    }
  }
}
\section{Changes in koRpus version 0.04-39 (2013-03-12)}{
  \subsection{fixed}{
    \itemize{
      \item optimized \code{tokenize()} to also detect prefixes/suffixes of the defined
        heuristics if they co-occur with punctuation
      \item re-saved hyph.fr.rda with explicitly UTF-8 ecoded vectors
      \item renamed LICENSE to LINCENSE.txt, so it won't get installed, as demnanded
        by Writing R Extensions
    }
  }
  \subsection{changed}{
    \itemize{
      \item the language specific heuristics \code{"en"} and \code{"fr"} in \code{tokenize()} were renamed
        into \code{"suf"} and \code{"pre"}. but they are still available, with \code{"fr"} now
        activating both \code{"suf"} and \code{"pre"}.
      \item \code{read.hyph.pat()} now explicitly sets vector encoding to UTF-8 with
        \code{Encoding()}<-, to ensure that the generated objects don't cause warnings from R
        CMD check if they're included in packages
      \item internally replaced paste(..., sep=\code{""}) with paste0(...)
    }
  }
  \subsection{added}{
    \itemize{
      \item added new getter/setter methods \code{taggedText()}, \code{taggedText()}<-, \code{describe()},
        \code{describe()}<-, \code{language()} and \code{language()}<- for tagged text objects
      \item added \code{is.taggedText()} test function
      \item added a warning to \code{treetag()} if \code{"TT.options"} is not a list (because this
        will likely render the options meaningless if they *contain* a list).
      \item \code{tokenize()} can now apply a list of patterns/replacements to given texts
        via the new \code{"clean.raw"} attribute, and even supports perl-like regular
        expressions. the replacements are done before the texts are tokenized, so this
        can be tried to globally clean up bad characters or simply replace
        strings, etc.
      \item \code{tokenize()} and \code{treetag()} have a new option \code{"stopwords"} to enable stopword
        detection
      \item \code{kRp.filter.wclass()} can now remove detected stopwords
      \item \code{tokenize()} and \code{treetag()} have a new option \code{"stemmer"} to interface with
        stemmer functions/methods like \code{Snowball::SnowballStemmer()}
    }
  }
}
\section{Changes in koRpus version 0.04-38 (2012-11-30)}{
  \subsection{added}{
    \itemize{
      \item added support for french (thanks to alexandre brulet)
    }
  }
}
\section{Changes in koRpus version 0.04-37 (2012-09-15)}{
  \subsection{fixed}{
    \itemize{
      \item a typo in Spache calculation (substraction instead of addition of a
        constant) lead to wrong results
      \item Spache now counts unfamiliar words only once, as explained in the
        original article
      \item old Spache formula was missing in readability(index=\code{"all"})
    }
  }
  \subsection{changed}{
    \itemize{
      \item validated Linsear Write, Dale-Chall (1948) and Spache (1953) results and
        removed warnings
      \item status messages of \code{hyphen()} and \code{lex.div()} have been replaced by a space
        saving prograss bar added
      \item added tests for \code{lex.div()}, \code{hyphen()} and \code{readability()}
    }
  }
}
\section{Changes in koRpus version 0.04-36 (2012-08-27)}{
  \subsection{fixed}{
    \itemize{
      \item tests should now work on any machine
    }
  }
}
\section{Changes in koRpus version 0.04-35 (2012-08-21)}{
  \subsection{changed}{
    \itemize{
      \item using utf8-tokenizer.perl now in all UTF-8 presets, also on windows
        systems. the script is part of the windows installer of TreeTagger 3.2 (at
        least since june 2012)
    }
  }
  \subsection{fixed}{
    \itemize{
      \item correct.*() methods now also update the descriptive statistics in
        corrected objects
    }
  }
}
\section{Changes in koRpus version 0.04-34 (2012-06-02)}{
  \subsection{added}{
    \itemize{
      \item there's now a class union \code{"kRp.taggedText"} with the members \code{"kRp.tagged"},
        \code{"kRp.analysis"}, \code{"kRp.txt.freq"} and \code{"kRp.txt.trans"}
    }
  }
  \subsection{changed}{
    \itemize{
      \item advanced \code{summary()} statistics for objects returned by \code{clozeDelete()}
      \item clozeDelete(offset=\code{"all"}) now iterates through all cloze variants and
        prints the results, including the new \code{summary()} data
      \item \code{clozeDelete()} now uses the new class union \code{"kRp.taggedText"} as signature
      \item \code{read.corp.custom()} now uses \code{table()}, \code{"quiet"} is TRUE by default, the new
        option \code{"caseSens"} can be used to ignore character case, and \code{"corpus"} can
        now also be a tagged text object
    }
  }
  \subsection{fixed}{
    \itemize{
      \item \code{summary()} for objects of class kRp.txt.freq was broken
      \item as(\code{"kRp.tagged"}) for objects of class kRp.txt.freq was broken
    }
  }
}
\section{Changes in koRpus version 0.04-33 (2012-05-26)}{
  \subsection{changed}{
    \itemize{
      \item elaborated documentation for method \code{cTest()}
    }
  }
  \subsection{added}{
    \itemize{
      \item added new method \code{clozeDelete()}
      \item added new list \code{"cTest"} in desc slot of the objects returned by \code{cTest()},
        which lists all words that were changed (in \code{clozeDelete()} this list is
        called \code{"cloze"})
    }
  }
}
\section{Changes in koRpus version 0.04-32 (2012-05-11)}{
  \subsection{added}{
    \itemize{
      \item added new function \code{jumbledWords()} and new method \code{cTest()}
    }
  }
  \subsection{fixed}{
    \itemize{
      \item \code{kRp.text.paste()} now also removes superfluous spaces at the end of texts
        (i.e., before the last fullstop)
    }
  }
}
\section{Changes in koRpus version 0.04-31 (2012-04-22)}{
  \subsection{added}{
    \itemize{
      \item koRpus now suggests the \code{"testthat"} package and uses it for automatic tests
      \item \code{treetag()} and \code{tokenize()} now also accept input from open connections
    }
  }
  \subsection{fixed}{
    \itemize{
      \item \code{treetag()} shouldn't fail on file names with spaces any more
    }
  }
}
\section{Changes in koRpus version 0.04-30 (2012-04-06)}{
  \itemize{
    \item added features:
    \item kRp.corp.freq class objects now include the columns 'lttr', 'lemma',
      'tag' and 'wclass'
    \item \code{query()} for corpus frequency objects now returns objects of the same
      class, to allow nested queries
    \item the 'query' parameter of \code{query()} can now be a list of lists, to
      facilitate nested requests more easily
    \item \code{query()} can now invoke \code{grepl()}, if 'var' is set to \code{"regexp"}; i.e., you
      can now filter words by regular expressions (inspired by suggestions after
      the koRpus talk at TeaP 2012)
  }
}
\section{Changes in koRpus version 0.04-29 (2012-04-05)}{
  \itemize{
    \item fixed bug in \code{summary()} for tagged objects without punctuation
    \item renamed \code{kRp.freq.analysis()} to \code{freq.analysis()} (with wrapper function for
      backwards compatibility)
    \item \code{readability.num()} can now directly digest objects of class kRp.readability
    \item data documentation hyph.XX is now a roxygen source file as well
    \item cleaned up \code{summary()} and \code{show()} docs
    \item adjustements to the roxygen2 docs (methods)
  }
}
\section{Changes in koRpus version 0.04-28 (2012-03-10)}{
  \itemize{
    \item code cleanup: initialized some variables by setting them NULL, to avoid
      needless NOTEs from R CMD check (\code{hyphen()}, and internal functions
      \code{frqcy.by.rel()}, \code{load.hyph.pattern()}, \code{tagged.txt.rm.classes()} and
      \code{text.freq.analysis()})
    \item re-formatted the ChangeLog so roxyPackage can translate it into a NEWS.Rd
      file
  }
}
\section{Changes in koRpus version 0.04-27 (2012-03-07)}{
  \itemize{
    \item prep for CRAN release:
    \item 0.04-26 was short-lived...
    \item really fixed plot docs
    \item removed usage section from hyph.XX data documentation
    \item renamed \code{text.features()} to \code{textFeatures()}
    \item encapsulated examples in \code{set.kRp.env()}/get.\code{kRp.env()} in \\dontrun\{\}
    \item re-encoded hyph.XX data objects to UTF-8
    \item replaces non-ASCII characters in code with unicode escapes
  }
}
\section{Changes in koRpus version 0.04-26 (2012-03-07)}{
  \itemize{
    \item fixed plot docs
    \item prep for inital CRAN release
  }
}
\section{Changes in koRpus version 0.04-25 (2012-03-05)}{
  \itemize{
    \item re-compressed all hyphenation pattern data files, using xz compression
    \item lifted the R dependency from 2.9 to 2.10
    \item compressed LCC tarballs are now detected automatically
    \item \code{kRp.freq.analysis()} now also lists the log10 value of word frequencies in
      the TT.res slot
    \item in the desc slot of kRp.txt.freq class objects, the rather misleading
      list elements \code{"freq"} and \code{"freq.wclass"} were more adequately renamed to
      \code{"freq.token"} and \code{"freq.types"}, respectively
    \item unmatched words in frequency analyses now get value 0, not NA
    \item fixed wrong signature for option \code{"tagger"} in \code{kRp.text.analysis()}
    \item fixed \code{kRp.cluster()} which still called some old slots
  }
}
\section{Changes in koRpus version 0.04-24 (2012-03-01)}{
  \itemize{
    \item fixed bug for attempts to calculate value distribution texts without any
      sentence endings
    \item all readability wrapper functions now also accept a list of text features
      for calculation
    \item class kRp.readability now inherits kRp.tagged
    \item \code{readability()} now checks for presence of a hyphen slot and re-uses it, if
      no new hyphen object was provided; this in addition to the previous
      change enables one to re-analyze a text more efficiently, as already calculated
      results are also preserved
    \item letter and character distribution in kRp.tagged desc slot now include
      columns with zero values if the respective values are missing (e.g., no words
      with five letters, but some with six, etc.)
    \item added summary method for class kRp.tagged, summarizing main information
      from the desc slot
    \item added plot method for class kRp.tagged
    \item show method for kRp.readability now lists unfamiliar words for
      Harris-Jacobson
    \item cleaned up code of \code{lex.div.num()} a bit
  }
}
\section{Changes in koRpus version 0.04-23 (2012-02-24)}{
  \itemize{
    \item added precise RGL formula option to FORCAST
    \item removed validation warnings from several indices, because results have
      been checked against those of other tools, and were comparable, so the
      implementations of these measures are assumed to be correct: - \code{lex.div()}: TTR,
      MSTTR, C, R, CTTR, U, Maas, HD-D, MTLD (thanks a lot to scott jarvis &
      phil mccarthy for calculating sample texts!) - \code{readability()}: ARI, ARI NRI,
      Bormuth, Coleman-Liau, Dale-Chall, Dale-Chall PSK, DRP,
      Farr-Jenkins-Paterson, Farr-Jenkins-Paterson PSK, Flesch, Flesch PSK, Flesch-Kincaid, FOG,
      FOG PSK, FORCAST, LIX, RIX, SMOG, Spache, Wheeler-Smith
    \item moved all calculation from \code{readability()} to an internal function
      \code{kRp.rdb.formulae()}. to make it easier to write a similar function to \code{lex.div.num()}
      for the readability fomulas as well
    \item added \code{readability.num()}
    \item adjusted exsyl calculation for ELF to the approach used in other
      measures, which also results in a change of its default \code{"syll"} parameter from 1 to
      2; also corrected a typo in the docs, the index was proposed by Fang, not
      Farr
    \item readability results now list letter distribution, not character
      distribution in desc slot
    \item the desc slot from readability calculations was enhanced so that it can
      directly be used as the txt.features parameter for \code{readability.num()}
    \item docs were polished
  }
}
\section{Changes in koRpus version 0.04-22 (2012-02-08)}{
  \itemize{
    \item further fixes to the Wheeler-Smith implementation. according to the
      original paper, polysyllabic words need to be counted, and the example given
      shows that this means words with more than one syllable, not three or more,
      as Bamberger & Vanecek (1984) suggested
    \item fixed HD-D, previous results are now labelled as ATTR in the HDD slot
    \item adjusted HD-D.char calculation for small number of tokens (probabilities
      are now set to 1, not NaN)
    \item added MATTR characteristics
    \item \code{show()} for \code{lex.div()} objects now also reports SD for characteristics
  }
}
\section{Changes in koRpus version 0.04-21 (2012-02-07)}{
  \itemize{
    \item MTLD now uses a slightly more efficient algorithm, inspired by the one
      used for MATTR
    \item MSTTR now also reports SD of TTRs
    \item differentiated the word class adposition into pre-, post- and
      circumposition in the language support for german and russian
    \item added both Tränke-Bailer formulae to \code{readability()}, incl. wrapper
      \code{traenkle.bailer()} and \code{show()}/summary() methods
    \item Coleman formulae now also count only prepositions as such
    \item fixed Wheeler-Smith (thanks to eleni miltsakaki)
  }
}
\section{Changes in koRpus version 0.04-20 (2012-02-06)}{
  \itemize{
    \item added Moving Average TTR (MATTR) to \code{lex.div()}, incl. wrapper  \code{MATTR()} and
      \code{show()}/summary() methods
    \item added \code{"rand.sample"} and \code{"window"} to the parameters returned by \code{lex.div()}
    \item further re-arranged the code of \code{readability()} and \code{lex.div()} to make it
      easier to maintain
    \item summary(flat=TRUE) for readability objects is now a numeric vector
  }
}
\section{Changes in koRpus version 0.04-19 (2012-02-02)}{
  \itemize{
    \item added five harris-jacobson readability formulae, incl. wrapper
      \code{harris.jacobson()} and \code{show()}/summary() methods
    \item updated vignette
    \item MTLD characteristics are now twice as fast
    \item classes \code{"kRp.txt.freq"} and \code{"kRp.txt.trans"} now simply extend
      \code{"kRp.tagged"}, and \code{"kRp.analysis"} extends \code{"kRp.txt.freq"}
    \item removed internal function \code{check.kRp.object()} (globally replaced by
      \code{inherits()})
    \item fixed letter count issue in \code{readability()}
    \item fixed bugs in loading word lists in \code{readability()}
    \item fixed crash if index=\code{"all"} in \code{readability()}
    \item reordered default kRp.readabilty slot order alphabetically, as well as
      \code{show()} and \code{summary()} for readability results
    \item renamed results of the Neue Wiener Sachtextformeln from WSTF* to nWS* in
      readability object methods \code{show()} and \code{summary()} for consistency
    \item renamed \code{WSFT()} to \code{nWS()} for the same reason
    \item cleaned up roxygen comments for more roxygen2 compliance
  }
}
\section{Changes in koRpus version 0.04-18 (2012-01-22)}{
  \itemize{
    \item added missing word exclusion to Gunning FOG measure
    \item added sentence length, word length, distribution of characters and
      letters to \code{"desc"} slot of class kRp.tagged and \code{readability()} results, where
      missing
    \item both syllable (\code{hyphen()}) and character distributions gained inversed
      cummulation for absolute numbers and percentages, so this one table now makes
      it easy to see how many words with more/equal/less characters/syllables
      there are in a text
    \item changed internals of \code{kRp.freq.analysis()} and \code{readability()} to re-use
      descriptives of tagged text objects
    \item NOTE: this also changed the names of some result elements in their \code{"desc"}
      slots for overall consistency (\code{"avg.sent.len"} is now \code{"avg.sentc.length"},
      \code{"avg.word.len"} became \code{"avg.word.length"}, and instances of \code{"num.words"},
      \code{"num.chars"} etc. lost the \code{"num."} prefix). in case you accessed these
      directly, check if you need to adopt these changes. this is a first round of
      changes towards 0.05, see the notes to 0.04-17 below!
  }
}
\section{Changes in koRpus version 0.04-17 (2012-01-17)}{
  \itemize{
    \item replaced the english hyphenation parameter set with a new one, which was
      made with PatGen2 especially for koRpus
    \item \code{tokenize()} will now interpret single letters followed by a dot as an
      abbreviation (e.g., of a name), not a sentence ending, if heuristics include
      \code{"abbr"}
    \item fixed bug which caused \code{hyphen()} to drop syllables if only one pattern
      match was found
    \item added cache support to the correct method of class kRp.hyphen
    \item added number of words and sentences to \code{"desc"} slot of class kRp.tagged
    \item elaborated \code{treetag()} error message if no TreeTagger command was specified
    \item NOTE: koRpus 0.05 will likely merge some object classes similar to
      kRp.tagged, i.e. kRp.txt.freq and kRp.txt.trans, into one class for tokenized
      text, either replacing or inheriting those classes
  }
}
\section{Changes in koRpus version 0.04-16 (2012-01-15)}{
  \itemize{
    \item added slot \code{"desc"} to class kRp.tagged, to have descriptive statistics
      directly available in the object
    \item added support for descriptive statistics to \code{tokenize()} and \code{treetag()}
    \item added function \code{text.features()} to extract a 9-features set from texts for
      authorship detection (inspired by a talk at the 28C3)
    \item \code{hyphen()} can now cache results on a per session basis, making it
      noticeably faster
  }
}
\section{Changes in koRpus version 0.04-15 (2012-01-04)}{
  \itemize{
    \item \code{manage.hyph.pat()} is now an exported function
    \item added initial support for italian (thanks to alberto mirisola)
    \item added italian hyphenation patterns
    \item changed min.length from 4 to 3 in \code{hyphen()} and \code{manage.hyph.pat()}
    \item hyphen now considers hyphenating before last letters of a word
    \item tuned hyph.en (with contributions by laura hauser)
    \item fixed check for existing tokenizer, tagger and parameter file in \code{treetag()}
    \item fixed MTLD calculation for texts which don't make even one factor
  }
}
\section{Changes in koRpus version 0.04-14 (2011-12-22)}{
  \itemize{
    \item added new internal function \code{manage.hyph.pat()} to add/replace/remove
      pattern entries for hyphenation
    \item added number of tokens per factor and standard deviation to MTLD results
      (thx to aris xanthos for the suggestion)
  }
}
\section{Changes in koRpus version 0.04-13 (2011-11-22)}{
  \itemize{
    \item added column \code{"token"} to slots MTLD$all.forw and MTLD$all.back of
      \code{lex.div()} results, so you can verify the results more easily
    \item slot HDD$type.probs of \code{lex.div()} results is now sorted (decreasing)
    \item removed warnings of missing encoding, since \code{enc2utf()} seems to do a
      pretty good job
  }
}
\section{Changes in koRpus version 0.04-12 (2011-11-21)}{
  \itemize{
    \item added support for the newer LCC .tar archive format
    \item changed vignette accordingly
    \item for consistency, changed \code{"words"} and \code{"dist.words"} into \code{"tokens"} and
      \code{"types"} in class kRp.corp.freq, slot desc
    \item added lgeV0 and the relative vocabulary growth measures suggested by Maas
      to \code{lex.div()}; furthermore, a is now reported instead of a^2
    \item added lgV0 and lgeV0 to \code{lex.div.num()}
    \item show method for class kRp.TTR now excludes Inf values from
      charasteristics values
  }
}
\section{Changes in koRpus version 0.04-11 (2011-11-20)}{
  \itemize{
    \item added function \code{lex.div.num()}, calculates TTR family measures by numbers
      of tokens and types directly
    \item cleaned up \code{lex.div()} code a little
  }
}
\section{Changes in koRpus version 0.04-10 (2011-11-19)}{
  \itemize{
    \item fixed missing 'input.enc' information if \code{treetag()} option 'treetagger' is
      not \code{"manual"} but a script
    \item enhanced encoding handling internally if none was specified
    \item changed default value of 'case.sens' to FALSE in \code{lex.div()}, as this seems
      to be more common
    \item changed default value of 'fileEncoding' from "UTF-8" to NULL and use
      \code{enc2utf()} internally if no encoding was defined
  }
}
\section{Changes in koRpus version 0.04-9 (2011-10-27)}{
  \itemize{
    \item \code{tokenize()} now converts all input to UTF-8 internally, to prevent
      conflicts later on (\code{treetag()} does that since 0.04-7 already)
    \item added an experimental feature to \code{treetag()} to replace TreeTagger's
      tokenizer with \code{tokenize()}
  }
}
\section{Changes in koRpus version 0.04-8 (2011-09-21)}{
  \itemize{
    \item fixed bugs in \code{treetag()}: \code{"debug"} now works without \code{"manual"} config as
      well, and global TT.options are now found if no preset was selected
  }
}
\section{Changes in koRpus version 0.04-7 (2011-09-16)}{
  \itemize{
    \item added \code{"encoding"} option to \code{treetag()} and defaults to the language presets
    \item fixed some option check and file path issues in \code{treetag()}
  }
}
\section{Changes in koRpus version 0.04-6 (2011-09-11)}{
  \itemize{
    \item fixed package description for R 2.14
  }
}
\section{Changes in koRpus version 0.04-5 (2011-09-01)}{
  \itemize{
    \item fixed dozends of small glitches in the docs which caused warnings during
      package checks
  }
}
\section{Changes in koRpus version 0.04-4 (2011-08-23)}{
  \itemize{
    \item fixed bug in getting the right preset: mixed \code{"lang"} and \code{"preset"} during
      the modularization
  }
}
\section{Changes in koRpus version 0.04-3 (2011-08-19)}{
  \itemize{
    \item modularized language support by the internal function \code{set.lang.support()},
      this should make it much easier to add new languages in the future,
      because it means to add only one R file. \code{hyphen()}, \code{kRp.POS.tags()} and \code{treetag()}
      now use this new method
    \item added CITATION file
  }
}
\section{Changes in koRpus version 0.04-2 (2011-08-18)}{
  \itemize{
    \item fixed duplicate \code{"PREP"} definition in spanish POS tags, which caused
      \code{treetag()} to consume lots of RAM
    \item fixed superfluous \code{"es"} definitions in \code{treetag()}
  }
}
\section{Changes in koRpus version 0.04-1 (2011-08-16)}{
  \itemize{
    \item added support for spanish (thanks to earl brown)
    \item docs can be created from source by roxygen2 (but all class docs are
      static, until '@slot' works again)
  }
}
\section{Changes in koRpus version 0.03-4 (2011-08-09)}{
  \itemize{
    \item added support for autodetection of headlines and paragraphs in \code{tokenize()}
    \item added support to revert autodetected headlines and paragraphs in
      \code{kRp.text.paste()}
    \item updated RKWard plugin to use \code{tokenize()}
  }
}
\section{Changes in koRpus version 0.03-3 (2011-08-08)}{
  \itemize{
    \item added parameters for formula C and simplified formula to SMOG
    \item enhanced readability formulas (like adding age levels to Flesch.Kincaid,
      grade levels to LIX)
    \item removed the duplicate Amstad index (is now just Flesch.de)
  }
}
\section{Changes in koRpus version 0.03-2 (2011-08-03)}{
  \itemize{
    \item added the full RKWard plugin as inst/rkward, so both get updated
      simultanously
    \item added experimental internal functions to import result logs from
      Readability Studio and TextQuest
  }
}
\section{Changes in koRpus version 0.03-1 (2011-07-29)}{
  \itemize{
    \item integrated internal tags to \code{kRp.POS.tags()}, so \code{tokenize()} can return
      valid kRp.tagged class objects, i.e. substitute TreeTagger if it's not
      available
    \item consequently renamed 'treetagger' option into 'tagger' in \code{readability()},
      \code{kRp.freq.analysis()} and \code{kRp.text.analysis()}
    \item lots of small fixes
  }
}
\section{Changes in koRpus version 0.02-9 (2011-07-17)}{
  \itemize{
    \item added a simple \code{tokenize()} function
    \item first working version of \code{read.corp.custom()}
    \item added \code{"..."} option to readability, kRp.freq.analysis and
      kRp.text.analysis, to configure \code{treetag()}
    \item added TT.options to the get/set environment functions
    \item changed default values for \code{treetag()} (for readability)
    \item fixed bug in internal \code{check.file()} function (mode=\code{"exec"} returned TRUE
      too soon)
    \item added warning messages to \code{readability()} and \code{lex.div()} to make people
      aware these implemetations are not yet fully validatied
    \item introduced release dates in this ChangeLog ;-) (reconstructed them for
      earlier releases from the time stamps on the server)
  }
}
\section{Changes in koRpus version 0.02-8 (2011-07-03)}{
  \itemize{
    \item added \code{"desc"} slot with some statistics to class kRp.hyphen and \code{hyphen()}
    \item added grading information for Flesch and RIX measures
    \item fixed grading for Wheeler-Smith formula
    \item introduced \code{"quiet"} options for \code{hyphen()}, \code{lex.div()} and \code{readability()}
    \item further improved the vignette, elaborated on the examples
  }
}
\section{Changes in koRpus version 0.02-7 (2011-06-29)}{
  \itemize{
    \item fixed typo in kRp.POS.tags(\code{"ru"}): "Vmis-sfa-e" tags no longer a \code{"vern"},
      but a \code{"verb"}
    \item removed XML package dependency again, by writing a small parser (there
      was no windows binary for the XML package, which was obviously a problem...)
    \item fixed \code{"quiet"} option in \code{guess.lang()}
  }
}
\section{Changes in koRpus version 0.02-6 (2011-06-26)}{
  \itemize{
    \item fixed bug in calculation of sentence lengths in \code{kRp.freq.analysis()}
      (counted punctuation as words)
    \item tweaked hyph.en patterns to get better results
    \item solved a small charset issue in \code{treetag()}
    \item fixed \code{hyphen()} output if doubled hyphenation marks appeared
  }
}
\section{Changes in koRpus version 0.02-5 (2011-06-25)}{
  \itemize{
    \item elaborated the vignette a little (including some references)
    \item added support for zipped LCC database archives to \code{read.corp.LCC()}
    \item improved handling of unknown POS tags: now causes an error dump for
      debugging
    \item added \code{query()} method to search in objects of class kRp.tagged
  }
}
\section{Changes in koRpus version 0.02-4 (2011-06-18)}{
  \itemize{
    \item de-factorized \code{treetag()} output
    \item fixed hyphenation problems (remove all non-characters for \code{hyphen()})
  }
}
\section{Changes in koRpus version 0.02-3 (2011-06-11)}{
  \itemize{
    \item fixed missing "''" and "$" POS tags in kRp.POS.tags(\code{"en"})
  }
}
\section{Changes in koRpus version 0.02-2 (2011-06-06)}{
  \itemize{
    \item renamed \code{kRp.guess.lang()} to \code{guess.lang()}
    \item \code{guess.lang()} now gzips only in memory by default, saves about 1/8 of
      processing time - added option \code{"in.mem"} to switch back to previous behavious
      (temporary files)
    \item added internal function \code{is.supported.lang()} as a possible wrapper for
      guessed ULIs
    \item added internal functions \code{roxy.description()} and \code{roxy.package()} to ease
      development
  }
}
\section{Changes in koRpus version 0.02-1 (2011-06-04)}{
  \itemize{
    \item added support for automatic language determination: - changed internal
      function \code{compression.ratio()} to \code{txt.compress()} - added internal function
      \code{read.udhr()} - added \code{kRp.guess.lang()} and class kRp.lang
  }
}
\section{Changes in koRpus version 0.01-8 (2011-05-30)}{
  \itemize{
    \item added class kRp.txt.trans for results of \code{kRp.text.transform()}
    \item enhanced function \code{kRp.text.transform()}, most notably calculate differences
  }
}
\section{Changes in koRpus version 0.01-7 (2011-05-28)}{
  \itemize{
    \item added function \code{kRp.text.paste()}
    \item added function \code{kRp.text.transform()}
  }
}
\section{Changes in koRpus version 0.01-6 (2011-05-27)}{
  \itemize{
    \item fixed \code{hyphen()} bug (leading dots in words caused functions to fail)
    \item added \code{kRp.filter.wclass()}
    \item added TODO list to the sources
  }
}
\section{Changes in koRpus version 0.01-5 (2011-05-16)}{
  \itemize{
    \item fixed another bug in frequency analysis with corpus data (superfluous
      class definition)
    \item fixed missing POS tags: refinement of english tags (extra tags for "to
      be" and "to have")
    \item added more to the vignette
    \item added .Rinstignore file to clean up the doc folder
  }
}
\section{Changes in koRpus version 0.01-4 (2011-05-12)}{
  \itemize{
    \item began to write a vignette
    \item fixed \code{treetag()} failing on windows machines (hopefully...)
  }
}
\section{Changes in koRpus version 0.01-3 (2011-05-10)}{
  \itemize{
    \item added TRI readability index
    \item fixed bug in frequency analysis with corpus data (wrong class definition)
    \item fixed bug in Bormuth implementation (didn't fetch parameters)
    \item fixed missing Flesch indices in summary method
    \item corrected display of FOG indices in summary method (grade instead of raw)
    \item added \code{compression.ratio()} to internal functions
  }
}
\section{Changes in koRpus version 0.01-2 (2011-05-03)}{
  \itemize{
    \item enhanced \code{query()} methods
    \item fixed some typos and smaller bugs
  }
}
\section{Changes in koRpus version 0.01-1 (2011-04-24)}{
  \itemize{
    \item initial public release (via reaktanz.de)
  }
}
