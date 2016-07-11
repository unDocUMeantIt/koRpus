## TEMPLATE FILE -- ADJUST TO YOUR LANGUAGE AND PREFERENCES
##
## you can use this template file to build a proper R package from your
## lang.support-xx.R and hyph.xx-data.R files. that is, you must have
## prepared them first before you can adjust and run this script!
## 
## throughout the template, there are some values you need to replace globally:
##   xyzedish: template name for the language (replace with "english", "dutch" etc.)
##   xx:       template name for the language abbreviation (replace with "en", "nl" etc.)
## 
## when you're done, remove this block ;-)
## 
## this script uses functions of the R package roxyPackage, so you need to
## install that one first, as well:
##   http://reaktanz.de/?c=hacking&s=roxyPackage
##   
##########################################################################
##
## roxyPackage will create all of the directories and files needed for
## packaging. all you need is the two R files lang.support-xx.R and
## hyph.xx-data.R, and this script.
##   
##########################################################################
##
## this is how it's done:

## 1. place lang.support-xx.R and hyph.xx-data.R in an R directory
#    
#     R packages need to have the actual R code in a directory called R,
#     right below the root directory of the package sources. your package
#     should be called "koRpus.lang.xx", so the directory structure should
#     look like this:
#    
#       koRpus.lang.xx/
#         R/
#           hyph.xx-data.R
#           lang.support-xx.R

require(roxyPackage)
local({

## 2. add metadata to this script
# 
#     roxyPackage will use the information you give here to generate all
#     the files you usually find in an R package. it also needs to know
#     where to find the package source directory you created in 1., and
#     where to put the results. for an in-depth tour through all features
#     please refer to the roxyPackage vignette, we'll just cover the basic
#     stuff here
#    

    # information about yourself, the package creator/maintainer
    name.first <- "firstname"
    name.middle <- ""
    name.family <- "familyname"
    email <- "first.family@example.org"
    
    # name of the language
    language.long <- "Xyzdish"
    language.short <- "xx"

    # the package version number
    package.version <- "0.01-1"
    package.source.dir <- file.path("/path","to","your","package","sources")
    # roxyPackage sets up a fully functional R package repository,
    # you can set the root directory here, it will be created if not existing.
    # NOTE: no files will be written to this directory if the script is run in
    # sandbox mode, see below
    package.local.repository <- file.path("/path","to","your","local","repository")

    # where is your R installation?
    # this is where you install packages to locally, e.g., ~/R.
    # this directory must exist
    R.local.libs <- file.path("/path","to","R")

    # entries for a ChangeLog file
    ChangeLog.entry <- list(
      added="initial release"
    )

    # set sandbox mode
    # 
    # this call turns on sandboxing for the repository, meaning it will only
    # be created in a temporary directory. however, the options
    # "pck.source.dir=FALSE" and "R.libs=FALSE" exclude the "package.source.dir"
    # and your local R library ("R.local.libs") from sandboxing, so those files
    # *will* be updated. change this to whatever you like, e.g.
    # sandbox(TRUE) to copy everything to the tempdir, or sandbox(FALSE) to also
    # create/update your actual repository
    sandbox(TRUE, pck.source.dir=FALSE, R.libs=FALSE)

## 3. run the script!
# 
#     you're done now with configuring, you can run the full script now.
#     it's recommended to try with a copy of your R files first, to not
#     mess things up accidently. once you get a feeling for what's going on
#     here, feel free to adjust parts of this script, also in the following
#     sections
#    
#     note that the script not only initializes a package from scratch, but
#     can also be used for updating it. normally, you'd only have to adjust
#     the version number and ChangeLog entry, and then run it again.

## 
##  no need for further configuration below here
## 
################################################

    # this can now be generated from the info given above
    name.full <- paste(name.first, name.middle, name.family)
    package.name <- paste0("koRpus.lang.", language.short)


    package.description <- data.frame(
        Package=package.name,
        Type="Package",
        Title=paste0("Language support for koRpus: ", language.long),
        Author= paste0(name.full, " [aut, cre]"),
        AuthorsR=paste0("c(person(given=\"", name.first, "\", ",
          if(!is.null(name.middle) & !identical(name.middle, "")){
            paste0("middle=\"", name.middle, "\", ")
          },
          "family=\"", name.family, "\", email=\"", email, "\", role=c(\"aut\", \"cre\")))"),
        Maintainer=paste0(name.full, " <", email, ">"),
        Depends="R (>= 3.3.0), koRpus (>= 0.06-3), methods",
        Description=paste0(
          "Adds support for the ", language.long, " language to the koRpus package.",
          "Due to some restrictions on CRAN, the full package sources are only available from the project homepage.",
          "To ask for help, report bugs, suggest feature improvements, or discuss the global development of the package,",
          "please subscribe to the koRpus-dev mailing list: https://ml06.ispgateway.de/mailman/listinfo/korpus-dev_r.reaktanz.de"
        ),
        License="GPL (>= 3)",
        Encoding="UTF-8",
        LazyLoad="yes",
        URL="http://reaktanz.de/?c=hacking&s=koRpus",
        stringsAsFactors=FALSE)

    roxy.package(actions=c(
        "roxy",            # roxygenize the docs
        "cite",            # update CITATION file
        "doc",             # update pdf documentation
        "cl2news",         # transform ChangeLog into NEWS.Rd
#         "news2rss",      # transform NEWS.Rd to RSS feed
#         "html",          # update HTML index files
#         "win",           # update the windows binary package
#         "deb",           # debianize the package (read the docs first!)
#         "macosx",        # update the mac OS X binary package
        "cleanRd",         # linebreaks for >90 chars in *.Rd
        "log",             # update ChangeLog
        "package"#,        # build & install koRpus.lang.xx package
#        "readme",         # add initial README.md file
#        "check"           # check package
#        "license",        # update LICENSE file
        ),
        pck.description=package.description,
        pck.source.dir=package.source.dir,
        pck.version=package.version,
        R.libs=R.local.libs,
        repo.root=package.local.repository,
        cleanup=TRUE,
        URL="http://R.reaktanz.de",
        ChangeLog=ChangeLog.entry
    )
})
