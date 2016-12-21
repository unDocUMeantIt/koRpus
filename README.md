# koRpus

[![Flattr this git repo](https://api.flattr.com/button/flattr-badge-large.png)](https://flattr.com/submit/auto?user_id=m.eik&url=https://github.com/unDocUMeantIt/koRpus&title=koRpus&language=en_GB&tags=github&category=software)

koRpus is an [R](https://r-project.org) package for text analysis. This includes, amongst others,
a wrapper for the POS tagger [TreeTagger](http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/),
functions for automatic language detection, hyphenation, several indices of lexical diversity
(e.g., type token ratio, HD-D/vocd-D, MTLD) and readability (e.g., Flesch, SMOG, LIX, Dale-Chall, Tuldava).

koRpus also includes a plugin for [RKWard](https://rkward.kde.org), a powerful GUI and
IDE for R, providing graphical dialogs for its basic features. To make full use of this feature,
please install [RKWard](https://rkward.kde.org) (plugins are detected automatically).

More information on koRpus is available on the [project homepage](https://reaktanz.de/?c=hacking&s=koRpus).

## Installation

There are three easy ways of getting koRpus:

### Stable releases via CRAN

The latest release that is considered stable for productive work can be found on the CRAN mirrors, which
means you can install it from a running R session like this:

```
install.packages("koRpus")
```

The CRAN packages are usually a bit behind the recent state of the package, and only updated after a
significant amount of changes or important bug fixes.

### Development releases via the project repository

Inbetween stable CRAN releases there's usually several testing or development versions released on the project's
own repository. These releases should also work without problems, but they are also intended to test new features
or supposed bug fixes, and get feedback before the next release goes to CRAN.

Installation is fairly easy, too:

```
install.packages("koRpus", repo="https://reaktanz.de/R")
```

To automatically get updates, consider adding the repository to your R configuration.  You might also
want to subscribe to the package's [RSS feed](https://reaktanz.de/R/pckg/koRpus/RSS.xml) to get notified of new releases.

If you're running a Debian based operating system, you might be interested in the
[precompiled *.deb packages](https://reaktanz.de/R/pckg/koRpus/deb_repo.html).

### Installation via GitHub

To install it directly from GitHub, you can use `install_github()` from the [devtools](https://github.com/hadley/devtools) package:

```
library(devtools)
install_github("unDocUMeantIt/koRpus") # stable release
install_github("unDocUMeantIt/koRpus", ref="develop") # development release
```

## Contributing

To ask for help, report bugs, suggest feature improvements, or discuss the global
development of the package, please either subscribe to the
[koRpus-dev mailing list](https://ml06.ispgateway.de/mailman/listinfo/korpus-dev_r.reaktanz.de), or
use the issue tracker on GitHub.

## Licence

Copyright 2012-2015 Meik Michalke <meik.michalke@hhu.de>

koRpus is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

koRpus is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with koRpus.  If not, see <http://www.gnu.org/licenses/>.
