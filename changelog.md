1.2.1

* Minor updates to cabal file, comments, etc.

1.2

* Speedup thanks to <https://github.com/elaforge/fast-tags/pull/22>.
Also many tags previously incorrectly marked as 'p' Pattern are now
correctly 'f' Function.

1.1.2:

* Implement `--no-module-tags` command line parameter to optionally avoid
tagging modules.

1.1.1:

* fix 'format' output for vim

1.1:

* Decode UTF8 leniently, so non-UTF8 will no longer cause a crash.  Removed
the --ignore-encoding-errors flag, since that's the default behaviour now.

1.0:

* Merged a whole bunch of patches from Sergey Vinokurov.  Copy paste from
<https://github.com/elaforge/fast-tags/pull/6>:

* recognize more syntactic constructs (consider tests as specification of
what's handled)

* add more tests

* use tasty to organize tests

* ability to produce emacs tags

* handling of literate files

* new mode to recursively traverse directory tree and search for haskell files

* optionally ignore encoding errors during reading and skip offending files

* ability to read \n-separated or \0-separated list of files from stdin
and blazing-fast speed of tag generation is presevred

0.0.6:

* fix bug where class context in a class's methods would be confused for the
context of the class itself

0.0.5:

* Tags with the same name are sorted by their type: Function, Type,
Constructor, Class, Module.

0.0.4:

* Fixed bug that prevented old tags from being filtered out.

0.0.3:

* Lots of speed ups, especially when given lots of files at once.

* Support for type families and GADTs.

* Support infix operators, multiple declarations per line, and fix various
other bugs that missed or gave bad tags.
