2.0.2

* fixes for ghc 9.0 and 9.2, thanks to @sergv and @ozkutuk

2.0.1

* Add support to qualified_tags.py for tag preview.

* support record pattern synonyms

* Reduce false positives from Template Haskell

2.0.0

* Added example git hooks in tools/ to incrementally update tags across
checkouts, pulls, rebases.  There is now no reason to generate tags from
scratch, except to clean out changes that this misses.

* accept multiple --src-prefix flags, in case your haskell repo has multiple
roots, but no cabal files

* add --cabal flag, which parses .cabal files and generates tags for the package

* collect tags from Alex and Happy files

* collect tags from some hsc2hs directives

* collect tags from #define and #let constructs

* lexer is now almost twice as fast

* improved line number precision when analysing Literal Haskell

1.5.0

* many small accuracy fixes from @sergv, see
<https://github.com/elaforge/fast-tags/pull/40>

* fix a bug with emacs tags where I suppressed all adjacent tags,
not just ones with the same name

* change emacs output to a hopefully more compatible form

* add --exclude flag

1.4.2

* add --src-prefix flag

Minor UI cleanups:

* don't read filenames from stdin on no arguments, use - for that

* don't automatically read contents of directories

* don't automatically recurse into any directories starting with dot

* don't follow symlinks by default when -R is given

Make qualified tags work better:

* qualified_tag.py: strip leading and trailing dots

* make qualified_tag.py retry qualified tags without qualification

1.4.1

* Ghc 8.4.1 compatibility.

1.4

* Add qualified_tag.py, which is more clever about following qualified symbols,
without modifying iskeyword.  I also added --fully_qualified to support it.
Now renamed imports such as `import A.B.C as D` should chase `D.x` to
`A.B.C.x`.  Thanks to JonnyRa in https://github.com/elaforge/fast-tags/issues/33
for the inspiration.

1.3

* A tag will suppress any other tags with the same name within 2 lines.
This should prevent multiple tag matches for things like `data X = X`.
Currently the 2 is not configurable, but could be easily enough if someone
wanted that.

* Lock stderr when writing so warnings don't get scrambled.

* A --qualified flag which emits tags qualified by module name.  There's an
example vimrc line that can use that without having to permanently modify
iskeyword.

* Tags should be always sorted now, instead of being
inconsistently-but-mostly-sorted as they were before.  I replaced
complicated code trying to keep the output sorted with one sort at the end.
It's simpler and doesn't seem to be measurably slower.

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
