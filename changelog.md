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
