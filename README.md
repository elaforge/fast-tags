# fast-tags - fast and robust tag generator for Haskell #

[![Build Status][badge-travis]](https://travis-ci.org/elaforge/fast-tags)

# Supported GHC versions #

Tested with GHC `7.8.4`, `7.10.3`, `8.0.2`, `8.2.2`, `8.4.1`.

# Installation #

Build with either `cabal` or `stack`.

# Usage #

In order to generate tags for all Haskell files under current directory, issue

```
  fast-tags -R .
```

See the `tools` directory for editor and source control integration.

[badge-travis]: https://travis-ci.org/elaforge/fast-tags.svg?branch=master
