# fast-tags - fast and robust tag generator for Haskell #

[![Build Status](https://github.com/elaforge/fast-tags/workflows/ci/badge.svg)](https://github.com/elaforge/fast-tags/actions?query=workflow%3Aci)

# Supported GHC versions #

Tested with GHC `7.10`, `8.0`, `8.2`, `8.4`, `8.4`, `8.6`, `8.8`, `8.10`, `9.0`.

# Installation #

Build with either `cabal` or `stack`.

# Usage #

In order to generate tags for all Haskell files under current directory, issue

```
  fast-tags -R .
```

You can also generate tags that span accross directories or projects:
```
fast-tags -R . path/to/other/project
```

See the `tools` directory for editor and source control integration.

[badge-travis]: https://travis-ci.org/elaforge/fast-tags.svg?branch=master
