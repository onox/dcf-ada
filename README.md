[![License](https://img.shields.io/github/license/onox/dcf-ada.svg?color=blue)](https://github.com/onox/dcf-ada/blob/master/LICENSE)
[![Build status](https://img.shields.io/shippable/5cd73c52bbceea00079c64f0/master.svg)](https://app.shippable.com/github/onox/dcf-ada)
[![GitHub release](https://img.shields.io/github/release/onox/dcf-ada.svg)](https://github.com/onox/dcf-ada/releases/latest)
[![IRC](https://img.shields.io/badge/IRC-%23ada%20on%20freenode-orange.svg)](https://webchat.freenode.net/?channels=ada)

# dcf-ada

An Ada 2012 library for document container files, a Zip-based archive format
standardized in [ISO/IEC 21320-1:2015][url-iso-21320]. Document container
files are Zip files with several restrictions:

 * Only "store" (uncompressed) and "deflate" compression methods are allowed

 * Archives may not be encrypted or contain digital signatures

 * Archives may not span multiple volumes or be segmented

This library is based on the [Zip-Ada][url-zip-ada] library, with extensive
modifications:

 * Binary and Windows-specific files have been removed with [The BFG Repo Cleaner][url-bfg]

 * UNIX line endings

 * Reformatted code to Ada default style guide

 * Removed obsolescent features and implementation-defined extensions

 * Removed prohibited features

## Dependencies

In order to build the library, you need to have:

 * An Ada 2012 compiler

 * GPRBuild and `make`

## Installing dependencies on Ubuntu 18.04 LTS

Install the dependencies using apt:

```sh
$ sudo apt install gnat-7 gprbuild make
```

## Installation

A Makefile is provided to build the source code and tools. Use `make` to build
the source code:

```
$ make
```

You can override CFLAGS if desired. After having compiled the source code,
the library can be installed by executing:

```
$ make PREFIX=/usr install
```

Change `PREFIX` to the preferred destination folder, for example `~/.local`.
Import `dcf_ada` in your \*.gpr project file:

```ada
with "dcf_ada";
```

## Tools

Some tools to compress or decompress document container files can be build with:

```sh
$ make tools
```

## Contributing

Read the [contributing guidelines][url-contributing] if you want to add
a bugfix or an improvement.

## License

This library is distributed under the terms of the [MIT License][url-mit].

  [url-bfg]: https://rtyley.github.io/bfg-repo-cleaner
  [url-contributing]: /CONTRIBUTING.md
  [url-iso-21320]: https://www.iso.org/standard/60101.html
  [url-mit]: https://opensource.org/licenses/mit
  [url-zip-ada]: https://unzip-ada.sourceforge.net
