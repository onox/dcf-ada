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

 * Reformatted code to Ada default style guide

 * Removed obsolescent features and implementation-defined extensions

 * Removed features prohibited by ISO standard

 * Removed lots of duplicated code and simplified API

 * All packages except one that uses `Ada.Calendar` are preelaborated

## Usage

Before the archived files in a document container file can be queried
or extracted, the archive first needs to be opened and loaded:

```ada
Archive_Stream : aliased DCF.Streams.File_Zipstream := DCF.Streams.Open (Archive_Name);
Info : DCF.Zip.Zip_Info;
```

Then load the archive by calling `DCF.Zip.Load (Info, Archive_Stream)`.
While the `Info` and `Archive_Stream` objects are in scope, the files
can be visited in order to extract them.

### Visiting archived files in a document container file

Files can be extracted by visiting over all the archived files in the
archive and then extracting each of them:

```ada
procedure Visit_File (File : DCF.Zip.Archived_File) is
begin
   Ada.Text_IO.Put_Line ("Visiting " & File.Name);
end Visit_File;

package Visit_All_Files is new DCF.Zip.Traverse (Visit_File);
```

Call `Visit_All_Files (Info)` to visit all files. To extract a single
file, use `Traverse_One_File` instead.

### Querying an archived file

A `File` object of type `Archived_File` can be queried:

 * `Name` returns the name (path) of the file

 * `Compressed_Size` gives the compressed size

 * `Uncompressed_Size` gives the uncompressed size

 * `Date_Time` returns the modification date and time

 * `Compressed` returns True if compressed with the "deflate" algorithm
   and False if stored uncompressed

 * `Encrypted` returns True if the file is encrypted and False otherwise.
   Note that encryption is prohibited and not supported, thus these files
   cannot be decrypted and extracted

 * `CRC_32` returns the CRC code. This number can be printed with
   `DCF.Zip.CRC.Image`

### Extracting an archived file

While visiting an archived file, the file can be extracted using a stream
object:

```ada
type File_Stream_Writer
  (File : access DCF.Streams.File_Zipstream)
is new Ada.Streams.Root_Stream_Type with record
  Index : DCF.Streams.Zs_Index_Type := DCF.Streams.Zs_Index_Type'First;
end record;
```

In the implementation of `File_Stream_Writer.Write`, set the index of
`This.File.all` to `This.Index` and then write `Item` to `This.File.all`.
After writing, make sure to update `This.Index` because the `Write` procedure
may be called multiple times for large files. See [unzipdcf][url-unzipdcf]
tool for the exact implementation.

Create the file and then write to it using the `File_Stream_Writer` object:

```ada
declare
   File_Stream : aliased DCF.Streams.File_Zipstream
     := DCF.Streams.Create (File.Name);
   Stream_Writer : File_Stream_Writer (File_Stream'Access);
begin
   DCF.Unzip.Streams.Extract
     (Destination      => Stream_Writer,
      Archive_Info     => Info,
      File             => File,
      Verify_Integrity => False);
end;
```

Note that you should verify that `File.Name` is a valid path and sanitize
it before attempting to create and write to the file.

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
  [url-unzipdcf]: /tools/unzipdcf.adb
